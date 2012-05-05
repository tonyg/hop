(* Copyright 2012 Tony Garnock-Jones <tonygarnockjones@gmail.com>. *)

(* This file is part of Hop. *)

(* Hop is free software: you can redistribute it and/or modify it *)
(* under the terms of the GNU General Public License as published by the *)
(* Free Software Foundation, either version 3 of the License, or (at your *)
(* option) any later version. *)

(* Hop is distributed in the hope that it will be useful, but *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* General Public License for more details. *)

(* You should have received a copy of the GNU General Public License *)
(* along with Hop.  If not, see <http://www.gnu.org/licenses/>. *)

open Sexp
open Status

type t = {
    name: string;
    subscriptions: Subscription.set_t;
    ch: Message.t Squeue.t;
    mutable backlog: Sexp.t Queue.t;
    mutable waiters: Subscription.t Queue.t;
  }

let classname = "queue"

let report info n =
  Log.info (Printf.sprintf "do_burst %d capacity, %d backlog, %d waiters, %d ticks left\n%!"
	      (Squeue.approx_capacity info.ch)
	      (Queue.length info.backlog)
	      (Queue.length info.waiters)
	      n) []

let rec do_burst info n =
  (* report info n; *)
  if Queue.is_empty info.backlog then false
  else
    if Queue.is_empty info.waiters then false
    else
      if n = 0 then true (* maybe more work available, but should poll for outside events *)
      else
	let body = Queue.peek info.backlog in
	let sub = Queue.pop info.waiters in
	if Subscription.send_to_subscription info.name info.subscriptions sub body
	then
	  (Queue.push sub info.waiters;
	   ignore (Queue.pop info.backlog);
	   do_burst info (n - 1))
	else
	  do_burst info n

let rec process_and_wait info =
  if not (do_burst info 1000)
  then Squeue.pop info.ch
  else
    match Squeue.peek info.ch with
    | Some m -> m
    | None -> process_and_wait info

let shoveller info =
  let rec loop () =
    match process_and_wait info with
    | Message.Post (name, body, token) ->
	Queue.push body info.backlog;
	loop ()
    | Message.Subscribe (filter, Str sink, name, Str reply_sink, reply_name) ->
	let sub =
	  Subscription.create
	    info.name info.subscriptions filter sink name reply_sink reply_name in
	Queue.push sub info.waiters;
	loop ()
    | Message.Unsubscribe (Str token) ->
	ignore (Subscription.delete info.name info.subscriptions token);
	loop ()
    | m ->
	Util.message_not_understood "queue" m;
	loop ()
  in loop ()

let queue_factory arg =
  match arg with
  | (Arr [Str name]) ->
      let info = {
	name = name;
	subscriptions = Subscription.new_set ();
	ch = Squeue.create 1000;
	backlog = Queue.create ();
	waiters = Queue.create ()
      } in
      ignore (Util.create_thread name None shoveller info);
      let queue_handler n sexp = Squeue.add (Message.message_of_sexp sexp) info.ch in
      replace_ok (Node.make_idempotent_named classname name queue_handler) (Str name)
  | _ ->
      Problem (Str "bad-arg")

let init () =
  Factory.register_class classname queue_factory
