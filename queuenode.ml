(* Copyright 2012 Tony Garnock-Jones <tonygarnockjones@gmail.com>. *)

(* This file is part of Ocamlmsg. *)

(* Ocamlmsg is free software: you can redistribute it and/or modify it *)
(* under the terms of the GNU General Public License as published by the *)
(* Free Software Foundation, either version 3 of the License, or (at your *)
(* option) any later version. *)

(* Ocamlmsg is distributed in the hope that it will be useful, but *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* General Public License for more details. *)

(* You should have received a copy of the GNU General Public License *)
(* along with Ocamlmsg.  If not, see <http://www.gnu.org/licenses/>. *)

open Sexp
open Status

type t = {
    name: string;
    subscriptions: Subscription.set_t;
    ch: Message.t Squeue.t;
    mutable backlog: Sexp.t Fqueue.t;
    mutable waiters: Subscription.t Fqueue.t;
  }

let classname = "queue"

let report info n =
  Log.info (Printf.sprintf "do_burst %d capacity, %d backlog, %d waiters, %d ticks left\n%!"
	      (Squeue.approx_capacity info.ch)
	      (Fqueue.length info.backlog)
	      (Fqueue.length info.waiters)
	      n) []

let rec do_burst info n =
  (* report info n; *)
  if Fqueue.is_empty info.backlog then false
  else
    if Fqueue.is_empty info.waiters then false
    else
      if n = 0 then true (* maybe more work available, but should poll for outside events *)
      else
	let (body, new_backlog) = Fqueue.really_pop_front info.backlog in
	let (sub, new_waiters) = Fqueue.really_pop_front info.waiters in
	info.waiters <- new_waiters;
	if Subscription.send_to_subscription info.name info.subscriptions sub body
	then
	  (info.waiters <- Fqueue.push_back info.waiters sub;
	   info.backlog <- new_backlog;
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
	info.backlog <- Fqueue.push_back info.backlog body;
	loop ()
    | Message.Subscribe (filter, Str sink, name, Str reply_sink, reply_name) ->
	let sub =
	  Subscription.create
	    info.name info.subscriptions filter sink name reply_sink reply_name in
	info.waiters <- Fqueue.push_back info.waiters sub;
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
	backlog = Fqueue.empty;
	waiters = Fqueue.empty
      } in
      ignore (Util.create_thread name None shoveller info);
      let queue_handler n sexp = Squeue.add (Message.message_of_sexp sexp) info.ch in
      replace_ok (Node.make_idempotent_named classname name queue_handler) (Str name)
  | _ ->
      Problem (Str "bad-arg")

let init () =
  Factory.register_class classname queue_factory
