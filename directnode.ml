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
open Datastructures
open Status

type t = {
    name: string;
    subscriptions: Subscription.set_t;
    mtx: Mutex.t;
    mutable routing_table: UuidSet.t StringMap.t;
  }

let classname = "direct"

let unsubscribe info uuid =
  Util.with_mutex0 info.mtx
    (fun () ->
      match Subscription.delete info.name info.subscriptions uuid with
      | Some sub ->
	  (match sub.Subscription.filter with
	  | Str binding_key ->
	      (try
		let old_set = StringMap.find binding_key info.routing_table in
		let new_set = UuidSet.remove sub.Subscription.uuid old_set in
		if UuidSet.is_empty new_set
		then info.routing_table <- StringMap.remove binding_key info.routing_table
		else info.routing_table <- StringMap.add binding_key new_set info.routing_table
	      with Not_found ->
		())
	  | _ -> ())
      | None -> ())

let route_message info n sexp =
  match Message.message_of_sexp sexp with
  | Message.Post (Str name, body, token) ->
      let routing_snapshot = info.routing_table in
      let matching = (try StringMap.find name routing_snapshot with Not_found -> UuidSet.empty) in
      UuidSet.iter
	(fun (uuid) ->
	  match Subscription.lookup info.subscriptions uuid with
	  | Some sub ->
	      ignore (Subscription.send_to_subscription' sub body (unsubscribe info))
	  | None ->
	      ())
	matching
  | Message.Subscribe (Str binding_key as filter, Str sink, name, Str reply_sink, reply_name) ->
      Util.with_mutex0 info.mtx
	(fun () ->
	  let sub =
	    Subscription.create
	      info.name info.subscriptions filter sink name reply_sink reply_name in
	  let old_set =
	    (try StringMap.find binding_key info.routing_table with Not_found -> UuidSet.empty) in
	  let new_set = UuidSet.add sub.Subscription.uuid old_set in
	  info.routing_table <- StringMap.add binding_key new_set info.routing_table)
  | Message.Unsubscribe (Str token) ->
      unsubscribe info token
  | m ->
      Util.message_not_understood classname m

let factory arg =
  match arg with
  | (Arr [Str name]) ->
      let info = {
	name = name;
	subscriptions = Subscription.new_set ();
	mtx = Mutex.create ();
	routing_table = StringMap.empty;
      } in
      replace_ok (Node.make_idempotent_named classname name (route_message info)) (Str name)
  | _ ->
      Problem (Str "bad-arg")

let init () =
  Factory.register_class classname factory
