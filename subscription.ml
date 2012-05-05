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

open Datastructures

type t = {
    mutable live: bool;
    uuid: Uuid.t;
    filter: Sexp.t;
    sink: string;
    name: Sexp.t
  }

type set_t = t StringMap.t ref

let new_set () = ref StringMap.empty

let count subs = StringMap.cardinal !subs

let create source subs filter sink name reply_sink reply_name =
  let uuid = Uuid.create () in
  let sub = {
    live = true;
    uuid = uuid;
    filter = filter;
    sink = sink;
    name = name
  } in
  subs := StringMap.add uuid sub !subs;
  Meta.announce_subscription source filter sink name true;
  Node.post_ignore reply_sink reply_name (Message.subscribe_ok (Sexp.Str uuid)) (Sexp.Str "");
  sub

let delete source subs uuid =
  try
    let sub = StringMap.find uuid !subs in
    sub.live <- false;
    subs := StringMap.remove uuid !subs;
    Meta.announce_subscription source sub.filter sub.sink sub.name false;
    Some sub
  with Not_found ->
    None

let lookup subs uuid =
  try Some (StringMap.find uuid !subs)
  with Not_found -> None

let send_to_subscription' sub body delete_action =
  if not sub.live
  then false
  else
    if Node.post sub.sink sub.name body (Sexp.Str sub.uuid)
    then true
    else (delete_action sub.uuid; false)

let send_to_subscription source subs sub body =
  send_to_subscription' sub body (fun (uuid) -> delete source subs uuid)
