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

open Printf
open Datastructures
open Status

type handle_message_t = t -> Sexp.t -> unit
and t = {
    mutable names: StringSet.t;
    class_name: string;
    handle_message: handle_message_t
  }

let mutex = Mutex.create ()
let directory = ref StringMap.empty

let local_container_name () = "server"

let make class_name handler = {
  names = StringSet.empty;
  class_name = class_name;
  handle_message = handler
}

let lookup name =
  try Some (StringMap.find name !directory)
  with Not_found -> None

let all_node_names () = string_map_keys !directory

(* Approximate because it doesn't lock or run in a transaction *)
let approx_exists name = StringMap.mem name !directory

let bind (filter, node) =
  if filter = ""
  then (Log.warn "Binding to empty name forbidden" []; false)
  else
    Util.with_mutex0 mutex
      (fun () ->
	if StringMap.mem filter !directory
	then false
	else (directory := StringMap.add filter node !directory;
	      node.names <- StringSet.add filter node.names;
	      Log.info "Node bound" [Sexp.Str filter; Sexp.Str node.class_name];
	      true))

(* For use in factory constructor functions, hence the odd return type and values *)
let make_named class_name node_name handler =
  let node = make class_name handler in
  if bind (node_name, node) then Ok node else Problem (Sexp.Str "bind-failed")

(* For use in factory constructor functions, hence the odd return type and values *)
let make_idempotent_named class_name node_name handler =
  match lookup node_name with
  | Some n ->
      if n.class_name = class_name
      then Ok n
      else Problem (Sexp.Str "class-mismatch")
  | None ->
      let node = make class_name handler in
      if bind (node_name, node) then Ok node else Problem (Sexp.Str "bind-failed")

let unbind name =
  Util.with_mutex0 mutex
    (fun () ->
      match lookup name with
      | Some n ->
	  Log.info "Node unbound" [Sexp.Str name; Sexp.Str n.class_name];
	  n.names <- StringSet.remove name n.names;
	  directory := StringMap.remove name !directory;
	  true
      | None ->
	  false)

let unbind_all n =
  StringSet.iter (fun name -> ignore (unbind name)) n.names;
  n.names <- StringSet.empty

let send name body =
  match lookup name with
  | Some n ->
      (try n.handle_message n body
      with e ->
	Log.warn "Node message handler raised exception"
	  [Sexp.Str name;
	   Sexp.Str (Printexc.to_string e)]);
      true
  | None -> false

let post name label body token =
  send name (Message.post (label, body, token))

let bind_ignore (filter, node) =
  if bind (filter, node)
  then ()
  else Log.warn "Duplicate binding" [Sexp.Str filter]

let send_ignore name body =
  if send name body || name = ""
  then ()
  else Log.warn "send to missing node" [Sexp.Str name; body]

let post_ignore name label body token =
  send_ignore name (Message.post (label, body, token))
