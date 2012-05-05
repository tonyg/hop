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
and name = {
    label: string;
    mutable binding: t option
  }

module NameTable = Weak.Make(struct
  type t = name
  let equal a b = (a.label = b.label)
  let hash a = Hashtbl.hash a.label
end)
module NameSet = Set.Make(struct
  type t = name
  let compare a b = String.compare a.label b.label
end)

let mutex = Mutex.create ()
let name_table = NameTable.create 100
let directory = ref NameSet.empty

let name_of_string str =
  Util.with_mutex0 mutex (fun () ->
    let template = {label = str; binding = None} in
    NameTable.merge name_table template)

let local_container_name () = "server"

let make class_name handler = {
  names = StringSet.empty;
  class_name = class_name;
  handle_message = handler
}

let lookup name = name.binding

let all_node_names () = NameSet.elements !directory
let all_node_name_strings () = List.map (fun x -> x.label) (all_node_names ())

(* Approximate because it doesn't lock or run in a transaction *)
let approx_exists name =
  match name.binding with
  | Some _ -> true
  | None -> false

let bind (filter, node) =
  if filter.label = ""
  then (Log.warn "Binding to empty name forbidden" []; false)
  else
    Util.with_mutex0 mutex (fun () ->
      filter.binding <- Some node;
      directory := NameSet.add filter !directory;
      node.names <- StringSet.add filter.label node.names;
      Log.info "Node bound" [Sexp.Str filter.label; Sexp.Str node.class_name];
      true)

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
  Util.with_mutex0 mutex (fun () ->
    match lookup name with
    | Some n ->
	Log.info "Node unbound" [Sexp.Str name.label; Sexp.Str n.class_name];
	n.names <- StringSet.remove name.label n.names;
	name.binding <- None;
	directory := NameSet.remove name !directory;
	true
    | None ->
	false)

let unbind_all n =
  StringSet.iter (fun name -> ignore (unbind (name_of_string name))) n.names;
  n.names <- StringSet.empty

let send name body =
  match lookup name with
  | Some n ->
      (try n.handle_message n body
      with e ->
	Log.warn "Node message handler raised exception"
	  [Sexp.Str name.label;
	   Sexp.Str (Printexc.to_string e)]);
      true
  | None -> false

let send' str body = send (name_of_string str) body

let post name label body token =
  send name (Message.post (label, body, token))

let post' str label body token = post (name_of_string str) label body token

let bind_ignore (filter, node) =
  if bind (filter, node)
  then ()
  else Log.warn "Duplicate binding" [Sexp.Str filter.label]

let send_ignore name body =
  if send name body || name.label = ""
  then ()
  else Log.warn "send to missing node" [Sexp.Str name.label; body]

let send_ignore' str body = send_ignore (name_of_string str) body

let post_ignore name label body token =
  send_ignore name (Message.post (label, body, token))

let post_ignore' str label body token = post_ignore (name_of_string str) label body token
