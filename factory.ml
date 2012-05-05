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
open Sexp
open Datastructures

type factory_t = Sexp.t -> (Sexp.t, Sexp.t) Status.t

let mutex = Mutex.create ()
let classes = ref StringMap.empty

let register_class name factory =
  Util.with_mutex0 mutex
    (fun () ->
      if StringMap.mem name !classes
      then (Log.error "Duplicate node class name" [Str name];
	    exit 1)
      else (Log.info "Registered node class" [Str name];
	    classes := StringMap.add name factory !classes))

let all_class_names () =
  Datastructures.string_map_keys !classes

let lookup_class name =
  try Some (StringMap.find name !classes)
  with Not_found -> None

let factory_handler n sexp =
  match Message.message_of_sexp sexp with
  | Message.Create (Str classname, arg, Str reply_sink, Str reply_name) ->
      let reply =
	match lookup_class classname with
	| Some factory ->
	    (match factory arg with
	    | Status.Ok info ->
		Log.info "Node create ok"
		  [Str classname; arg; Str reply_sink; Str reply_name; info];
		Message.create_ok info
	    | Status.Problem explanation ->
		Log.info "Node create failed"
		  [Str classname; arg; Str reply_sink; Str reply_name; explanation];
		Message.create_failed (Arr [Str "constructor"; explanation]))
	| None ->
	    Log.warn "Node class not found" [Str classname];
	    Message.create_failed (Arr [Str "factory"; Str "class-not-found"])
      in
      Node.post_ignore' reply_sink (Str reply_name) reply (Str "")
  | m ->
      Util.message_not_understood "factory" m

let init () =
  Node.bind_ignore (Node.name_of_string "factory", Node.make "factory" factory_handler)
