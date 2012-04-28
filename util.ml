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
open Printf

let message_not_understood context m =
  Log.warn "Message not understood" [Str context; Message.sexp_of_message m]

let create_thread name cleanup main initarg =
  let guarded_main initarg =
    try
      main initarg
    with e ->
      Log.warn "Thread died with exception" [Str name; Str (Printexc.to_string e)];
      (match cleanup with
      | Some cleaner -> cleaner ()
      | None -> ())
  in
  Thread.create guarded_main initarg

let with_mutex m f arg =
  Mutex.lock m;
  try
    let result = f arg in
    Mutex.unlock m;
    result
  with e ->
    Mutex.unlock m;
    raise e

let with_mutex0 m thunk = with_mutex m thunk ()

let starts_with s1 s2 =
  try Str.first_chars s1 (String.length s2) = s2 with _ -> false

let ends_with s1 s2 =
  try Str.last_chars s1 (String.length s2) = s2 with _ -> false
