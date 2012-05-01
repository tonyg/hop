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
      | Some cleaner -> cleaner e
      | None -> ())
  in
  Thread.create guarded_main initarg

let daemon_thread_died name nested_cleaner e =
  (match nested_cleaner with
  | Some c -> c e
  | None -> ());
  Server_control.shutdown_now [Sexp.Str "Daemon thread exited"; Sexp.Str name]

let create_daemon_thread name cleanup main initarg =
  create_thread name (Some (daemon_thread_died name cleanup)) main initarg

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

let strip s =
  let len = String.length s in
  let ws i = Char.code (String.get s i) <= 32 in
  let rec left index = if index < len && ws index then left (index + 1) else index in
  let rec right index = if index >= 0 && ws index then right (index - 1) else index in
  let l = left 0 in
  let r = 1 + right (len - 1) in
  if r <= l then "" else String.sub s l (r - l)

let strsub replacement_fn s =
  let len = String.length s in
  let finish_span low high acc = String.sub s low (high - low) :: acc in
  let finish acc = String.concat "" (List.rev acc) in
  let rec outer_loop acc low =
    let rec inner_loop high =
      if high = len
      then finish (finish_span low high acc)
      else
	match replacement_fn (String.get s high) with
	| Some handler ->
	    let (replacement, new_low) = handler (s, high) in
	    outer_loop (replacement :: finish_span low high acc) new_low
	| None ->
	    inner_loop (high + 1)
    in inner_loop low
  in outer_loop [] 0

let unhex_char c =
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> Char.code c - Char.code '0'
  | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let unhex s =
  let len = String.length s in
  let rec loop index acc =
    if index = len
    then acc
    else loop (index + 1) (acc * 16 + unhex_char (String.get s index))
  in
  loop 0 0
