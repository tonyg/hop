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

let mtx = Mutex.create ()
let write_to_log label body =
  Mutex.lock mtx;
  (try
    print_string label;
    print_string ": ";
    output_sexp_human stdout body;
    print_newline ()
  with _ -> ());
  Mutex.unlock mtx

let hook = ref write_to_log

let info message args = (!hook) "info" (Arr (Str message :: args))
let warn message args = (!hook) "warn" (Arr (Str message :: args))
let error message args = (!hook) "error" (Arr (Str message :: args))
