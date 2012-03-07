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

type t = string

let create () =
  (* 128 bits *)
  let w1 = Random.bits () in
  let w2 = Random.bits () in
  let w3 = Random.bits () in
  let w4 = Random.bits () in
  let bb = Random.int 256 in
  Printf.sprintf "%08x%08x%08x%08x%02x" w1 w2 w3 w4 bb

let init () =
  Random.self_init ()
