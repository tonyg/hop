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

open Unix

let rec accept_loop sock connection_start_fn =
  let (s, peername) = accept sock in
  setsockopt s TCP_NODELAY true;
  ignore (connection_start_fn (s, peername));
  accept_loop sock connection_start_fn

let start_net protocol_name port_number connection_start_fn =
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock (ADDR_INET (inet_addr_of_string "0.0.0.0", port_number));
  listen sock 5;
  Log.info "Accepting connections" [Sexp.Str protocol_name; Sexp.Str (string_of_int port_number)];
  accept_loop sock connection_start_fn
