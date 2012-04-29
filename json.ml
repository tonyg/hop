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

type t =
  | Num of float
  | Str of string
  | Arr of t list
  | Rec of (string * t) list
  | Flg of bool
  | Nil

let escape_re = Str.regexp "\""
let escape_char s =
  match s with
  | "\"" -> "\\\""
  | _ -> failwith ("Unexpected JSON char to escape: " ^ s)
let escape s = Str.global_substitute escape_re escape_char s

let str_to_string s =
  "\"" ^ escape s ^ "\""

let rec to_string j =
  match j with
  | Num f ->
      if float_of_int (int_of_float f) = f
      then string_of_int (int_of_float f)
      else string_of_float f
  | Str s ->
      str_to_string s
  | Arr js ->
      "[" ^ String.concat "," (List.map to_string js) ^ "]"
  | Rec kvs ->
      "{" ^ String.concat "," (List.map kv_to_string kvs) ^ "}"
  | Flg b ->
      if b then "true" else "false"
  | Nil ->
      "null"

and kv_to_string (k, v) =
  str_to_string k ^ ":" ^ to_string v

let resp code reason j =
  Httpd.resp_generic code reason
    [Httpd.content_type_header_name, "application/json"]
    (Httpd.Fixed (to_string j))
let resp_ok j = resp 200 "OK" j
