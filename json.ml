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

exception Syntax_error

let str s = Str s

let escape_char c =
  match c with
  | '\"' -> Some (fun (s, pos) -> ("\\\"", pos + 1))
  | _ -> None
let escape s = Util.strsub escape_char s

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

let accumulate_utf8 codepoint (acc, len) =
  (* Of course, at the moment, the codepoint is limited to 16 bits... *)
  if codepoint < 0x80 then
    (Char.chr codepoint :: acc, len + 1)
  else if codepoint < 0x800 then
    (Char.chr (0x80 lor (codepoint land 0x3f)) ::
     Char.chr (0xC0 lor ((codepoint lsr 6) land 0x1f)) ::
     acc, len + 2)
  else if codepoint < 0x10000 then
    (Char.chr (0x80 lor (codepoint land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)) ::
     Char.chr (0xE0 lor ((codepoint lsr 12) land 0xf)) ::
     acc, len + 3)
  else if codepoint < 0x200000 then
    (Char.chr (0x80 lor (codepoint land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 12) land 0x3f)) ::
     Char.chr (0xF0 lor ((codepoint lsr 18) land 0x7)) ::
     acc, len + 4)
  else if codepoint < 0x4000000 then
    (Char.chr (0x80 lor (codepoint land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 12) land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 18) land 0x3f)) ::
     Char.chr (0xF8 lor ((codepoint lsr 24) land 0x3)) ::
     acc, len + 5)
  else
    (Char.chr (0x80 lor (codepoint land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 12) land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 18) land 0x3f)) ::
     Char.chr (0x80 lor ((codepoint lsr 24) land 0x3f)) ::
     Char.chr (0xFC lor ((codepoint lsr 30) land 0x1)) ::
     acc, len + 6)

let string_of_revlist acc len =
  let buf = String.make len ' ' in
  let rec fill cs i =
    match cs with
    | [] -> ()
    | c :: cs' -> (String.set buf i c; fill cs' (i - 1))
  in
  fill acc (len - 1);
  buf

let rec parse_num b (acc, len) =
  match Ibuffer.peek_char b with
  | '+' | '-' | 'e' | 'E' | '.'
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
      as c ->
	Ibuffer.skip_byte b;
	parse_num b (c :: acc, len + 1)
  | _ ->
      Num (float_of_string (string_of_revlist acc len))

let rec parse_str b (acc, len) =
  match Ibuffer.next_char b with
  | '\"' -> Str (string_of_revlist acc len)
  | '\\' ->
      (match Ibuffer.next_char b with
      | 'b' -> parse_str b (Char.chr 8 :: acc, len + 1)
      | 'f' -> parse_str b (Char.chr 12 :: acc, len + 1)
      | 'n' -> parse_str b (Char.chr 10 :: acc, len + 1)
      | 'r' -> parse_str b (Char.chr 13 :: acc, len + 1)
      | 't' -> parse_str b (Char.chr 9 :: acc, len + 1)
      | 'u' -> parse_str b (accumulate_utf8 (Util.unhex (Ibuffer.next_chars b 4)) (acc, len))
      | c -> parse_str b (c :: acc, len + 1))
  | c -> parse_str b (c :: acc, len + 1)

let rec parse_arr b acc =
  Ibuffer.skip_ws b;
  match Ibuffer.peek_char b with
  | ']' -> Ibuffer.skip_byte b; Arr (List.rev acc)
  | ',' -> Ibuffer.skip_byte b; parse_arr b acc
  | _ -> parse_arr b (parse b :: acc)

and parse_rec b acc =
  Ibuffer.skip_ws b;
  match Ibuffer.peek_char b with
  | '}' -> Ibuffer.skip_byte b; Rec (List.rev acc)
  | ',' -> Ibuffer.skip_byte b; parse_rec b acc
  | _ ->
      (match parse b with
      | Str k ->
	  Ibuffer.skip_ws b;
	  (match Ibuffer.next_char b with
	  | ':' -> parse_rec b ((k, parse b) :: acc)
	  | _ -> raise Syntax_error)
      | _ -> raise Syntax_error)

and parse b =
  Ibuffer.skip_ws b;
  match Ibuffer.next_char b with
  | '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
      as c -> parse_num b ([c], 1)
  | '\"' -> parse_str b ([], 0)
  | '[' -> parse_arr b []
  | '{' -> parse_rec b []
  | 't' -> if Ibuffer.next_chars b 3 = "rue" then Flg true else raise Syntax_error
  | 'f' -> if Ibuffer.next_chars b 4 = "alse" then Flg false else raise Syntax_error
  | 'n' -> if Ibuffer.next_chars b 3 = "ull" then Nil else raise Syntax_error
  | _ -> raise Syntax_error

let of_string s = parse (Ibuffer.of_string s)

let resp code reason extra_headers j =
  Httpd.resp_generic code reason
    ((Httpd.content_type_header_name, "application/json") :: extra_headers)
    (Httpd.Fixed (to_string j))
let resp_ok extra_headers j = resp 200 "OK" extra_headers j
