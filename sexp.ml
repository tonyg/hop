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

(* SPKI SEXP *)

exception Syntax_error of string

type display_hint_t = {hint : t; body : t}
and t =
  | Str of string
  | Hint of display_hint_t
  | Arr of t list

let _output_str ch s =
  output_string ch (string_of_int (String.length s));
  output_char ch ':';
  output_string ch s

let rec output_sexp ch x =
  match x with
  | Str s ->
      _output_str ch s
  | Hint {hint = h; body = b} ->
      output_char ch '[';
      output_sexp ch h;
      output_char ch ']';
      output_sexp ch b
  | Arr xs ->
      output_char ch '(';
      List.iter (output_sexp ch) xs;
      output_char ch ')'

let rec stream_of_sexp x =
  Stringstream.make (fun () ->
    match x with
    | Str s ->
	Some (string_of_int (String.length s) ^ ":", false, Stringstream.const s)
    | Hint {hint = h; body = b} ->
	Some ("[", false,
	      Stringstream.seq (stream_of_sexp h)
		(Stringstream.seq (Stringstream.const "]")
		   (stream_of_sexp b)))
    | Arr xs ->
	Some ("(", false,
	      Stringstream.seq (Stringstream.map stream_of_sexp xs) (Stringstream.const ")")))

let output_char_escaped ch c =
  if c = '\"'
  then output_string ch "\\\""
  else output_char ch c

let rec output_sexp_human ch x =
  match x with
  | Str s ->
      output_char ch '\"';
      String.iter (output_char_escaped ch) s;
      output_char ch '\"'
  | Hint {hint = h; body = b} ->
      output_char ch '[';
      output_sexp_human ch h;
      output_char ch ']';
      output_sexp_human ch b
  | Arr xs ->
      output_char ch '(';
      (match xs with
      | [] -> ()
      | [x] -> output_sexp_human ch x
      | (x :: xs') ->
	  output_sexp_human ch x;
	  List.iter (fun x -> output_char ch ' '; output_sexp_human ch x) xs');
      output_char ch ')'

let output_sexp_and_flush ch x =
  output_sexp ch x;
  flush ch

let char_numeric c = '0' <= c && c <= '9'
let char_whitespace c = c <= ' '

let digit_val c = (int_of_char c) - (int_of_char '0')

let input_bytes ch count =
  let buf = String.create count in (* mutable strings?!?! *)
  really_input ch buf 0 count;
  buf

let syntax_error explanation = raise (Syntax_error explanation)

let input_sexp_outer input_char input_bytes =
  let rec input_simple_string len =
    match input_char () with
    | ':' -> Str (input_bytes len)
    | b when char_numeric b -> input_simple_string (len * 10 + digit_val b)
    | _ -> syntax_error "Bad simple-string length character"
  in
  let rec input_sexp_list () =
    let rec collect acc =
      match input_sexp_inner () with
      | None -> Arr (List.rev acc)
      | Some v -> collect (v :: acc)
    in collect []
  and input_sexp_inner () =
    match input_char () with
    | '(' -> Some (input_sexp_list ())
    | ')' -> None
    | '[' ->
	let hint = input_simple_string 0 in
	(match input_char () with
	| ']' -> Some (Hint {hint = hint; body = input_simple_string 0})
	| _ -> syntax_error "Missing close-bracket in display hint")
    | b when char_numeric b ->
	Some (input_simple_string (digit_val b))
    | b when char_whitespace b ->
	(* Convenience for testing *)
	input_sexp_inner ()
    | _ ->
	syntax_error "Bad SEXP input character"
  in
  match input_sexp_inner () with
  | None -> syntax_error "Unexpected end of list"
  | Some v -> v

let input_sexp ch = input_sexp_outer (fun () -> input_char ch) (input_bytes ch)
let parse b = input_sexp_outer (fun () -> Ibuffer.next_char b) (Ibuffer.next_chars b)
let sexp_of_string s = parse (Ibuffer.of_string s)
let string_of_sexp x = Stringstream.to_string (stream_of_sexp x)
