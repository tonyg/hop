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

let input_bytes count ch =
  let buf = String.create count in (* mutable strings?!?! *)
  really_input ch buf 0 count;
  Str buf

let syntax_error explanation = raise (Syntax_error explanation)

let rec input_simple_string len ch =
  match input_char ch with
  | ':' -> input_bytes len ch
  | b when char_numeric b -> input_simple_string (len * 10 + digit_val b) ch
  | _ -> syntax_error "Bad simple-string length character"

let rec input_sexp_list ch =
  let rec collect acc =
    match input_sexp_inner ch with
    | None -> Arr (List.rev acc)
    | Some v -> collect (v :: acc)
  in collect []

and input_sexp_inner ch =
  match input_char ch with
  | '(' -> Some (input_sexp_list ch)
  | ')' -> None
  | '[' ->
      let hint = input_simple_string 0 ch in
      (match input_char ch with
      | ']' -> Some (Hint {hint = hint; body = input_simple_string 0 ch})
      | _ -> syntax_error "Missing close-bracket in display hint")
  | b when char_numeric b ->
      Some (input_simple_string (digit_val b) ch)
  | b when char_whitespace b ->
      (* Convenience for testing *)
      input_sexp_inner ch
  | _ ->
      syntax_error "Bad SEXP input character"

let input_sexp ch =
  match input_sexp_inner ch with
  | None -> syntax_error "Unexpected end of list"
  | Some v -> v
