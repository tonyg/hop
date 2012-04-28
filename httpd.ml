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

type version = [`HTTP_1_0 | `HTTP_1_1]
type resp_version = [version | `SAME_AS_REQUEST]
type content = Fixed of string | Variable of Stringstream.t

type body = {
    headers: (string * string) list;
    content: content
  }

let empty_body = {headers = []; content = Fixed ""}

type req = {
    verb: string;
    path: string;
    query: string;
    req_version: version;
    req_body: body
  }

type resp = {
    resp_version: resp_version;
    status: int;
    reason: string;
    resp_body: body
  }

exception HTTPError of (int * string * body)

let html_content_type = "text/html;charset=utf-8"
let text_content_type = "text/plain;charset=utf-8"

let content_type_header_name = "Content-Type"

let html_content_type_header = (content_type_header_name, html_content_type)
let text_content_type_header = (content_type_header_name, text_content_type)

let http_error code reason body = raise (HTTPError (code, reason, body))

let http_error_plain code reason =
  http_error code reason
    {headers = [text_content_type_header]; content = Fixed reason}

let http_error_html_doc code reason doc =
  http_error code reason
    {headers = [html_content_type_header];
     content = Variable (Html.stream_of_html_doc doc)}

let html_error_doc code reason extra_body =
  let code_str = string_of_int code in
  (Html.html_document (code_str^" "^reason) []
     ((Html.tag "h1" [] [Html.text reason]) :: extra_body))

let http_error_html code reason extra_body =
  http_error_html_doc code reason (html_error_doc code reason extra_body)

let resp_generic code reason headers content =
  { resp_version = `SAME_AS_REQUEST;
    status = code;
    reason = reason;
    resp_body = {headers = headers; content = content} }

let resp_generic_ok headers content =
  resp_generic 200 "OK" headers content

let resp_html_doc code reason extra_headers doc =
  resp_generic code reason
    (html_content_type_header :: extra_headers)
    (Variable (Html.stream_of_html_doc doc))

let resp_html_doc_ok extra_headers doc = resp_html_doc 200 "OK" extra_headers doc

let resp_html code reason extra_headers title content =
  resp_html_doc code reason extra_headers (Html.html_document title [] content)

let resp_html_ok extra_headers title content =
  resp_html 200 "OK" extra_headers title content

let resp_plain code reason extra_headers text =
  resp_generic code reason
    (text_content_type_header :: extra_headers)
    (Fixed text)

let resp_plain_ok extra_headers text =
  resp_plain 200 "OK" extra_headers text

let resp_redirect_permanent new_path =
  resp_html_doc 301 "Moved permanently" ["Location", new_path]
    (html_error_doc 301 "Moved permanently"
       [Html.text "The document has moved ";
	Html.tag "a" ["href", new_path] [Html.text "here"];
	Html.text "."])

let url_escape_re = Str.regexp "[% ]"
let escape_url_char s =
  match s with
  | "%" -> "%25"
  | " " -> "%20"
  | _ -> failwith ("Unexpected URL char to escape: " ^ s)
let url_escape s = Str.global_substitute url_escape_re escape_url_char s

let url_unescape_re = Str.regexp "%[0-9a-zA-Z][0-9a-zA-Z]"
let unhex_char c =
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> Char.code c - Char.code '0'
  | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> Char.code c - Char.code 'a'
  | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' -> Char.code c - Char.code 'A'
  | _ -> 0
let unescape_url_char s =
  String.make 1 (Char.chr (unhex_char (String.get s 1) * 16 + unhex_char (String.get s 2)))
let url_unescape s = Str.global_substitute url_unescape_re unescape_url_char s

let render_header cout (k, v) =
  output_string cout k;
  output_string cout ": ";
  output_string cout v;
  output_string cout "\r\n"

let render_chunk cout chunk =
  match chunk with
  | "" -> ()
  | _ ->
      output_string cout (Printf.sprintf "%x\r\n" (String.length chunk));
      output_string cout chunk;
      output_string cout "\r\n"

let render_fixed_content cout s =
  render_header cout ("Content-Length", string_of_int (String.length s));
  output_string cout "\r\n";
  output_string cout s

let render_content cout v c =
  match c with
  | Fixed s ->
      render_fixed_content cout s
  | Variable s ->
      match v with
      | `HTTP_1_0 ->
	  render_fixed_content cout (Stringstream.to_string s)
      | `HTTP_1_1 ->
	  render_header cout ("Transfer-Encoding", "chunked");
	  output_string cout "\r\n";
	  Stringstream.iter (render_chunk cout) s;
	  output_string cout "0\r\n\r\n"

let render_body cout v b =
  List.iter (render_header cout) b.headers;
  render_content cout v b.content

let string_of_version v =
  match v with
  | `HTTP_1_0 -> "HTTP/1.0"
  | `HTTP_1_1 -> "HTTP/1.1"

let version_of_string v =
  match v with
  | "HTTP/1.0" -> `HTTP_1_0
  | "HTTP/1.1" -> `HTTP_1_1
  | _ -> http_error_html 400 "Invalid HTTP version" []

let render_req cout r =
  output_string cout (r.verb^" "^url_escape r.path^" "^string_of_version r.req_version^"\r\n");
  render_body cout r.req_version r.req_body

let render_resp cout req_version r =
  let resp_version =
    (match r.resp_version with
    | `SAME_AS_REQUEST -> req_version
    | #version as v -> v)
  in
  output_string cout
    (string_of_version resp_version^" "^string_of_int r.status^" "^r.reason^"\r\n");
  render_body cout resp_version r.resp_body

let split_query p =
  match Str.bounded_split (Str.regexp "\\?") p 2 with
  | path :: query :: _ -> (path, query)
  | path :: [] -> (path, "")
  | [] -> ("", "")

let parse_body cin = empty_body

let input_crlf cin =
  let line = input_line cin in
  let len = String.length line in
  if len > 0 && String.get line (len - 1) = '\r'
  then String.sub line 0 (len - 1)
  else line

let rec parse_req cin spurious_newline_credit =
  match Str.bounded_split (Str.regexp " ") (input_crlf cin) 3 with
  | [] ->
      (* HTTP spec requires that we ignore leading CRLFs. We choose to do so, up to a point. *)
      if spurious_newline_credit = 0
      then http_error_html 400 "Bad request: too many leading CRLFs" []
      else parse_req cin (spurious_newline_credit - 1)
  | [verb; path; version_str] ->
      let version = version_of_string version_str in
      let body = parse_body cin in
      let path = url_unescape path in
      let (path, query) = split_query path in
      { verb = verb; path = path; query = query; req_version = version; req_body = body }
  | _ -> http_error_html 400 "Bad request line" []

let main handle_req (s, peername) =
  let cin = in_channel_of_descr s in
  let cout = out_channel_of_descr s in
  (try
    (try
      let req = parse_req cin 512 in
      render_resp cout req.req_version (handle_req req)
    with HTTPError (code, reason, body) ->
      render_resp cout `HTTP_1_0
	{ resp_version = `HTTP_1_0; status = code; reason = reason; resp_body = body })
  with _ -> ());
  (try flush cout with _ -> ());
  close s
