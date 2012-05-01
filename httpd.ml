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

open Unix

type version = [`HTTP_1_0 | `HTTP_1_1]
type resp_version = [version | `SAME_AS_REQUEST]
type content = Fixed of string | Variable of Stringstream.t
type completion = Completion_normal | Completion_error

type body = {
    headers: (string * string) list;
    content: content
  }

let empty_content = Fixed ""
let empty_body = {headers = []; content = empty_content}

type req = {
    verb: string;
    path: string;
    query: (string * string option) list;
    req_version: version;
    req_body: body
  }

type resp = {
    resp_version: resp_version;
    status: int;
    reason: string;
    resp_body: body;
    completion_callbacks: (completion -> unit) list
  }

exception HTTPError of (int * string * body)

let html_content_type = "text/html;charset=utf-8"
let text_content_type = "text/plain;charset=utf-8"

let content_type_header_name = "Content-Type"

let html_content_type_header = (content_type_header_name, html_content_type)
let text_content_type_header = (content_type_header_name, text_content_type)

let disable_cache_headers () =
  ["Expires", "Thu, 01 Jan 1981 00:00:00 GMT";
   "Last-Modified", Httpd_date.http_gmtime (Unix.time ());
   "Cache-Control", "no-cache, must-revalidate, max-age=0";
   "Pragma", "no-cache"]

let add_headers headers resp =
  let b = resp.resp_body in
  {resp with resp_body = {b with headers = b.headers @ headers}}

let add_disable_cache_headers resp = add_headers (disable_cache_headers ()) resp

let add_date_header resp = add_headers ["Date", Httpd_date.http_gmtime (Unix.time ())] resp

let add_completion_callback cb resp =
  {resp with completion_callbacks = cb :: resp.completion_callbacks}

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
    resp_body = {headers = headers; content = content};
    completion_callbacks = [] }

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

let escape_url_char c =
  match c with
  | '%' -> Some (fun (s, pos) -> ("%25", pos + 1))
  | ' ' -> Some (fun (s, pos) -> ("%20", pos + 1))
  | _ -> None
let url_escape s = Util.strsub escape_url_char s

let unescape_url_hex_code (s, pos) =
  let len = String.length s in
  if len - pos >= 3
  then
    let v1 = Util.unhex_char (String.get s (pos + 1)) in
    let v2 = Util.unhex_char (String.get s (pos + 2)) in
    if v1 = -1 || v2 = -1
    then http_error_html 400 ("Bad percent escaping: '"^String.sub s pos 3^"'") []
    else (String.make 1 (Char.chr (v1 * 16 + v2)), pos + 3)
  else http_error_html 400 ("Bad percent escaping: '"^String.sub s pos (len - pos)^"'") []

let unescape_url_char c =
  match c with
  | '%' -> Some unescape_url_hex_code
  | _ -> None

let url_unescape s = Util.strsub unescape_url_char s

let render_header cout (k, v) =
  output_string cout k;
  output_string cout ": ";
  output_string cout v;
  output_string cout "\r\n"

let render_chunk cout (chunk, should_flush) =
  (match chunk with
  | "" -> ()
  | _ ->
      output_string cout (Printf.sprintf "%x\r\n" (String.length chunk));
      output_string cout chunk;
      output_string cout "\r\n");
  if should_flush then flush cout else ()

let render_fixed_content cout s headers_only =
  render_header cout ("Content-Length", string_of_int (String.length s));
  output_string cout "\r\n";
  if headers_only then () else output_string cout s

let string_of_content c =
  match c with
  | Fixed s -> s
  | Variable s -> Stringstream.to_string s

let render_content cout v c headers_only =
  match c with
  | Fixed s ->
      render_fixed_content cout s headers_only
  | Variable s ->
      match v with
      | `HTTP_1_0 ->
	  render_fixed_content cout (Stringstream.to_string s) headers_only
      | `HTTP_1_1 ->
	  if headers_only
	  then (output_string cout "\r\n")
	  else (render_header cout ("Transfer-Encoding", "chunked");
		output_string cout "\r\n";
		Stringstream.iter (render_chunk cout) s;
		output_string cout "0\r\n\r\n")

let render_body cout v b headers_only =
  List.iter (render_header cout) b.headers;
  render_content cout v b.content headers_only

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
  render_body cout r.req_version r.req_body false

let render_resp cout req_version req_verb r =
  let resp_version =
    (match r.resp_version with
    | `SAME_AS_REQUEST -> req_version
    | #version as v -> v)
  in
  output_string cout
    (string_of_version resp_version^" "^string_of_int r.status^" "^r.reason^"\r\n");
  render_body cout resp_version r.resp_body (match req_verb with "HEAD" -> true | _ -> false)

let split_query p =
  match Str.bounded_split (Str.regexp "\\?") p 2 with
  | path :: query :: _ -> (path, query)
  | path :: [] -> (path, "")
  | [] -> ("", "")

let parse_urlencoded_binding s =
  match Str.bounded_split (Str.regexp "=") s 2 with
  | k :: v :: _ -> (url_unescape k, Some (url_unescape v))
  | k :: [] -> (url_unescape k, None)
  | [] -> ("", None)

let parse_urlencoded q =
  let pieces = Str.split (Str.regexp "&") q in
  List.map parse_urlencoded_binding pieces

let find_header' name hs =
  let lc_name = String.lowercase name in
  let rec search hs =
    match hs with
    | [] -> raise Not_found
    | (k, v) :: hs' ->
	if String.lowercase k = lc_name
	then v
	else search hs'
  in
  search hs

let find_header name hs =
  try Some (find_header' name hs) with Not_found -> None

let find_param name params =
  try Some (List.assoc name params) with Not_found -> None

let input_crlf cin =
  let line = input_line cin in
  let len = String.length line in
  if len > 0 && String.get line (len - 1) = '\r'
  then String.sub line 0 (len - 1)
  else line

let rec parse_headers cin =
  match Str.bounded_split (Str.regexp ":") (input_crlf cin) 2 with
  | [] ->
      []
  | [k; v] ->
      (k, Util.strip v) :: parse_headers cin
  | k :: _ ->
      http_error_html 400 ("Bad header: "^k) []

let parse_chunks cin =
  fun () ->
    let hexlen_str = input_crlf cin in
    let chunk_len = Util.unhex hexlen_str in
    let buffer = String.make chunk_len '\000' in
    really_input cin buffer 0 chunk_len;
    (if input_crlf cin <> "" then http_error_html 400 "Invalid chunk boundary" [] else ());
    if chunk_len = 0 then None else Some (buffer, false)

let parse_body cin =
  let headers = parse_headers cin in
  match find_header "Transfer-Encoding" headers with
  | None | Some "identity" ->
      (match find_header "Content-Length" headers with
      | None ->
	  (* http_error_html 411 "Length required" [] *)
	  {headers = headers; content = empty_content}
      | Some length_str ->
	  let length = int_of_string length_str in
	  let buffer = String.make length '\000' in
	  really_input cin buffer 0 length;
	  {headers = headers; content = Fixed buffer})
  | Some "chunked" ->
      {headers = headers; content = Variable (Stringstream.from_iter (parse_chunks cin))}
  | Some unsupported ->
      http_error_html 400 ("Unsupported Transfer-Encoding: "^unsupported) []

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
      let (path, query) = split_query path in
      let path = url_unescape path in
      let query = parse_urlencoded query in
      { verb = verb; path = path; query = query; req_version = version; req_body = body }
  | _ -> http_error_html 400 "Bad request line" []

let discard_unread_body req =
  match req.req_body.content with
  | Fixed _ -> ()
  | Variable s -> Stringstream.iter (fun v -> ()) s (* force chunks to be read *)

let connection_keepalive req =
  find_header "Connection" req.req_body.headers = Some "keep-alive"

let main handle_req (s, peername) =
  let cin = in_channel_of_descr s in
  let cout = out_channel_of_descr s in
  (try
    (try
      let rec request_loop () =
	let req = parse_req cin 512 in
	let resp = handle_req req in

	let completion_mutex = Mutex.create () in
	let completion = ref None in
	let set_completion v =
	  Util.with_mutex0 completion_mutex (fun () ->
	    match !completion with
	    | None ->
		completion := Some v;
		List.iter (fun cb -> cb v) resp.completion_callbacks
	    | Some _ -> ())
	in

	(* Here we spawn a thread that just watches the socket to see
	if it either becomes active or closes during rendering of the
	response, so that we can make decisions based on this in any
	eventual streaming response generators. In particular, if
	we're implementing some kind of XHR streaming andthe client
	goes away, we want to abandon the streaming as soon as
	possible. *)
	let input_waiter () =
	  try
	    (let (r, w, e) = Unix.select [s] [] [s] (-1.0) in
	    set_completion (if r <> [] then Completion_normal else Completion_error))
	  with _ -> set_completion Completion_error
	in
	ignore (Thread.create input_waiter ());

	(try
	  render_resp cout req.req_version req.verb resp;
	  discard_unread_body req;
	  flush cout;
	  set_completion Completion_normal
	with e ->
	  set_completion Completion_error;
	  raise e);

	if connection_keepalive req then request_loop () else ()
      in
      request_loop ()
    with
    | End_of_file ->
	()
    | HTTPError (code, reason, body) ->
	render_resp cout `HTTP_1_0
	  "GET" (* ugh this should probably be done better *)
	  { resp_version = `HTTP_1_0;
	    status = code;
	    reason = reason;
	    resp_body = body;
	    completion_callbacks = [] })
  with
  | Sys_error message ->
      Log.info "Sys_error in httpd handler" [Sexp.Str message]
  | exn ->
      Log.error "Uncaught exception in httpd handler" [Sexp.Str (Printexc.to_string exn)]);
  (try flush cout with _ -> ());
  close s
