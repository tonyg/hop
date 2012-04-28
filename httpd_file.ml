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

let visible_path_component s =
  match s with
  | "" -> false
  | "." -> false
  | ".." -> false
  | _ -> if String.get s 0 = '.' then false else true

let sanitize_path_re = Str.regexp "/"
let sanitize_path p =
  String.concat "/" (List.filter visible_path_component (Str.split sanitize_path_re p))

let extension_map ext =
  match String.lowercase ext with
  | ".txt" -> Httpd.text_content_type
  | ".html" | ".htm" -> Httpd.html_content_type
  | ".bin" -> "application/octet-stream"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | ".png" -> "image/png"
  | ".css" -> "text/css"
  | ".js" -> "text/javascript"
  | ".json" -> "application/json"
  | _ -> "application/octet-stream"

let analyze_path p =
  let p = sanitize_path p in
  let chopped_p = (try Filename.chop_extension p with _ -> p) in
  let ext = String.sub p (String.length chopped_p) (String.length p - String.length chopped_p) in
  let p = if p = "" then "." else p in
  (p, extension_map ext)

let read_and_close_file handle =
  let buflen = 4096 in
  let buffer = String.make buflen '\000' in
  fun () ->
    let count =
      (try
	input handle buffer 0 buflen
      with e -> (close_in handle; raise e))
    in
    if count > 0
    then Some (String.sub buffer 0 count)
    else (close_in handle;
	  None)

let rec read_dir dirhandle =
  try
    let n = Unix.readdir dirhandle in
    n :: read_dir dirhandle
  with End_of_file ->
    []

let render_directory_listing path =
  let dir = Unix.opendir path in
  let entries = List.filter visible_path_component (read_dir dir) in
  Unix.closedir dir;
  Html.html_document path []
    [Html.tag "h1" [] [Html.text "Directory listing for "; Html.tag "tt" [] [Html.text path]];
     Html.tag "ul" []
       (List.map
	  (fun e -> Html.tag "li" [] [Html.tag "a" ["href", e] [Html.text e]])
	  entries);
     Html.unclosed_tag "hr" [] []]

let resp_raw_file mime_type path =
  Httpd.resp_generic_ok
    [Httpd.content_type_header_name, mime_type]
    (Httpd.Variable (Stringstream.from_iter (read_and_close_file (open_in_bin path))))

let resp_file raw_path =
  let (path, mime_type) = analyze_path raw_path in
  (try
    (if Sys.is_directory path
    then
      let maybe_index_html = Filename.concat path "index.html" in
      if Sys.file_exists maybe_index_html && not (Sys.is_directory maybe_index_html)
      then resp_raw_file Httpd.html_content_type maybe_index_html
      else
	if path = "." || Str.last_chars raw_path 1 = "/"
	then Httpd.resp_html_doc_ok [] (render_directory_listing path)
	else Httpd.resp_redirect_permanent ("/"^path^"/")
    else if Sys.file_exists path
    then
      resp_raw_file mime_type path
    else
      Httpd.http_error_html 404 "Not found" [])
  with Sys_error message ->
    Httpd.http_error_html 403 "Forbidden" [Html.text message])
