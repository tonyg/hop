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

type html_content =
  | Tag of string * (string * string) list * html_content list * bool
  | Text of string

type html_document = {
    html_title: string;
    html_headers: html_content list;
    html_body: html_content list
  }

let html_document title headers body =
  {html_title = title; html_headers = headers; html_body = body}

let tag label attrs contents = Tag (label, attrs, contents, false)
let unclosed_tag label attrs contents = Tag (label, attrs, contents, true)
let text str = Text str

let tag_of_document doc =
  tag "html" []
    [tag "head" []
       ([unclosed_tag "meta" ["charset", "utf-8"] [];
	 tag "title" [] [text doc.html_title]]
	@ doc.html_headers);
     tag "body" [] doc.html_body]

let escape_html_char c =
  match c with
  | '&' -> Some (fun (s, pos) -> ("&amp;", pos + 1))
  | '<' -> Some (fun (s, pos) -> ("&lt;", pos + 1))
  | '>' -> Some (fun (s, pos) -> ("&gt;", pos + 1))
  | _ -> None
let html_escape s = Util.strsub escape_html_char s

let escape_html_attribute_char c =
  match c with
  | '\'' -> Some (fun (s, pos) -> ("&apos;", pos + 1))
  | '\"' -> Some (fun (s, pos) -> ("&quot;", pos + 1))
  | _ -> None
let html_attribute_escape s = Util.strsub escape_html_attribute_char s

let string_of_html_attribute (k, v) =
  k ^ "=\"" ^ html_attribute_escape v ^ "\""

let string_of_html_attributes attrs =
  String.concat " " (List.map string_of_html_attribute attrs)

let html_open_tag_string label attrs =
  match attrs with
  | [] ->
      "<" ^ label ^ ">"
  | _ ->
      "<" ^ label ^ " " ^ string_of_html_attributes attrs ^ ">"

let rec string_of_html_contents cs = String.concat "" (List.map string_of_html cs)
and string_of_html c =
  match c with
  | Tag (label, attrs, [], true) ->
      html_open_tag_string label attrs
  | Tag (label, attrs, contents, _) ->
      html_open_tag_string label attrs ^ string_of_html_contents contents ^ "</" ^ label ^ ">"
  | Text str ->
      html_escape str

let rec stream_of_html_contents cs = Stringstream.map stream_of_html cs
and stream_of_html c =
  Stringstream.make (fun () ->
    match c with
    | Tag (label, attrs, [], true) ->
	Some (html_open_tag_string label attrs, false, Stringstream.empty)
    | Tag (label, attrs, contents, _) ->
	Some (html_open_tag_string label attrs, false,
	      Stringstream.seq
		(stream_of_html_contents contents) (Stringstream.const ("</"^label^">")))
    | Text str ->
	Some (str, false, Stringstream.empty))

let stream_of_html_doc d =
  Stringstream.seq (Stringstream.const "<!DOCTYPE html>") (stream_of_html (tag_of_document d))
