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

open Html

let handle_dynamic_req r =
  Httpd.http_error_html 500 "Not yet implemented" []

let handle_req r =
  if Util.starts_with r.Httpd.path "/_"
  then handle_dynamic_req r
  else
    match r.Httpd.verb with
    | "GET" -> Httpd_file.resp_file (Filename.concat "./web" r.Httpd.path)
    | _ -> Httpd.http_error_html 400 ("Unsupported HTTP method "^r.Httpd.verb) []

let start (s, peername) =
  Util.create_thread (Connections.endpoint_name peername ^ " HTTP service")
    None
    (Httpd.main handle_req)
    (s, peername)

let init () =
  ignore (Util.create_thread "HTTP listener" None (Net.start_net "HTTP" 5678) start)
