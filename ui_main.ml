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
open Hof

let dispatch_table = ref []

let longest_prefix_first (p1, _) (p2, _) =
  String.length p2 - String.length p1

let register_dispatcher (prefix, handler) =
  dispatch_table := List.sort longest_prefix_first ((prefix, handler) :: !dispatch_table)

let handle_dynamic_req id r =
  let rec search_table table =
    match table with
    | [] ->
	Httpd.http_error_html 404 "Not found"
	  [Html.tag "p" [] [Html.text ("No route for URL path "^r.Httpd.path)]]
    | (prefix, handler) :: rest ->
	if Util.starts_with r.Httpd.path prefix
	then handler id r
	else search_table rest
  in
  search_table !dispatch_table

let handle_req id r =
  if Util.starts_with r.Httpd.path "/_"
  then handle_dynamic_req id r
  else
    match r.Httpd.verb with
    | "GET" | "HEAD" -> Httpd_file.resp_file (Filename.concat "./web" r.Httpd.path)
    | _ -> Httpd.http_error_html 400 ("Unsupported HTTP method "^r.Httpd.verb) []

let cleanup_req id () =
  match Node.lookup id with
  | Some n -> Node.unbind_all n
  | None -> ()

let start (s, peername) =
  let id = "http-" ^ Uuid.create () in
  Util.create_thread (Connections.endpoint_name peername ^ " HTTP service")
    (Some (cleanup_req id))
    (Httpd.main (handle_req id))
    (s, peername)

let boot_time = Unix.time ()
let api_server_stats id r =
  Json.resp_ok [] (Json.Rec
		     ["connection_count", Json.Num (float_of_int !Connections.connection_count);
		      "boot_time", Json.Num boot_time;
		      "uptime", Json.Num (Unix.time () -. boot_time);
		      "classes", Json.Arr (List.map Json.str (Factory.all_class_names ()))])
  |> Httpd.add_date_header

let init () =
  register_dispatcher ("/_/server_stats", api_server_stats);
  ignore (Util.create_thread "HTTP listener" None (Net.start_net "HTTP" 5678) start)
