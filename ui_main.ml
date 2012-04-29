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

let dispatch_table = ref []

let longest_prefix_first (p1, _) (p2, _) =
  String.length p2 - String.length p1

let register_dispatcher (prefix, handler) =
  dispatch_table := List.sort longest_prefix_first ((prefix, handler) :: !dispatch_table)

let handle_dynamic_req r =
  let rec search_table table =
    match table with
    | [] ->
	Httpd.http_error_html 404 "Not found"
	  [Html.tag "p" [] [Html.text ("No route for URL path "^r.Httpd.path)]]
    | (prefix, handler) :: rest ->
	if Util.starts_with r.Httpd.path prefix
	then handler r
	else search_table rest
  in
  search_table !dispatch_table

let handle_req r =
  if Util.starts_with r.Httpd.path "/_"
  then handle_dynamic_req r
  else
    match r.Httpd.verb with
    | "GET" | "HEAD" -> Httpd_file.resp_file (Filename.concat "./web" r.Httpd.path)
    | _ -> Httpd.http_error_html 400 ("Unsupported HTTP method "^r.Httpd.verb) []

let start (s, peername) =
  Util.create_thread (Connections.endpoint_name peername ^ " HTTP service")
    None
    (Httpd.main handle_req)
    (s, peername)

let boot_time = Unix.time ()
let api_server_stats r =
  Json.resp_ok [] (Json.Rec
		     ["connection_count", Json.Num (float_of_int !Connections.connection_count);
		      "boot_time", Json.Num boot_time;
		      "uptime", Json.Num (Unix.time () -. boot_time)])

let api_tap_source r =
  let id = Uuid.create () in
  let id_block_and_padding = Stringstream.const_flush (id ^ ";" ^ String.make 2048 'h' ^ ";") in
  let rec message_stream () =
    Thread.delay 0.1;
    let v = Json.to_string (Json.Rec ["now", Json.Num (Unix.time ());
				      "id", Json.Str (Uuid.create ())]) in
    Some (Printf.sprintf "%d;%s;" (String.length v) v, true, Stringstream.make message_stream)
  in
  Httpd.resp_generic 200 "Streaming"
    [Httpd.text_content_type_header;
     "Access-Control-Allow-Origin", "*"]
    (Httpd.Variable
       (Stringstream.switch_after 131072
	  (Stringstream.seq id_block_and_padding (Stringstream.make message_stream))
	  Stringstream.empty))

let counter = ref 0
let api_tap_sink r =
  let params = Httpd.parse_urlencoded (Httpd.string_of_content r.Httpd.req_body.Httpd.content) in
  (* let stream_id = List.assoc "metadata.id" params in *)
  match List.assoc "metadata.type" params with
  | Some "send" ->
      (match List.assoc "data" params with
      | Some data_str ->
	  let data = Json.of_string data_str in
	  counter := 1 + !counter;
	  Printf.printf "Data: %d %s\n%!" !counter (Json.to_string data);
	  Httpd.resp_generic 202 "Accepted" [] (Httpd.empty_content)
      | _ -> Httpd.http_error_html 406 "Bad data parameter" [])
  | _ -> Httpd.http_error_html 406 "Unsupported metadata.type" []

let api_tap r =
  match r.Httpd.verb with
  | "GET" -> api_tap_source r
  | "POST" -> api_tap_sink r
  | _ -> Httpd.http_error_html 400 "Unsupported tap method" []

let register_api_hooks () =
  List.iter register_dispatcher
    ["/_/server_stats", api_server_stats;
     "/_/tap", api_tap]

let init () =
  register_api_hooks ();
  ignore (Util.create_thread "HTTP listener" None (Net.start_net "HTTP" 5678) start)
