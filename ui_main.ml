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

open Html
open Hof
open Datastructures

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
	let wholepath = r.Httpd.path in
	if Util.starts_with wholepath prefix
	then
	  (let wholepath_len = String.length wholepath in
	  let prefix_len = String.length prefix in
	  let suffix = String.sub wholepath prefix_len (wholepath_len - prefix_len) in
	  handler suffix id r)
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

let cleanup_req id exn =
  match Node.lookup id with
  | Some n -> Node.unbind_all n
  | None -> ()

let start (s, peername) =
  let id = Node.name_of_string ("http-" ^ Uuid.create ()) in
  Util.create_thread (Connections.endpoint_name peername ^ " HTTP service")
    (Some (cleanup_req id))
    (Httpd.main (handle_req id))
    (s, peername)

let boot_time = Unix.time ()
let api_server_stats _ id r =
  Json.resp_ok [] (Json.Rec
		     ["connection_count", Json.Num (float_of_int !Connections.connection_count);
		      "boot_time", Json.Num boot_time;
		      "uptime", Json.Num (Unix.time () -. boot_time);
		      "classes", Json.Arr (List.map Json.str (Factory.all_class_names ()))])
  |> Httpd.add_date_header

let api_nodes _ id r =
  let by_class_name name =
    match Node.lookup name with
    | Some n -> Some (n.Node.class_name, name.Node.label)
    | None -> None
  in
  let info = classify by_class_name (Node.all_node_names ()) in
  Json.resp_ok []
    (Json.Rec
       (List.map
	  (fun (class_name, node_names) -> (class_name, Json.Arr (List.map Json.str node_names)))
	  (StringMap.bindings info)))
  |> Httpd.add_date_header

let api_node_info suffix id r =
  (match Node.lookup (Node.name_of_string suffix) with
  | Some n ->
      Json.resp_ok [] (Json.Rec
			 ["names", Json.Arr (List.map Json.str (StringSet.elements n.Node.names));
			  "class_name", Json.Str n.Node.class_name])
  | None ->
      Json.resp 404 "No such node name" [] Json.Nil)
  |> Httpd.add_date_header

let init () =
  register_dispatcher ("/_/server_stats", api_server_stats);
  register_dispatcher ("/_/nodes", api_nodes);
  register_dispatcher ("/_/node/", api_node_info);
  ignore (Util.create_daemon_thread "HTTP listener" None (Net.start_net "HTTP" 5678) start)
