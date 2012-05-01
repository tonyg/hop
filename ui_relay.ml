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

type outbound_message =
  | Data of Sexp.t
  | Heartbeat
  | Shutdown

let rec message_stream ch =
  let deliver_payload payload =
    Some (Printf.sprintf "%d;%s;" (String.length payload) payload,
	  true,
	  Stringstream.make (message_stream ch))
  in
  let deliver_sexp_chunk sexp = deliver_payload (Json.to_string (Sexpjson.json_of_sexp sexp)) in
  fun () ->
    match Squeue.pop ch with
    | Data sexp -> deliver_sexp_chunk sexp
    | Heartbeat -> deliver_payload ""
    | Shutdown -> None

let rec api_tap_source id r =
  let ch = Squeue.create 10 in
  let handle_message n sexp = Squeue.add (Data sexp) ch in
  let n = Node.make "http_tap" handle_message in
  if not (Node.bind (id, n))
  then Httpd.http_error_html 500 "Internal ID collision" []
  else
    let id_block_and_padding = Stringstream.const_flush (id ^ ";" ^ String.make 2048 'h' ^ ";") in
    handle_message n (Message.subscribe (Sexp.Str (Node.local_container_name()),
					 Sexp.Str "", Sexp.Str "",
					 Sexp.Str "", Sexp.Str ""));
    Httpd.add_completion_callback
      (Httpd.resp_generic 200 "Streaming"
	 ([Httpd.text_content_type_header;
	   "Access-Control-Allow-Origin", "*";
	   "Date", Httpd_date.http_gmtime (Unix.time ())]
	  @ Httpd.disable_cache_headers ())
	 (Httpd.Variable
	    (Stringstream.switch_after 131072
	       (Stringstream.seq id_block_and_padding (Stringstream.make (message_stream ch)))
	       Stringstream.empty)))
      (fun _ ->
	Node.unbind_all n;
	Squeue.add Shutdown ch)

let api_tap_sink irrelevant_id r =
  let params = Httpd.parse_urlencoded (Httpd.string_of_content r.Httpd.req_body.Httpd.content) in
  (* let stream_id = List.assoc "metadata.id" params in *)
  match Httpd.find_param "metadata.type" params with
  | Some (Some "send") ->
      (match Httpd.find_param "data" params with
      | Some (Some data_str) ->
	  let data =
	    (try Sexpjson.sexp_of_json (Json.of_string data_str)
	    with _ -> Httpd.http_error_html 406 "Bad data parameter" []) in
	  (match Message.message_of_sexp data with
	  | Message.Post (Sexp.Str name, body, token) ->
	      Node.send_ignore name body;
	      Httpd.resp_generic 202 "Accepted" [] (Httpd.empty_content)
	  | _ ->
	      Httpd.http_error_html 406 "Message not understood" [])
      | _ -> Httpd.http_error_html 406 "Bad data parameter" [])
  | _ -> Httpd.http_error_html 406 "Unsupported metadata.type" []

let api_tap id r =
  match r.Httpd.verb with
  | "GET" -> api_tap_source id r
  | "POST" -> api_tap_sink id r
  | _ -> Httpd.http_error_html 400 "Unsupported tap method" []

let init () =
  Ui_main.register_dispatcher ("/_/tap", api_tap)
