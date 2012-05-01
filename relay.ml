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
open Printf
open Thread
open Sexp

let send_error ch message details =
  let m = Message.error (Str message, details) in
  Log.warn "Sending error" [m];
  ch m

let send_sexp_syntax_error ch explanation =
  send_error ch explanation (Str "http://people.csail.mit.edu/rivest/Sexp.txt")

let dispatch_message n ch m =
  match m with
  | Message.Post (Str name, body, token) ->
      Node.send_ignore name body
  | Message.Subscribe (Str filter, sink, name, Str reply_sink, Str reply_name) ->
      if Node.bind(filter, n)
      then Node.post_ignore
	  reply_sink
	  (Str reply_name)
	  (Message.subscribe_ok (Str filter))
	  (Str "")
      else Log.warn "Bind failed" [Str filter]
  | Message.Unsubscribe (Str token) ->
      if Node.unbind token
      then ()
      else Log.warn "Unbind failed" [Str token]
  | _ ->
      send_error ch "Message not understood" (Message.sexp_of_message m)

let issue_banner cin cout =
  output_sexp_and_flush cout (Arr [Str "hop"; Str ""]);
  output_sexp_and_flush cout
    (Message.subscribe (Str (Node.local_container_name()),
			Str "", Str "",
			Str "", Str ""));
  true

let relay_boot (peername, mtx, cin, cout) = (peername, mtx, cin, cout)

let relay_handler (_, mtx, _, cout) _ m =
  Util.with_mutex mtx (output_sexp_and_flush cout) m

let relay_mainloop (peername, mtx, cin, cout) n =
  let write_sexp = Util.with_mutex mtx (output_sexp cout) in
  (try
    while true do
      dispatch_message n write_sexp (Message.message_of_sexp (Sexp.input_sexp cin))
    done
  with
  | Sexp.Syntax_error explanation ->
      (send_sexp_syntax_error write_sexp explanation;
       Log.info "Disconnected relay for syntax error"
	 [Str (Connections.endpoint_name peername); Str explanation])
  )

let start (s, peername) =
  Connections.start_connection "relay" issue_banner
    relay_boot relay_handler relay_mainloop (s, peername)

let init () =
  ignore (Util.create_daemon_thread "Hop listener" None (Net.start_net "Hop" 5671) start)
