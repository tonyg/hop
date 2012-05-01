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
open Printf
open Thread
open Sexp

let connection_mtx = Mutex.create ()
let connection_count = ref 0

let endpoint_name n =
  match n with
  | ADDR_INET (host, port) -> sprintf "%s:%d" (string_of_inet_addr host) port
  | _ -> "??unknown??"

let flush_output mtx flush_control cout =
  let rec loop () =
    match Event.poll (Event.receive flush_control) with
    | Some () -> ()
    | None ->
	let ok = Util.with_mutex0 mtx (fun () -> try flush cout; true with _ -> false) in
	if ok then (Thread.delay 0.1; loop ()) else ()
  in loop ()

let connection_main class_name peername cin cout issue_banner boot_fn node_fn mainloop =
  Log.info ("Accepted "^class_name) [Str (endpoint_name peername)];
  if issue_banner cin cout
  then
    let mtx = Mutex.create () in
    let flush_control = Event.new_channel () in
    ignore (Util.create_thread (endpoint_name peername ^ " flush") None
	      (flush_output mtx flush_control) cout);
    let shared_state = boot_fn (peername, mtx, cin, cout) in
    let n = Node.make class_name (node_fn shared_state) in
    (try
      mainloop shared_state n
    with
    | End_of_file ->
	Log.info ("Disconnecting "^class_name^" normally") [Str (endpoint_name peername)]
    | Sys_error message ->
	Log.warn ("Disconnected "^class_name^" by Sys_error")
	  [Str (endpoint_name peername); Str message]
    | exn ->
	Log.error ("Uncaught exception in "^class_name) [Str (Printexc.to_string exn)]
    );
    Node.unbind_all n;
    Event.sync (Event.send flush_control ())
  else
    Log.error ("Disconnected "^class_name^" by failed initial handshake") []

let start_connection' class_name issue_banner boot_fn node_fn mainloop (s, peername) =
  let cin = in_channel_of_descr s in
  let cout = out_channel_of_descr s in
  Util.with_mutex0 connection_mtx (fun () -> connection_count := !connection_count + 1);
  connection_main class_name peername cin cout issue_banner boot_fn node_fn mainloop;
  Util.with_mutex0 connection_mtx (fun () -> connection_count := !connection_count - 1);
  (try flush cout with _ -> ());
  close s

let start_connection class_name issue_banner boot_fn node_fn mainloop (s, peername) =
  Util.create_thread
    (endpoint_name peername ^ " input")
    None
    (start_connection' class_name issue_banner boot_fn node_fn mainloop)
    (s, peername)
