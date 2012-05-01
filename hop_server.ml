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

let hook_log () =
  let old_hook = !Log.hook in
  let new_hook label body =
    ignore (Node.post "system.log" (Sexp.Str label) body (Sexp.Str ""));
    old_hook label body
  in
  Log.hook := new_hook

let create_ready_file () =
  match Config.get "ready-file" with
  | Some ready_file_path ->
      Log.info "Creating ready file" [Sexp.Str ready_file_path];
      close_out (open_out ready_file_path)
  | None ->
      ()

let _ =
  Printf.printf "%s %s, %s\n%s\n%!"
    App_info.product App_info.version App_info.copyright App_info.licence_blurb;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Uuid.init ();
  Config.init ();
  Factory.init ();
  Queuenode.init ();
  Fanoutnode.init ();
  Directnode.init ();
  Meta.init ();
  hook_log ();
  Amqp_relay.init ();
  Ui_main.init ();
  Ui_relay.init ();
  (* Speedtest.init (); *)
  Relay.init ();
  Server_control.run_until "AMQP ready";
  Server_control.run_until "HTTP ready";
  Server_control.run_until "Hop ready";
  if Server_control.is_running ()
  then (create_ready_file ();
	Server_control.run_forever ())
  else ()
