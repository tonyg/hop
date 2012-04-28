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

let hook_log () =
  let old_hook = !Log.hook in
  let new_hook label body =
    ignore (Node.post "system.log" (Sexp.Str label) body (Sexp.Str ""));
    old_hook label body
  in
  Log.hook := new_hook

let _ =
  Printf.printf "%s %s, %s\n%s\n%!"
    App_info.product App_info.version App_info.copyright App_info.licence_blurb;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Uuid.init ();
  Factory.init ();
  Queuenode.init ();
  Fanoutnode.init ();
  Directnode.init ();
  Meta.init ();
  hook_log ();
  Amqp_relay.init ();
  (* Speedtest.init (); *)
  Net.start_net "Hop" 5671 Relay.start
