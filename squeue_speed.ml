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

open Sexp

let nmessages = 500000

let receiver_done = Squeue.create 1

let receiver ch =
  let count = ref 0 in
  while !count < nmessages do
    ignore (Squeue.pop ch);
    count := !count + 1
  done;
  Log.info "done" [];
  Squeue.add () receiver_done

let sender () =
  let ch = Squeue.create 1000 in
  ignore (Util.create_thread "Speed test receiver" None receiver ch);
  (try
    let starttime = Unix.gettimeofday () in
    let count = ref 0 in
    while !count < nmessages do
      Squeue.add () ch;
      count := !count + 1
    done;
    let stoptime = Unix.gettimeofday () in
    let delta = stoptime -. starttime in
    Log.info (Printf.sprintf "Speedtest took %d ms (%d Hz)"
		(int_of_float (delta *. 1000.0))
		(int_of_float ((float_of_int nmessages) /. delta))) []
  with exn -> Log.error "Uncaught exception in speedtest" [Str (Printexc.to_string exn)])

let _ =
  sender ();
  Squeue.pop receiver_done
