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

open Datastructures

let continue_running = ref true
let control_queue = Squeue.create 1

let achieved_milestones = ref StringSet.empty

let milestone name =
  Squeue.add (`Milestone name) control_queue

let shutdown_now details =
  Squeue.add (`Shutdown details) control_queue

let is_milestone_achieved m =
  match m with
  | Some m' ->
      StringSet.mem m' !achieved_milestones
  | None ->
      false

let rec run' until_milestone =
  match is_milestone_achieved until_milestone with
  | true ->
      ()
  | false ->
      (match Squeue.pop control_queue with
      | `Shutdown details ->
	  Log.error "Shutting down server" details;
	  continue_running := false;
	  ()
      | `Milestone name ->
	  Log.info "Achieved milestone" [Sexp.Str name];
	  achieved_milestones := StringSet.add name !achieved_milestones;
	  run' until_milestone)

let is_running () = !continue_running

let run_until milestone =
  if !continue_running
  then (Log.info "Waiting for milestone" [Sexp.Str milestone];
	run' (Some milestone))
  else ()

let run_forever () =
  if !continue_running
  then run' None
  else ()
