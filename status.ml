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

type ('success, 'failure) t =
  | Ok of 'success
  | Problem of 'failure

type ('transient, 'permanent) failure_t =
  | Transient of 'transient
  | Permanent of 'permanent

let is_ok x =
  match x with
  | Ok _ -> true
  | Problem _ -> false

let is_problem x = not (is_ok x)

let is_transient x =
  match x with
  | Transient _ -> true
  | Permanent _ -> false

let is_permanent x = not (is_transient x)

let replace_ok x info =
  match x with
  | Ok _ -> Ok info
  | Problem p -> Problem p

let replace_ok' x info_fn =
  match x with
  | Ok _ -> Ok (info_fn ())
  | Problem p -> Problem p
