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

type t = Stream of (unit -> (string * bool * t) option)

let make f = Stream f

let run (Stream f) = f ()

let empty = Stream (fun () -> None)
let const v = Stream (fun () -> Some (v, false, empty))
let const_flush v = Stream (fun () -> Some (v, true, empty))

let flush_before s = Stream (fun () -> Some ("", true, s))
let flush = flush_before empty

let rec seq s1 s2 =
  Stream (fun () ->
    match run s1 with
    | None -> run s2
    | Some (v, f, k) -> Some (v, f, seq k s2))

let rec from_list vs =
  Stream (fun () ->
    match vs with
    | [] -> None
    | v :: vs -> Some (v, false, (from_list vs)))

let rec map f vs =
  Stream (fun () ->
    match vs with
    | [] -> None
    | v :: vs -> run (seq (f v) (map f vs)))

let rec from_iter f =
  let cache = ref None in
  Stream (fun () ->
    match !cache with
    | Some v -> v
    | None ->
	let result =
	  (match f () with
	  | Some (str, should_flush) -> Some (str, should_flush, from_iter f)
	  | None -> None)
	in
	cache := Some result;
	result)

let rec iter f s =
  match run s with
  | None -> ()
  | Some (v, flush, k) -> (f (v, flush); iter f k)

let rec to_list s =
  match run s with
  | None -> []
  | Some (v, _, k) -> v :: to_list k

let rec to_string s =
  String.concat "" (to_list s)

let rec switch_after' on_boundary limit s1 s2 =
  if limit > 0 || not on_boundary
  then Stream (fun () ->
    match run s1 with
    | None -> None
    | Some (v, f, k) -> Some (v, f, switch_after' f (limit - String.length v) k s2))
  else s2

let switch_after limit s1 s2 = switch_after' true limit s1 s2
