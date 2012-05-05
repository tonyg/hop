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

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module UuidSet = StringSet

let string_map_keys m = StringMap.fold (fun k _ acc -> k :: acc) m []

let classify f xs =
  let rec loop acc xs =
    match xs with
    | [] -> acc
    | x :: xs' ->
	(match f x with
	| Some (classification, v) ->
	    loop
	      (StringMap.add
		 classification
		 (v :: (try StringMap.find classification acc with Not_found -> []))
		 acc)
	      xs'
	| None -> loop acc xs')
  in loop StringMap.empty xs
