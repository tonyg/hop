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

open Hof

let config = ref []

let get key =
  try Some (List.assoc key !config) with Not_found -> None

let get' key default_value =
  try (List.assoc key !config) with Not_found -> default_value

let push k v =
  config := (k, v) :: !config

let get_all key =
  List.filter (fun (k, v) -> k = key) !config
  |> List.rev_map (fun (k, v) -> v)

let init () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  let rec loop index current_key =
    if index >= argc
    then ()
    else
      (let opt = argv.(index) in
      if Util.starts_with opt "--"
      then loop (index + 1) (String.sub opt 2 (String.length opt - 2))
      else (push current_key opt;
	    loop (index + 1) current_key))
  in loop 1 ""
