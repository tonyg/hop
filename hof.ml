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

(* Higher-order function utilities, based on the interfaces in Ocaml
Batteries. *)

let ( |> ) x f = f x
let ( **> ) f x = f x

let identity x = x
let const x y = x

let ( |- ) first second x = second (first x)
let ( -| ) second first x = second (first x)

let flip f x y = f y x

let ( *** ) f g (x, y) = (f x, g y)
let ( &&& ) f g x = (f x, g x)

let first f (x, y) = (f x, y)
let second f (x, y) = (x, f y)
