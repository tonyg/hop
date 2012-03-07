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

type t = {
    mutable pos: int;
    limit: int;
    buf: string;
  }

let create s ofs len = {
  pos = ofs;
  limit = ofs + len;
  buf = s
}

let of_string s = create s 0 (String.length s)

let sub b ofs len =
  if b.pos + ofs + len > b.limit
  then
    raise End_of_file
  else
    { pos = b.pos + ofs;
      limit = b.pos + ofs + len;
      buf = b.buf }

let remaining b = b.limit - b.pos

let next_char b =
  if b.pos < b.limit
  then
    let v = String.get b.buf b.pos in
    b.pos <- b.pos + 1;
    v
  else
    raise End_of_file

let next_byte b = int_of_char (next_char b)

let next_chars b n =
  if remaining b < n
  then
    raise End_of_file
  else
    let dst = String.create n in
    String.blit b.buf b.pos dst 0 n;
    b.pos <- b.pos + n;
    dst

let next_sub b n =
  let v = sub b 0 n in
  b.pos <- b.pos + n;
  v
