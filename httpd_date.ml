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

open Unix

let days = ["Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"]
let months = ["Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"]

(* Example: *)
(* Wed, 15 Nov 1995 06:25:24 GMT *)
let http_gmtime t =
  let tm = gmtime t in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    (List.nth days tm.tm_wday) tm.tm_mday (List.nth months tm.tm_mon) (tm.tm_year + 1900)
    tm.tm_hour tm.tm_min tm.tm_sec
