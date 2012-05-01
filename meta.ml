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

let announce_subscription source filter sink name on_off =
  Node.post_ignore "meta" (Str source)
    (if on_off
    then Message.subscribed (Str source, filter, Str sink, name)
    else Message.unsubscribed (Str source, filter, Str sink, name))
    (Str "")

let init () =
  Node.send_ignore "factory" (Message.create (Str "direct", Arr [Str "meta"], Str "", Str ""))
