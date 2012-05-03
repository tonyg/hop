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

(* Shared queue *)

type 'a t = {
    mtx: Mutex.t;
    mutable capacity: int;
    nonfull: Condition.t;
    nonempty: Condition.t;
    queue: 'a array;
    mutable read_pointer: int;
    mutable write_pointer: int;
  }

let create n = {
  mtx = Mutex.create ();
  capacity = n;
  nonfull = Condition.create ();
  nonempty = Condition.create ();
  queue = Array.make n (Obj.magic None);
  read_pointer = 0;
  write_pointer = 0
}

let approx_capacity q = q.capacity

let add v q =
  Mutex.lock q.mtx;
  while q.capacity < 1 do
    Condition.wait q.nonfull q.mtx
  done;
  q.capacity <- q.capacity - 1;
  Array.set q.queue q.write_pointer v;
  q.write_pointer <- (q.write_pointer + 1) mod (Array.length q.queue);
  Condition.signal q.nonempty;
  Mutex.unlock q.mtx

let _locked_empty q =
  q.capacity = (Array.length q.queue)

let pop q =
  Mutex.lock q.mtx;
  while _locked_empty q do
    Condition.wait q.nonempty q.mtx
  done;
  let result = Array.get q.queue q.read_pointer in
  q.read_pointer <- (q.read_pointer + 1) mod (Array.length q.queue);
  q.capacity <- q.capacity + 1;
  Condition.signal q.nonfull;
  Mutex.unlock q.mtx;
  result

let peek q =
  Mutex.lock q.mtx;
  let result =
    if _locked_empty q
    then None
    else
      (let result = Array.get q.queue q.read_pointer in
      q.read_pointer <- (q.read_pointer + 1) mod (Array.length q.queue);
      q.capacity <- q.capacity + 1;
      Condition.signal q.nonfull;
      Some result)
  in
  Mutex.unlock q.mtx;
  result
