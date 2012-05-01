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
    queue: 'a Queue.t
  }

let create n = {
  mtx = Mutex.create ();
  capacity = n;
  nonfull = Condition.create ();
  nonempty = Condition.create ();
  queue = Queue.create ()
}

let approx_capacity q = q.capacity

let add v q =
  Mutex.lock q.mtx;
  while q.capacity < 1 do
    Condition.wait q.nonfull q.mtx
  done;
  q.capacity <- q.capacity - 1;
  Queue.add v q.queue;
  Condition.signal q.nonempty;
  Mutex.unlock q.mtx

let pop q =
  Mutex.lock q.mtx;
  while Queue.is_empty q.queue do
    Condition.wait q.nonempty q.mtx
  done;
  let result = Queue.pop q.queue in
  q.capacity <- q.capacity + 1;
  Condition.signal q.nonfull;
  Mutex.unlock q.mtx;
  result

let peek q =
  Mutex.lock q.mtx;
  let result =
    if Queue.is_empty q.queue
    then None
    else (q.capacity <- q.capacity + 1;
	  Condition.signal q.nonfull;
	  Some (Queue.pop q.queue))
  in
  Mutex.unlock q.mtx;
  result
