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
