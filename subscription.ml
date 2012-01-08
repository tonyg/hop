open Datastructures

type t = {
    mutable live: bool;
    uuid: Uuid.t;
    filter: Sexp.t;
    sink: string;
    name: Sexp.t
  }

type set_t = t StringMap.t ref

let new_set () = ref StringMap.empty

let create source subs filter sink name reply_sink reply_name =
  let uuid = Uuid.create () in
  let sub = {
    live = true;
    uuid = uuid;
    filter = filter;
    sink = sink;
    name = name
  } in
  subs := StringMap.add uuid sub !subs;
  Node.post_ignore reply_sink reply_name (Message.subscribe_ok (Sexp.Str uuid)) (Sexp.Str "");
  sub

let delete source subs uuid =
  (try (StringMap.find uuid !subs).live <- false
  with Not_found -> ());
  subs := StringMap.remove uuid !subs

let send_to_subscription source subs sub body =
  if not sub.live
  then false
  else
    if Node.post sub.sink sub.name body (Sexp.Str sub.uuid)
    then true
    else (delete source subs sub.uuid; false)
