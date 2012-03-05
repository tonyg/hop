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

let create subs filter sink name reply_sink reply_name =
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

let delete subs uuid =
  try
    let sub = StringMap.find uuid !subs in
    sub.live <- false;
    subs := StringMap.remove uuid !subs;
    Some sub
  with Not_found ->
    None

let lookup subs uuid =
  try Some (StringMap.find uuid !subs)
  with Not_found -> None

let send_to_subscription' sub body delete_action =
  if not sub.live
  then false
  else
    if Node.post sub.sink sub.name body (Sexp.Str sub.uuid)
    then true
    else (delete_action sub.uuid; false)

let send_to_subscription subs sub body =
  send_to_subscription' sub body (fun (uuid) -> delete subs uuid)
