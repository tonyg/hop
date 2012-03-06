open Sexp
open Datastructures
open Status

type t = {
    name: string;
    subscriptions: Subscription.set_t;
    mtx: Mutex.t;
  }

let classname = "fanout"

let unsubscribe info uuid =
  Util.with_mutex0 info.mtx
    (fun () -> ignore (Subscription.delete info.name info.subscriptions uuid))

let route_message info n sexp =
  match Message.message_of_sexp sexp with
  | Message.Post (Str name, body, token) ->
      let snapshot = !(info.subscriptions) in
      StringMap.iter
	(fun uuid sub ->
	  ignore (Subscription.send_to_subscription' sub body (unsubscribe info)))
	snapshot
  | Message.Subscribe (Str binding_key as filter, Str sink, name, Str reply_sink, reply_name) ->
      Util.with_mutex0 info.mtx
	(fun () ->
	  ignore (Subscription.create
		    info.name info.subscriptions filter sink name reply_sink reply_name))
  | Message.Unsubscribe (Str token) ->
      unsubscribe info token
  | m ->
      Util.message_not_understood classname m

let factory arg =
  match arg with
  | (Arr [Str name]) ->
      let info = {
	name = name;
	subscriptions = Subscription.new_set ();
	mtx = Mutex.create ();
      } in
      replace_ok (Node.make_idempotent_named classname name (route_message info)) (Str name)
  | _ ->
      Problem (Str "bad-arg")

let init () =
  Factory.register_class classname factory
