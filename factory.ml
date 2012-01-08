open Printf
open Sexp
open Datastructures

type factory_t = Sexp.t -> Sexp.t option

let classes = ref StringMap.empty

let register_class name factory =
  if StringMap.mem name !classes
  then (fprintf stderr "ERROR: Duplicate node class name %s\n%!" name;
	exit 1)
  else classes := StringMap.add name factory !classes

let lookup_class name =
  try Some (StringMap.find name !classes)
  with Not_found -> None

let factory_handler n sexp =
  match Message.message_of_sexp sexp with
  | Message.Create (Str classname, arg, Str reply_sink, Str reply_name) ->
      (match lookup_class classname with
      | Some factory ->
	  let reply =
	    match factory arg with
	    | None ->
		Message.create_ok
	    | Some explanation ->
		Message.create_failed explanation
	  in
	  Node.post_ignore reply_sink (Str reply_name) reply (Str "")
      | None ->
	  Log.warn "Node class not found" [Str classname])
  | m ->
      Util.message_not_understood "factory" m

let init () =
  Node.bind_ignore ("factory", Node.make "factory" factory_handler)
