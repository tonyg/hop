open Printf
open Datastructures

type handle_message_t = t -> Sexp.t -> unit
and t = {
    mutable names: StringSet.t;
    class_name: string;
    handle_message: handle_message_t
  }

let directory = ref StringMap.empty

let local_container_name () = "server"

let make class_name handler = {
  names = StringSet.empty;
  class_name = class_name;
  handle_message = handler
}

let lookup name =
  try Some (StringMap.find name !directory)
  with Not_found -> None

let bind (filter, node) =
  if filter = ""
  then (printf "WARNING: Binding to empty name forbidden\n%!"; false)
  else
    if StringMap.mem filter !directory
    then false
    else (directory := StringMap.add filter node !directory;
	  node.names <- StringSet.add filter node.names;
	  printf "INFO: Binding node <<%s>> of class %s\n%!" filter node.class_name;
	  true)

(* For use in factory constructor functions, hence the odd return type and values *)
let make_named class_name node_name handler =
  let node = make class_name handler in
  if bind (node_name, node) then None else Some (Sexp.Str "bind-failed")

let unbind name =
  match lookup name with
  | Some n ->
      printf "INFO: Unbinding node <<%s>> of class %s\n%!" name n.class_name;
      n.names <- StringSet.remove name n.names;
      directory := StringMap.remove name !directory;
      true
  | None ->
      false

let unbind_all n =
  StringSet.iter (fun name -> ignore (unbind name)) n.names;
  n.names <- StringSet.empty

let send name body =
  match lookup name with
  | Some n ->
      (try n.handle_message n body
      with e ->
	printf "WARNING: Node <<%s>> message handler raised %s\n%!"
	  name
	  (Printexc.to_string e));
      true
  | None -> false

let post name label body token =
  send name (Message.post (label, body, token))

let bind_ignore (filter, node) =
  if bind (filter, node)
  then ()
  else printf "WARNING: Duplicate binding <<%s>>\n%!" filter

let send_ignore name body =
  if send name body
  then ()
  else (printf "WARNING: send to missing node %s: " name;
	Sexp.output_sexp Pervasives.stdout body;
	print_newline ())

let post_ignore name label body token =
  send_ignore name (Message.post (label, body, token))
