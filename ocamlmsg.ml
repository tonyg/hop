open Unix
open Printf
open Thread

let rec accept_loop sock =
  let (s, peername) = accept sock in
  ignore (Relay.start_relay (s, peername));
  accept_loop sock

let start_net port_number =
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock (ADDR_INET (inet_addr_of_string "0.0.0.0", port_number));
  listen sock 5;
  Log.info "Accepting connections" [Sexp.Str (string_of_int port_number)];
  accept_loop sock

let hook_log () =
  let old_hook = !Log.hook in
  let new_hook label body =
    ignore (Node.post "system.log" (Sexp.Str label) body (Sexp.Str ""));
    old_hook label body
  in
  Log.hook := new_hook

let _ =
  printf "ocamlmsg ALPHA, Copyright (C) 2012 Tony Garnock-Jones. All rights reserved.\n%!";
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Uuid.init ();
  Factory.init ();
  Queuenode.init ();
  hook_log ();
  start_net 5671
