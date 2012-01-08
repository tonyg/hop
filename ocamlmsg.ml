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
  printf "INFO: Accepting connections on port %d.\n%!" port_number;
  accept_loop sock

let _ =
  printf "ocamlmsg ALPHA, Copyright (C) 2012 Tony Garnock-Jones. All rights reserved.\n%!";
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Uuid.init ();
  Factory.init ();
  Queuenode.init ();
  start_net 5671
