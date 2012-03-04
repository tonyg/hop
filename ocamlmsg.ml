open Unix
open Printf
open Thread

let rec accept_loop sock connection_start_fn =
  let (s, peername) = accept sock in
  setsockopt s TCP_NODELAY true;
  ignore (connection_start_fn (s, peername));
  accept_loop sock connection_start_fn

let start_net port_number connection_start_fn =
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock (ADDR_INET (inet_addr_of_string "0.0.0.0", port_number));
  listen sock 5;
  Log.info "Accepting connections" [Sexp.Str (string_of_int port_number)];
  accept_loop sock connection_start_fn

let hook_log () =
  let old_hook = !Log.hook in
  let new_hook label body =
    ignore (Node.post "system.log" (Sexp.Str label) body (Sexp.Str ""));
    old_hook label body
  in
  Log.hook := new_hook

let _ =
  printf "%s %s, %s %s\n%!" App_info.product App_info.version App_info.copyright App_info.licence;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Uuid.init ();
  Factory.init ();
  Queuenode.init ();
  Directnode.init ();
  hook_log ();
  ignore (Util.create_thread "AMQP listener" None (start_net Amqp_spec.port) Amqp_relay.start);
  start_net 5671 Relay.start
