open Unix

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
