open Unix
open Printf
open Thread
open Sexp

let connection_count = ref 0

let endpoint_name n =
  match n with
  | ADDR_INET (host, port) -> sprintf "%s:%d" (string_of_inet_addr host) port
  | _ -> "??unknown??"

let send_error ch message details =
  let m = Message.error (Str message, details) in
  print_string "WARNING: Sending error: ";
  output_sexp Pervasives.stdout m;
  print_newline ();
  output_sexp_and_flush ch m

let send_sexp_syntax_error ch explanation =
  send_error ch explanation (Str "http://people.csail.mit.edu/rivest/Sexp.txt")

let dispatch_message n ch m =
  match m with
  | Message.Post (Str name, body, token) ->
      Node.send_ignore name body
  | Message.Subscribe (Str filter, sink, name, Str reply_sink, Str reply_name) ->
      if Node.bind(filter, n)
      then Node.post_ignore
	  reply_sink
	  (Str reply_name)
	  (Message.subscribe_ok (Str filter))
	  (Str "")
      else printf "WARNING: Bind failed <<%s>>\n%!" filter
  | Message.Unsubscribe token ->
      () (* %%% TODO *)
  | _ ->
      send_error ch "Message not understood" (Message.sexp_of_message m)

let output_thread ch cout =
  let rec loop v =
    match v with
    | Some (Some sexp) ->
	output_sexp cout sexp;
	loop (Event.poll (Event.receive ch))
    | Some None ->
	()
    | None ->
	(* flush cout; *)
	loop (Some (Event.sync (Event.receive ch)))
  in loop None

let relay_handler mtx cout n m =
  Mutex.lock mtx;
  output_sexp cout m;
  Mutex.unlock mtx

let relay_main peername cin cout =
  printf "INFO: Accepted connection from %s\n%!" (endpoint_name peername);
  output_sexp_and_flush cout (Arr [Str "hop"; Str ""]);
  output_sexp_and_flush cout
    (Message.subscribe (Str (Node.local_container_name()),
			Str "", Str "",
			Str "", Str ""));
  let mtx = Mutex.create () in
  let n = Node.make "relay" (relay_handler mtx cout) in
  (try
    while true do
      dispatch_message n cout (Message.message_of_sexp (Sexp.input_sexp cin))
    done
  with
  | End_of_file ->
      printf "INFO: Disconnecting %s normally.\n%!" (endpoint_name peername)
  | Sexp.Syntax_error explanation ->
      send_sexp_syntax_error cout explanation);
  Node.unbind_all n

let start_relay' (s, peername) =
  let cin = in_channel_of_descr s in
  let cout = out_channel_of_descr s in
  connection_count := 1 + !connection_count;
  relay_main peername cin cout;
  connection_count := 0 + !connection_count;
  flush cout;
  close s

let start_relay (s, peername) =
  Util.create_thread (endpoint_name peername ^ " input") None start_relay' (s, peername)
