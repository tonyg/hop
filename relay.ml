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
  ch m

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

let flush_output mtx flush_control cout =
  while Event.poll (Event.receive flush_control) = None do
    Mutex.lock mtx;
    flush cout;
    Mutex.unlock mtx;
    Thread.delay 0.1
  done

let relay_handler write_sexp n m =
  write_sexp m

let relay_main peername cin cout =
  printf "INFO: Accepted connection from %s\n%!" (endpoint_name peername);
  output_sexp_and_flush cout (Arr [Str "hop"; Str ""]);
  output_sexp_and_flush cout
    (Message.subscribe (Str (Node.local_container_name()),
			Str "", Str "",
			Str "", Str ""));
  let mtx = Mutex.create () in
  let write_sexp s =
    Mutex.lock mtx;
    output_sexp cout s;
    Mutex.unlock mtx
  in
  let flush_control = Event.new_channel () in
  ignore (Util.create_thread (endpoint_name peername ^ " flush") None
	    (flush_output mtx flush_control) cout);
  let n = Node.make "relay" (relay_handler write_sexp) in
  (try
    while true do
      dispatch_message n write_sexp (Message.message_of_sexp (Sexp.input_sexp cin))
    done
  with
  | End_of_file ->
      printf "INFO: Disconnecting %s normally.\n%!" (endpoint_name peername)
  | Sexp.Syntax_error explanation ->
      send_sexp_syntax_error write_sexp explanation);
  Node.unbind_all n;
  Event.sync (Event.send flush_control ())

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
