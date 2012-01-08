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
      else Log.warn "Bind failed" [Str filter]
  | Message.Unsubscribe token ->
      () (* %%% TODO *)
  | _ ->
      send_error ch "Message not understood" (Message.sexp_of_message m)

let flush_output mtx flush_control cout =
  let rec loop () =
    match Event.poll (Event.receive flush_control) with
    | Some () -> ()
    | None ->
	Mutex.lock mtx;
	let ok = try flush cout; true with _ -> false in
	Mutex.unlock mtx;
	if ok then (Thread.delay 0.1; loop ()) else ()
  in loop ()

let relay_handler write_sexp n m =
  write_sexp m

let relay_main peername cin cout =
  Log.info "Accepted connection" [Str (endpoint_name peername)];
  output_sexp_and_flush cout (Arr [Str "hop"; Str ""]);
  output_sexp_and_flush cout
    (Message.subscribe (Str (Node.local_container_name()),
			Str "", Str "",
			Str "", Str ""));
  let mtx = Mutex.create () in
  let write_sexp s =
    Mutex.lock mtx;
    (try output_sexp cout s with Sys_error _ -> ()); (* TODO: try removing this *)
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
      Log.info "Disconnecting normally" [Str (endpoint_name peername)]
  | Sexp.Syntax_error explanation ->
      send_sexp_syntax_error write_sexp explanation
  | Sys_error message ->
      Log.warn "Disconnected by Sys_error" [Str (endpoint_name peername); Str message]
  );
  Node.unbind_all n;
  Event.sync (Event.send flush_control ())

let start_relay' (s, peername) =
  let cin = in_channel_of_descr s in
  let cout = out_channel_of_descr s in
  connection_count := 1 + !connection_count;
  relay_main peername cin cout;
  connection_count := 0 + !connection_count;
  try flush cout with _ -> ();
  close s

let start_relay (s, peername) =
  Util.create_thread (endpoint_name peername ^ " input") None start_relay' (s, peername)
