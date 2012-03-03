open Unix
open Printf
open Thread
open Sexp

let connection_count = ref 0

let endpoint_name n =
  match n with
  | ADDR_INET (host, port) -> sprintf "%s:%d" (string_of_inet_addr host) port
  | _ -> "??unknown??"

let flush_output mtx flush_control cout =
  let rec loop () =
    match Event.poll (Event.receive flush_control) with
    | Some () -> ()
    | None ->
	let ok = Util.with_mutex0 mtx (fun () -> try flush cout; true with _ -> false) in
	if ok then (Thread.delay 0.1; loop ()) else ()
  in loop ()

let connection_main class_name peername cin cout issue_banner node_fn mainloop =
  Log.info ("Accepted "^class_name) [Str (endpoint_name peername)];
  if issue_banner cin cout
  then
    let mtx = Mutex.create () in
    let flush_control = Event.new_channel () in
    ignore (Util.create_thread (endpoint_name peername ^ " flush") None
	      (flush_output mtx flush_control) cout);
    let n = Node.make class_name (node_fn mtx cin cout) in
    (try
      mainloop peername mtx cin cout n
    with
    | End_of_file ->
	Log.info ("Disconnecting "^class_name^" normally") [Str (endpoint_name peername)]
    | Sys_error message ->
	Log.warn ("Disconnected "^class_name^" by Sys_error")
	  [Str (endpoint_name peername); Str message]
    | exn ->
	Log.error ("Uncaught exception in "^class_name) [Str (Printexc.to_string exn)]
    );
    Node.unbind_all n;
    Event.sync (Event.send flush_control ())
  else
    Log.error ("Disconnected "^class_name^" by failed initial handshake") []

let start_connection' class_name issue_banner node_fn mainloop (s, peername) =
  let cin = in_channel_of_descr s in
  let cout = out_channel_of_descr s in
  connection_count := !connection_count + 1;
  connection_main class_name peername cin cout issue_banner node_fn mainloop;
  connection_count := !connection_count - 1;
  (try flush cout with _ -> ());
  close s

let start_connection class_name issue_banner node_fn mainloop (s, peername) =
  Util.create_thread
    (endpoint_name peername ^ " input")
    None
    (start_connection' class_name issue_banner node_fn mainloop)
    (s, peername)
