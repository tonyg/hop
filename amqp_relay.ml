open Unix
open Printf
open Thread
open Amqp_spec

type connection_t = {
    n: Node.t;
    mtx: Mutex.t;
    cin: in_channel;
    cout: out_channel;
    mutable input_buf: string;
    mutable output_buf: Buffer.t;
    mutable frame_max: int;
    mutable connection_closed: bool
  }

let read_frame conn =
  let frame_type = input_byte conn.cin in
  let channel_hi = input_byte conn.cin in
  let channel_lo = input_byte conn.cin in
  let channel = (channel_hi lsr 8) lor channel_lo in
  let length = input_binary_int conn.cin in
  printf "Length %d\n%!" length;
  if length > conn.frame_max
  then raise (Amqp_wireformat.Amqp_exception (frame_error, "Frame longer than current frame_max"))
  else
    (really_input conn.cin conn.input_buf 0 length;
     if input_byte conn.cin <> frame_end
     then raise (Amqp_wireformat.Amqp_exception (frame_error, "Missing frame_end octet"))
     else (frame_type, channel, length))

let write_frame conn frame_type channel =
  output_byte conn.cout frame_type;
  output_byte conn.cout ((channel lsr 8) land 255);
  output_byte conn.cout (channel land 255);
  output_binary_int conn.cout (Buffer.length conn.output_buf);
  Buffer.output_buffer conn.cout conn.output_buf;
  Buffer.reset conn.output_buf;
  output_byte conn.cout frame_end

let serialize_method buf m =
  let (class_id, method_id) = method_index m in
  Amqp_wireformat.write_short buf class_id;
  Amqp_wireformat.write_short buf method_id;
  write_method m buf

let deserialize_method buf =
  let class_id = Amqp_wireformat.read_short buf in
  let method_id = Amqp_wireformat.read_short buf in
  read_method class_id method_id buf

let serialize_header buf body_size p =
  let class_id = class_index p in
  Amqp_wireformat.write_short buf class_id;
  Amqp_wireformat.write_short buf 0;
  Amqp_wireformat.write_longlong buf (Int64.of_int body_size);
  write_properties p buf

let send_content_body conn channel body =
  let offset = ref 0 in
  let len = String.length body in
  while (!offset) < len do
    let snip_len = min conn.frame_max (len - !offset) in
    Buffer.add_substring conn.output_buf body (!offset) snip_len;
    write_frame conn frame_body channel;
    offset := !offset + snip_len
  done

let next_method conn =
  let (frame_type, channel, length) = read_frame conn in
  if frame_type <> frame_method
  then raise (Amqp_wireformat.Amqp_exception
		(command_invalid,
		 (Printf.sprintf "Unexpected frame type %d" frame_type)))
  else
    let buf = Ibuffer.create conn.input_buf 0 length in
    (channel, deserialize_method buf)

let with_conn_mutex conn thunk = Util.with_mutex0 conn.mtx thunk

let send_method conn m =
  with_conn_mutex conn (fun () ->
    serialize_method conn.output_buf m;
    write_frame conn frame_method 0;
    flush conn.cout)

let send_error conn code message =
  if conn.connection_closed
  then
    ()
  else
    let m = Connection_close (code, message, 0, 0) in
    Log.warn "Sending error" [sexp_of_method m];
    send_method conn m

let issue_banner cin cout =
  let handshake = String.create 8 in
  try
    really_input cin handshake 0 8;
    if String.sub handshake 0 4 <> "AMQP"
    then (output_string cout "AMQP\000\000\009\001"; false)
    else true
  with End_of_file -> false

let amqp_handler mtx cin cout n m =
  raise (Amqp_wireformat.Amqp_exception (not_implemented, "TODO"))

let handle_method conn m =
  match m with
  | _ ->
      let (cid, mid) = method_index m in
      raise (Amqp_wireformat.Amqp_exception (not_implemented,
					     Printf.sprintf "Unsupported method %s (%d/%d)"
					       (method_name cid mid) cid mid))

let initial_frame_size = frame_min_size

let amqp_mainloop peername mtx cin cout n =
  let conn = {
    n = n;
    mtx = mtx;
    cin = cin;
    cout = cout;
    input_buf = String.create initial_frame_size;
    output_buf = Buffer.create initial_frame_size;
    frame_max = initial_frame_size;
    connection_closed = false
  } in
  (try
    let (major_version, minor_version, revision) = version in
    send_method conn (Connection_start (major_version, minor_version,
					Amqp_wireformat.table_of_list [],
					"", ""));
    let (_, Connection_start_ok (client_properties, mechanism, response, locale))
	= next_method conn in
    while true do
      let (channel, m) = next_method conn in
      handle_method conn m
    done
  with
  | Amqp_wireformat.Amqp_exception (code, message) ->
      send_error conn code message
  )

let start (s, peername) =
  Connections.start_connection "amqp" issue_banner amqp_handler amqp_mainloop (s, peername)
