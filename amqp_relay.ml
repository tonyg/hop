open Unix
open Printf
open Thread
open Amqp_spec
open Amqp_wireformat

type connection_t = {
    n: Node.t;
    mtx: Mutex.t;
    cin: in_channel;
    cout: out_channel;
    mutable input_buf: string;
    mutable output_buf: Buffer.t;
    mutable frame_max: int;
    mutable connection_closed: bool;
    mutable recent_queue_name: string option;
  }

let read_frame conn =
  let frame_type = input_byte conn.cin in
  let channel_hi = input_byte conn.cin in
  let channel_lo = input_byte conn.cin in
  let channel = (channel_hi lsr 8) lor channel_lo in
  let length = input_binary_int conn.cin in
  if length > conn.frame_max
  then die frame_error "Frame longer than current frame_max"
  else
    (really_input conn.cin conn.input_buf 0 length;
     if input_byte conn.cin <> frame_end
     then die frame_error "Missing frame_end octet"
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
  write_short buf class_id;
  write_short buf method_id;
  write_method m buf

let deserialize_method buf =
  let class_id = read_short buf in
  let method_id = read_short buf in
  read_method class_id method_id buf

let serialize_header buf body_size p =
  let class_id = class_index p in
  write_short buf class_id;
  write_short buf 0;
  write_longlong buf (Int64.of_int body_size);
  write_properties p buf

let deserialize_header buf =
  let class_id = read_short buf in
  let _ = read_short buf in
  let body_size = Int64.to_int (read_longlong buf) in
  (body_size, read_properties class_id buf)

let send_content_body conn channel body =
  let offset = ref 0 in
  let len = String.length body in
  while (!offset) < len do
    let snip_len = min conn.frame_max (len - !offset) in
    Buffer.add_substring conn.output_buf body (!offset) snip_len;
    write_frame conn frame_body channel;
    offset := !offset + snip_len
  done

let next_frame conn required_type =
  let (frame_type, channel, length) = read_frame conn in
  if frame_type <> required_type
  then die command_invalid (Printf.sprintf "Unexpected frame type %d" frame_type)
  else (channel, length)

let next_method conn =
  let (channel, length) = next_frame conn frame_method in
  (channel, deserialize_method (Ibuffer.create conn.input_buf 0 length))

let next_header conn =
  let (channel, length) = next_frame conn frame_header in
  (channel, deserialize_header (Ibuffer.create conn.input_buf 0 length))

let recv_content_body conn body_size =
  let buf = Buffer.create body_size in
  while Buffer.length buf < body_size do
    let (_, length) = next_frame conn frame_body in
    Buffer.add_substring buf conn.input_buf 0 length
  done;
  Buffer.contents buf

let with_conn_mutex conn thunk = Util.with_mutex0 conn.mtx thunk

let send_method conn channel m =
  with_conn_mutex conn (fun () ->
    serialize_method conn.output_buf m;
    write_frame conn frame_method channel;
    flush conn.cout)

let send_error conn code message =
  if conn.connection_closed
  then
    ()
  else
    let m = Connection_close (code, message, 0, 0) in
    Log.warn "Sending error" [sexp_of_method m];
    send_method conn 0 m

let issue_banner cin cout =
  let handshake = String.create 8 in
  try
    really_input cin handshake 0 8;
    if String.sub handshake 0 4 <> "AMQP"
    then (output_string cout "AMQP\000\000\009\001"; false)
    else true
  with End_of_file -> false

let amqp_handler mtx cin cout n m =
  die not_implemented "TODO:amqp_handler"

let get_recent_queue_name conn =
  match conn.recent_queue_name with
  | Some q -> q
  | None -> die syntax_error "Attempt to use nonexistent most-recently-declared-queue name"

let expand_mrdq conn queue =
  match queue with
  | "" -> get_recent_queue_name conn
  | other -> other

let handle_method conn channel m =
  if channel > 1 then die channel_error "Unsupported channel number" else ();
  match m with
  | Connection_close (code, text, _, _) ->
      Log.info "Client closed AMQP connection" [Sexp.Str (string_of_int code); Sexp.Str text];
      send_method conn channel Connection_close_ok;
      conn.connection_closed <- true
  | Channel_open -> send_method conn channel Channel_open_ok
  | Channel_close (code, text, _, _) ->
      Log.info "Client closed AMQP channel" [Sexp.Str (string_of_int code); Sexp.Str text];
      send_method conn channel Channel_close_ok;
  | Exchange_declare (exchange, type_, passive, durable, no_wait, arguments) ->
      Log.info "XDeclare%%%" [Sexp.Str exchange; Sexp.Str type_];
      send_method conn channel Exchange_declare_ok
  | Queue_declare (queue, passive, durable, exclusive, auto_delete, no_wait, arguments) ->
      let queue = (if queue = "" then Uuid.create () else queue) in
      Log.info "QDeclare%%%" [Sexp.Str queue];
      conn.recent_queue_name <- Some queue;
      send_method conn channel (Queue_declare_ok (queue, Int32.of_int 0, Int32.of_int 0))
  | Queue_bind (queue, exchange, routing_key, no_wait, arguments) ->
      let queue = expand_mrdq conn queue in
      Log.info "QBind%%%" [Sexp.Str queue; Sexp.Str exchange; Sexp.Str routing_key];
      send_method conn channel Queue_bind_ok
  | Basic_consume (queue, consumer_tag, no_local, no_ack, exclusive, no_wait, arguments) ->
      let queue = expand_mrdq conn queue in
      Log.info "Consume%%%" [Sexp.Str queue; Sexp.Str consumer_tag];
      send_method conn channel (Basic_consume_ok consumer_tag)
  | Basic_publish (exchange, routing_key, false, false) ->
      let (_, (body_size, properties)) = next_header conn in
      let body = recv_content_body conn body_size in
      Log.info "Publish%%%" [Sexp.Str exchange; Sexp.Str routing_key;
			     sexp_of_properties properties; Sexp.Str body]
  | _ ->
      let (cid, mid) = method_index m in
      die not_implemented (Printf.sprintf "Unsupported method (or method arguments) %s"
			     (method_name cid mid))

let initial_frame_size = frame_min_size
let suggested_frame_max = 131072

let server_properties = table_of_list [
  ("product", Table_string App_info.product);
  ("version", Table_string App_info.version);
  ("copyright", Table_string App_info.copyright);
  ("licence", Table_string App_info.licence);
  ("capabilities", Table_table (table_of_list []));
]

let check_login_details mechanism response =
  match mechanism with
  | "PLAIN" ->
      (match (Str.split (Str.regexp "\000") response) with
      | ["guest"; "guest"] -> ()
      | _ -> die access_refused "Access refused")
  | "AMQPLAIN" ->
      (let fields = decode_named_fields (Ibuffer.of_string response) in
      match (field_lookup_some "LOGIN" fields, field_lookup_some "PASSWORD" fields) with
      | (Some (Table_string "guest"), Some (Table_string "guest")) -> ()
      | _ -> die access_refused "Access refused")
  | _ -> die access_refused  "Bad auth mechanism"

let tune_connection conn frame_max =
  with_conn_mutex conn (fun () ->
    conn.input_buf <- String.create frame_max;
    conn.output_buf <- Buffer.create frame_max;
    conn.frame_max <- frame_max)

let handshake_and_tune conn =
  let (major_version, minor_version, revision) = version in
  send_method conn 0 (Connection_start (major_version, minor_version, server_properties,
					"PLAIN AMQPLAIN", "en_US"));
  let (client_properties, mechanism, response, locale) =
    match next_method conn with
    | (0, Connection_start_ok props) -> props
    | _ -> die not_allowed "Expected Connection_start_ok on channel 0"
  in
  check_login_details mechanism response;
  Log.info "Connection from AMQP client" [sexp_of_table client_properties];
  send_method conn 0 (Connection_tune (1, Int32.of_int suggested_frame_max, 0));
  let (channel_max, frame_max, heartbeat) =
    match next_method conn with
    | (0, Connection_tune_ok props) -> props
    | _ -> die not_allowed "Expected Connection_tune_ok on channel 0"
  in
  if channel_max > 1
  then die not_implemented "Channel numbers higher than 1 are not supported" else ();
  if (Int32.to_int frame_max) > suggested_frame_max
  then die syntax_error "Requested frame max too large" else ();
  if heartbeat > 0
  then die not_implemented "Heartbeats not yet implemented (patches welcome)" else ();
  tune_connection conn (Int32.to_int frame_max);
  let (virtual_host) =
    match next_method conn with
    | (0, Connection_open props) -> props
    | _ -> die not_allowed "Expected Connection_open on channel 0"
  in
  Log.info "Connected to vhost" [Sexp.Str virtual_host];
  send_method conn 0 Connection_open_ok

let amqp_mainloop peername mtx cin cout n =
  let conn = {
    n = n;
    mtx = mtx;
    cin = cin;
    cout = cout;
    input_buf = String.create initial_frame_size;
    output_buf = Buffer.create initial_frame_size;
    frame_max = initial_frame_size;
    connection_closed = false;
    recent_queue_name = None;
  } in
  (try
    handshake_and_tune conn;
    while not conn.connection_closed do
      let (channel, m) = next_method conn in
      handle_method conn channel m
    done
  with
  | Amqp_exception (code, message) ->
      send_error conn code message
  )

let start (s, peername) =
  Connections.start_connection "amqp" issue_banner amqp_handler amqp_mainloop (s, peername)
