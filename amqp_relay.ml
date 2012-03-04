open Unix
open Printf
open Thread
open Amqp_spec
open Amqp_wireformat

type connection_t = {
    peername: Unix.sockaddr;
    mtx: Mutex.t;
    cin: in_channel;
    cout: out_channel;
    name: Uuid.t;
    mutable input_buf: string;
    mutable output_buf: Buffer.t;
    mutable frame_max: int;
    mutable connection_closed: bool;
    mutable recent_queue_name: string option;
    mutable delivery_tag: int
  }

let initial_frame_size = frame_min_size
let suggested_frame_max = 131072

let amqp_boot (peername, mtx, cin, cout) = {
  peername = peername;
  mtx = mtx;
  cin = cin;
  cout = cout;
  name = Uuid.create ();
  input_buf = String.create initial_frame_size;
  output_buf = Buffer.create initial_frame_size;
  frame_max = initial_frame_size;
  connection_closed = false;
  recent_queue_name = None;
  delivery_tag = 1 (* Not 0: 0 means "all deliveries" in an ack *)
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

let send_content_method conn channel m p body_str =
  with_conn_mutex conn (fun () ->
    serialize_method conn.output_buf m;
    write_frame conn frame_method 1;
    serialize_header conn.output_buf (String.length body_str) p;
    write_frame conn frame_header 1;
    send_content_body conn 1 body_str;
    flush conn.cout)

let send_error conn code message =
  if conn.connection_closed
  then
    ()
  else
    conn.connection_closed <- true;
    let m = Connection_close (code, message, 0, 0) in
    Log.warn "Sending error" [sexp_of_method m];
    send_method conn 0 m

let send_warning conn code message =
  let m = Channel_close (code, message, 0, 0) in
  Log.warn "Sending warning" [sexp_of_method m];
  send_method conn 1 m

let issue_banner cin cout =
  let handshake = String.create 8 in
  try
    really_input cin handshake 0 8;
    if String.sub handshake 0 4 <> "AMQP"
    then (output_string cout "AMQP\000\000\009\001"; false)
    else true
  with End_of_file -> false

let reply_to_declaration conn status ok_fn =
  match Message.message_of_sexp status with
  | Message.Create_ok info ->
      send_method conn 1 (ok_fn info)
  | Message.Create_failed reason ->
      (match reason with
      | Sexp.Str s -> send_warning conn precondition_failed s
      | _ -> send_warning conn precondition_failed "See server logs for details")
  | _ -> die internal_error "Declare reply malformed"

let make_queue_declare_ok info =
  match info with
  | Sexp.Str queue_name -> Queue_declare_ok (queue_name, Int32.zero, Int32.zero)
  | _ -> die internal_error "Unusable queue name in declare response"

let send_delivery conn consumer_tag body_sexp =
  match body_sexp with
  | Sexp.Hint {Sexp.hint = Sexp.Str "amqp";
	       Sexp.body = Sexp.Arr [Sexp.Str exchange;
				     Sexp.Str routing_key;
				     properties_sexp;
				     Sexp.Str body_str]} ->
      let tag = with_conn_mutex conn (fun () ->
	let v = conn.delivery_tag in conn.delivery_tag <- v + 1; v)
      in
      send_content_method conn 1
	(Basic_deliver (consumer_tag, Int64.of_int tag, false, exchange, routing_key))
	(properties_of_sexp basic_class_id properties_sexp)
	body_str
  | _ -> die internal_error "Malformed AMQP message body sexp"

let amqp_handler conn n m_sexp =
  try
    (match Message.message_of_sexp m_sexp with
    | Message.Post (Sexp.Str "Exchange_declare_reply", status, _) ->
	reply_to_declaration conn status (fun (_) -> Exchange_declare_ok)
    | Message.Post (Sexp.Str "Queue_declare_reply", status, _) ->
	reply_to_declaration conn status make_queue_declare_ok
    | Message.Post (Sexp.Str "Queue_bind_reply", status, _) ->
	(match Message.message_of_sexp status with
	| Message.Subscribe_ok _ -> send_method conn 1 Queue_bind_ok
	| _ -> die internal_error "Queue bind reply malformed")
    | Message.Post (Sexp.Arr [Sexp.Str "Basic_consume_reply"; Sexp.Str consumer_tag], status, _) ->
	(match Message.message_of_sexp status with
	| Message.Subscribe_ok _ -> send_method conn 1 (Basic_consume_ok consumer_tag)
	| _ -> die internal_error "Basic consume reply malformed")
    | Message.Post (Sexp.Arr [Sexp.Str "delivery"; Sexp.Str consumer_tag], body, _) ->
	send_delivery conn consumer_tag body
    | _ ->
	Log.warn "AMQP outbound relay ignoring message" [m_sexp])
  with
  | Amqp_exception (code, message) ->
      send_error conn code message
  | exn ->
      send_error conn internal_error "";
      raise exn

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
  | Channel_open ->
      conn.delivery_tag <- 1;
      send_method conn channel Channel_open_ok
  | Channel_close (code, text, _, _) ->
      Log.info "Client closed AMQP channel" [Sexp.Str (string_of_int code); Sexp.Str text];
      send_method conn channel Channel_close_ok;
  | Exchange_declare (exchange, type_, passive, durable, no_wait, arguments) ->
      Node.send_ignore "factory" (Message.create (Sexp.Str type_,
						  Sexp.Arr [Sexp.Str exchange],
						  Sexp.Str conn.name,
						  Sexp.Str "Exchange_declare_reply"))
  | Queue_declare (queue, passive, durable, exclusive, auto_delete, no_wait, arguments) ->
      let queue = (if queue = "" then Uuid.create () else queue) in
      conn.recent_queue_name <- Some queue;
      Node.send_ignore "factory" (Message.create (Sexp.Str "queue",
						  Sexp.Arr [Sexp.Str queue],
						  Sexp.Str conn.name,
						  Sexp.Str "Queue_declare_reply"))
  | Queue_bind (queue, exchange, routing_key, no_wait, arguments) ->
      let queue = expand_mrdq conn queue in
      if not (Node.exists queue)
      then send_warning conn not_found ("Queue "^queue^" not found")
      else
	if Node.send exchange (Message.subscribe (Sexp.Str routing_key,
						  Sexp.Str queue,
						  Sexp.Str "",
						  Sexp.Str conn.name,
						  Sexp.Str "Queue_bind_reply"))
	then ()
	else send_warning conn not_found ("Exchange "^exchange^" not found")
  | Basic_consume (queue, consumer_tag, no_local, no_ack, exclusive, no_wait, arguments) ->
      let queue = expand_mrdq conn queue in
      let consumer_tag = (if consumer_tag = "" then Uuid.create () else consumer_tag) in
      if Node.send queue (Message.subscribe
			    (Sexp.Str "",
			     Sexp.Str conn.name,
			     Sexp.Arr [Sexp.Str "delivery"; Sexp.Str consumer_tag],
			     Sexp.Str conn.name,
			     Sexp.Arr [Sexp.Str "Basic_consume_reply"; Sexp.Str consumer_tag]))
      then ()
      else send_warning conn not_found ("Queue "^queue^" not found")
  | Basic_publish (exchange, routing_key, false, false) ->
      let (_, (body_size, properties)) = next_header conn in
      let body = recv_content_body conn body_size in
      if Node.post exchange
	  (Sexp.Str routing_key)
	  (Sexp.Hint {Sexp.hint = Sexp.Str "amqp";
		      Sexp.body = Sexp.Arr [Sexp.Str exchange;
					    Sexp.Str routing_key;
					    sexp_of_properties properties;
					    Sexp.Str body]})
	  (Sexp.Str "")
      then ()
      else send_warning conn not_found ("Exchange "^exchange^" not found")
  | Basic_ack (delivery_tag, multiple) ->
      ()
  | _ ->
      let (cid, mid) = method_index m in
      die not_implemented (Printf.sprintf "Unsupported method (or method arguments) %s"
			     (method_name cid mid))

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

let amqp_mainloop conn n =
  Node.bind_ignore (conn.name, n);
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
  Connections.start_connection "amqp" issue_banner
    amqp_boot amqp_handler amqp_mainloop (s, peername)

let init () =
  Node.send_ignore "factory" (Message.create (Sexp.Str "direct",
					      Sexp.Arr [Sexp.Str "amq.direct"],
					      Sexp.Str "", Sexp.Str ""));
  ignore (Util.create_thread "AMQP listener" None (Net.start_net Amqp_spec.port) start)
