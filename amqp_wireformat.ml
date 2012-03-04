open Sexp

exception Amqp_exception of (int * string)

let die code message = raise (Amqp_exception (code, message))

type octet_t = int
type short_t = int
type long_t = int32
type longlong_t = int64
type shortstr_t = string
type longstr_t = string
type bit_t = bool
type timestamp_t = int64

type table_t = { mutable table_body: table_body_t }
and table_body_t =
  | Encoded_table of string
  | Decoded_table of (string * table_value_t) list
  | Both_table of (string * (string * table_value_t) list)
and table_value_t =
  | Table_bool of bool (* t *)
  | Table_signed_byte of int (* b *)
  | Table_unsigned_byte of int (* B *)
  | Table_signed_short of int (* U *)
  | Table_unsigned_short of int (* u *)
  | Table_signed_long of int32 (* I *)
  | Table_unsigned_long of int32 (* i *)
  | Table_signed_longlong of int64 (* L *)
  | Table_unsigned_longlong of int64 (* l *)
  | Table_float of string (* f -- there seems to be no I/O for binary floats? *)
  | Table_double of string (* d -- there seems to be no I/O for binary floats? *)
  | Table_decimal of (int * int32) (* D *)
  | Table_short_string of string (* s *)
  | Table_string of string (* S *)
  | Table_array of table_value_t list (* A *)
  | Table_timestamp of int64 (* T *)
  | Table_table of table_t (* F *)
  | Table_void (* V *)

let read_octet input_buf = Ibuffer.next_byte input_buf
let read_short input_buf =
  let hi = read_octet input_buf in
  let lo = read_octet input_buf in
  (hi lsl 8) lor lo
let read_long input_buf = 
  let hi = Int32.of_int (read_short input_buf) in
  let lo = Int32.of_int (read_short input_buf) in
  Int32.logor (Int32.shift_left hi 16) lo
let read_longlong input_buf =
  let s0 = Int64.of_int (read_short input_buf) in
  let s1 = Int64.of_int (read_short input_buf) in
  let s2 = Int64.of_int (read_short input_buf) in
  let s3 = Int64.of_int (read_short input_buf) in
  Int64.logor
    (Int64.logor (Int64.shift_left s0 48) (Int64.shift_left s1 32))
    (Int64.logor (Int64.shift_left s2 16) s3)
let read_shortstr input_buf =
  let len = read_octet input_buf in
  Ibuffer.next_chars input_buf len
let read_longstr input_buf =
  let len = Int32.to_int (read_long input_buf) in
  Ibuffer.next_chars input_buf len
let read_timestamp input_buf = read_longlong input_buf
let read_table input_buf =
  { table_body = Encoded_table (read_longstr input_buf) }

let
rec decode_named_fields input_buf =
  if Ibuffer.remaining input_buf = 0
  then []
  else
    let s = read_shortstr input_buf in
    let f = read_table_value input_buf in
    (s, f) :: decode_named_fields input_buf

and decode_unnamed_fields input_buf =
  if Ibuffer.remaining input_buf = 0
  then []
  else
    let f = read_table_value input_buf in
    f :: decode_unnamed_fields input_buf

and read_table_value input_buf =
  match Ibuffer.next_char input_buf with
  | 't' -> Table_bool (read_octet input_buf <> 0)
  | 'b' ->
      let v = read_octet input_buf in
      Table_signed_byte (if v >= 128 then v - 256 else v)
  | 'B' -> Table_unsigned_byte (read_octet input_buf)
  | 'U' ->
      let v = read_short input_buf in
      Table_signed_short (if v >= 32768 then v - 65536 else v)
  | 'u' -> Table_unsigned_short (read_short input_buf)
  | 'I' -> Table_signed_long (read_long input_buf)
  | 'i' -> Table_unsigned_long (read_long input_buf)
  | 'L' -> Table_signed_longlong (read_longlong input_buf)
  | 'l' -> Table_unsigned_longlong (read_longlong input_buf)
  | 'f' -> Table_float (Ibuffer.next_chars input_buf 4)
  | 'd' -> Table_double (Ibuffer.next_chars input_buf 8)
  | 'D' ->
      let scale = read_octet input_buf in
      let v = read_long input_buf in
      Table_decimal (scale, v)
  | 's' -> Table_short_string (read_shortstr input_buf)
  | 'S' -> Table_string (read_longstr input_buf)
  | 'A' ->
      let n = Int32.to_int (read_long input_buf) in
      Table_array (decode_unnamed_fields (Ibuffer.next_sub input_buf n))
  | 'T' -> Table_timestamp (read_longlong input_buf)
  | 'F' -> Table_table { table_body = Encoded_table (read_longstr input_buf) }
  | 'V' -> Table_void
  | c -> die 502 (*syntax-error*) (Printf.sprintf "Unknown table field type code '%c'" c)

and decoded_table t =
  match t.table_body with
  | Encoded_table s ->
      let fs = decode_named_fields (Ibuffer.create s 0 (String.length s)) in
      t.table_body <- Both_table (s, fs);
      fs
  | Decoded_table fs -> fs
  | Both_table (_, fs) -> fs

let write_octet output_buf x = Buffer.add_char output_buf (char_of_int x)
let write_char output_buf x = write_octet output_buf (int_of_char x)
let write_short output_buf x =
  write_octet output_buf ((x lsr 8) land 255);
  write_octet output_buf (x land 255)
let write_long output_buf x =
  write_octet output_buf ((Int32.to_int (Int32.shift_right_logical x 24)) land 255);
  write_octet output_buf ((Int32.to_int (Int32.shift_right_logical x 16)) land 255);
  write_octet output_buf ((Int32.to_int (Int32.shift_right_logical x 8)) land 255);
  write_octet output_buf ((Int32.to_int x) land 255)
let write_longlong output_buf x =
  write_octet output_buf ((Int64.to_int (Int64.shift_right_logical x 56)) land 255);
  write_octet output_buf ((Int64.to_int (Int64.shift_right_logical x 48)) land 255);
  write_octet output_buf ((Int64.to_int (Int64.shift_right_logical x 40)) land 255);
  write_octet output_buf ((Int64.to_int (Int64.shift_right_logical x 32)) land 255);
  write_octet output_buf ((Int64.to_int (Int64.shift_right_logical x 24)) land 255);
  write_octet output_buf ((Int64.to_int (Int64.shift_right_logical x 16)) land 255);
  write_octet output_buf ((Int64.to_int (Int64.shift_right_logical x 8)) land 255);
  write_octet output_buf ((Int64.to_int x) land 255)
let write_shortstr output_buf x =
  let len = String.length x in
  write_octet output_buf len;
  Buffer.add_string output_buf x
let write_longstr output_buf x =
  write_long output_buf (Int32.of_int (String.length x));
  Buffer.add_string output_buf x
let write_timestamp output_buf x = write_longlong output_buf x

let
rec encode_named_fields output_buf fs =
  match fs with
    [] -> ()
  | (s, f) :: rest ->
      write_shortstr output_buf s;
      write_table_value output_buf f;
      encode_named_fields output_buf rest

and encode_unnamed_fields output_buf fs =
  match fs with
    [] -> ()
  | f :: rest ->
      write_table_value output_buf f;
      encode_unnamed_fields output_buf rest

and write_table_value output_buf f =
  let wcode c = write_char output_buf c in
  match f with
  | Table_bool true -> wcode 't'; write_octet output_buf 1
  | Table_bool false -> wcode 't'; write_octet output_buf 0
  | Table_signed_byte v -> wcode 'b'; write_octet output_buf (if v < 0 then v + 256 else v)
  | Table_unsigned_byte v -> wcode 'B'; write_octet output_buf v
  | Table_signed_short v -> wcode 'U'; write_short output_buf (if v < 0 then v + 65536 else v)
  | Table_unsigned_short v -> wcode 'u'; write_short output_buf v
  | Table_signed_long v -> wcode 'I'; write_long output_buf v
  | Table_unsigned_long v -> wcode 'i'; write_long output_buf v
  | Table_signed_longlong v -> wcode 'L'; write_longlong output_buf v
  | Table_unsigned_longlong v -> wcode 'l'; write_longlong output_buf v
  | Table_float v -> wcode 'f'; Buffer.add_string output_buf v
  | Table_double v -> wcode 'd'; Buffer.add_string output_buf v
  | Table_decimal (scale, v) -> wcode 'D'; write_octet output_buf scale; write_long output_buf v
  | Table_short_string v -> wcode 's'; write_shortstr output_buf v
  | Table_string v -> wcode 'S'; write_longstr output_buf v
  | Table_array vs ->
      wcode 'A';
      let buf = Buffer.create 1024 in
      encode_unnamed_fields buf vs;
      write_longstr output_buf (Buffer.contents buf)
  | Table_timestamp v -> wcode 'T'; write_longlong output_buf v
  | Table_table t -> wcode 'F'; write_longstr output_buf (encoded_table t)
  | Table_void -> wcode 'V'

and encoded_table t =
  match t.table_body with
  | Encoded_table s -> s
  | Decoded_table fs ->
      let buf = Buffer.create 1024 in
      encode_named_fields buf fs;
      let s = Buffer.contents buf in
      t.table_body <- Both_table (s, fs);
      s
  | Both_table (s, _) -> s

and write_table output_buf x = write_longstr output_buf (encoded_table x)

let sexp_of_octet x = Str (string_of_int x)
let sexp_of_short x = Str (string_of_int x)
let sexp_of_long x = Str (Int32.to_string x)
let sexp_of_longlong x = Str (Int64.to_string x)
let sexp_of_shortstr x = Str x
let sexp_of_longstr x = Str x
let sexp_of_bit x = if x then Str "1" else Str "0"
let sexp_of_timestamp x = Str (Int64.to_string x)

let rec
    sexp_of_table x = Hint {hint = Str "table";
			    body = Arr (List.map sexp_of_named_field (decoded_table x))}
and sexp_of_named_field (s, f) = Arr [Str s; sexp_of_unnamed_field f]
and sexp_of_unnamed_field f =
  match f with
  | Table_bool true -> Str "1"
  | Table_bool false -> Str "0"
  | Table_signed_byte v -> sexp_of_octet (if v < 0 then v + 256 else v)
  | Table_unsigned_byte v -> sexp_of_octet v
  | Table_signed_short v -> sexp_of_short (if v < 0 then v + 65536 else v)
  | Table_unsigned_short v -> sexp_of_short  v
  | Table_signed_long v -> sexp_of_long v
  | Table_unsigned_long v -> Hint {hint = Str "unsigned"; body = sexp_of_long v}
  | Table_signed_longlong v -> sexp_of_longlong v
  | Table_unsigned_longlong v -> Hint {hint = Str "unsigned"; body = sexp_of_longlong v}
  | Table_float v -> Hint {hint = Str "float"; body = Str v}
  | Table_double v -> Hint {hint = Str "double"; body = Str v}
  | Table_decimal (scale, v) -> Hint {hint = Str "decimal";
				      body = Arr [Arr [Str "scale"; sexp_of_octet scale];
						  Arr [Str "value"; sexp_of_long v]]}
  | Table_short_string v -> Str v
  | Table_string v -> Str v
  | Table_array vs -> Hint {hint = Str "array"; body = Arr (List.map sexp_of_unnamed_field vs)}
  | Table_timestamp v -> Hint {hint = Str "timestamp"; body = sexp_of_longlong v}
  | Table_table t -> sexp_of_table t
  | Table_void -> Arr []

let table_of_list fs = { table_body = Decoded_table fs }

let reserved_value_octet = 0
let reserved_value_short = 0
let reserved_value_long = Int32.zero
let reserved_value_longlong = Int64.zero
let reserved_value_shortstr = ""
let reserved_value_longstr = ""
let reserved_value_bit = false
let reserved_value_timestamp = Int64.zero
let reserved_value_table = { table_body = Encoded_table "" }

let field_lookup k def fs =
  try List.assoc k fs
  with Not_found -> def

let field_lookup_some k fs =
  try Some (List.assoc k fs)
  with Not_found -> None

let table_lookup k t = List.assoc k (decoded_table t)
let table_lookup_default k def t = field_lookup k def (decoded_table t)
let table_lookup_some k t = field_lookup_some k (decoded_table t)
