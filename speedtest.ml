open Sexp

let nmessages = 500000

let receiver ch =
  let low = ref 10000000 in
  let high = ref 0 in
  let sum = ref (Int64.zero) in
  let count = ref 0 in
  while !count < nmessages do
    let m = Squeue.pop ch in
    let now = Unix.gettimeofday () in
    match (Message.message_of_sexp m) with
    | Message.Post (_, Str starttime_str, _) ->
	let starttime = float_of_string starttime_str in
	let delta_us = int_of_float ((now -. starttime) *. 1000000.0) in
	count := !count + 1;
        if delta_us < !low then low := delta_us else ();
	if delta_us > !high then high := delta_us else ();
	sum := Int64.add (!sum) (Int64.of_int delta_us);
    | _ ->
	()
  done;
  Log.info (Printf.sprintf "Stats: low %d us, high %d us, average %d us"
	      (!low)
	      (!high)
	      (Int64.to_int (Int64.div (!sum) (Int64.of_int (!count))))) []

let sender () =
  let ch = Squeue.create 1000 in
  ignore (Util.create_thread "Speed test receiver" None receiver ch);
  let n = Node.make "speedtest" (fun _ m -> Squeue.add m ch) in
  (try
    ignore (Node.bind("speedtest", n));
    let starttime = Unix.gettimeofday () in
    let rec sendloop remaining =
      if remaining = 0
      then ()
      else
	let now = Unix.gettimeofday () in
	Node.post_ignore "speedtest" (Str "test_message") (Str (string_of_float now)) (Str "");
	sendloop (remaining - 1)
    in
    sendloop nmessages;
    let stoptime = Unix.gettimeofday () in
    let delta = stoptime -. starttime in
    Log.info (Printf.sprintf "Speedtest took %d ms (%d Hz)"
		(int_of_float (delta *. 1000.0))
		(int_of_float ((float_of_int nmessages) /. delta))) []
  with exn -> Log.error "Uncaught exception in speedtest" [Str (Printexc.to_string exn)]);
  Node.unbind_all n

let init () =
  ignore (Util.create_thread
	    "Speed test"
	    None
	    sender
	    ())
