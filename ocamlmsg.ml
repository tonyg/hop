let hook_log () =
  let old_hook = !Log.hook in
  let new_hook label body =
    ignore (Node.post "system.log" (Sexp.Str label) body (Sexp.Str ""));
    old_hook label body
  in
  Log.hook := new_hook

let _ =
  Printf.printf "%s %s, %s %s\n%!"
    App_info.product App_info.version App_info.copyright App_info.licence;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Uuid.init ();
  Factory.init ();
  Queuenode.init ();
  Fanoutnode.init ();
  Directnode.init ();
  Meta.init ();
  hook_log ();
  Amqp_relay.init ();
  (* Speedtest.init (); *)
  Net.start_net 5671 Relay.start
