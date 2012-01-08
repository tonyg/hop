open Printf

let message_not_understood context m =
  printf "WARNING: Message not understood in %s: " context;
  Sexp.output_sexp stdout (Message.sexp_of_message m);
  print_newline ()

let create_thread name cleanup main initarg =
  let guarded_main initarg =
    try
      main initarg
    with e ->
      printf "WARNING: Thread <<%s>> died with %s\n%!" name (Printexc.to_string e);
      (match cleanup with
      | Some cleaner -> cleaner ()
      | None -> ())
  in
  Thread.create guarded_main initarg
