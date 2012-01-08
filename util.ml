open Sexp
open Printf

let message_not_understood context m =
  Log.warn "Message not understood" [Str context; Message.sexp_of_message m]

let create_thread name cleanup main initarg =
  let guarded_main initarg =
    try
      main initarg
    with e ->
      Log.warn "Thread died with exception" [Str name; Str (Printexc.to_string e)];
      (match cleanup with
      | Some cleaner -> cleaner ()
      | None -> ())
  in
  Thread.create guarded_main initarg

let with_mutex m f arg =
  Mutex.lock m;
  try
    let result = f arg in
    Mutex.unlock m;
    result
  with e ->
    Mutex.unlock m;
    raise e

let with_mutex0 m thunk = with_mutex m thunk ()
