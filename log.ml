open Sexp

let mtx = Mutex.create ()
let write_to_log label body =
  Mutex.lock mtx;
  (try
    print_string label;
    print_string ": ";
    output_sexp_human stdout body;
    print_newline ()
  with _ -> ());
  Mutex.unlock mtx

let hook = ref write_to_log

let info message args = (!hook) "info" (Arr (Str message :: args))
let warn message args = (!hook) "warn" (Arr (Str message :: args))
