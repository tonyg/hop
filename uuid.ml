type t = string

let create () =
  (* 128 bits *)
  let w1 = Random.bits () in
  let w2 = Random.bits () in
  let w3 = Random.bits () in
  let w4 = Random.bits () in
  let bb = Random.int 256 in
  Printf.sprintf "%08x%08x%08x%08x%02x" w1 w2 w3 w4 bb

let init () =
  Random.self_init ()
