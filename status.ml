type ('success, 'failure) t =
  | Ok of 'success
  | Problem of 'failure

type ('transient, 'permanent) failure_t =
  | Transient of 'transient
  | Permanent of 'permanent

let is_ok x =
  match x with
  | Ok _ -> true
  | Problem _ -> false

let is_problem x = not (is_ok x)

let is_transient x =
  match x with
  | Transient _ -> true
  | Permanent _ -> false

let is_permanent x = not (is_transient x)

let replace_ok x info =
  match x with
  | Ok _ -> Ok info
  | Problem p -> Problem p

let replace_ok' x info_fn =
  match x with
  | Ok _ -> Ok (info_fn ())
  | Problem p -> Problem p
