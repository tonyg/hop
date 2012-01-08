(* Functional queue. *)

type 'a t = Q of int * 'a list * 'a list

let empty = Q (0, [], [])

let singleton v = Q (1, [], [v])

let length (Q (n, _, _)) = n

let is_empty q = (length q = 0)

let push_back (Q (n, front, back)) v = Q (n + 1, front, v :: back)
let push_front (Q (n, front, back)) v = Q (n + 1, v :: front, back)

let push_back_all (Q (n, front, back)) vs = Q (n + List.length vs, front, List.rev_append vs back)
let push_front_all (Q (n, front, back)) vs = Q (n + List.length vs, List.append vs front, back)

let push_back_all_rev (Q (n, front, back)) vs =
  Q (n + List.length vs, front, List.append vs back)
let push_front_all_rev (Q (n, front, back)) vs =
  Q (n + List.length vs, List.rev_append vs front, back)

let pop_ remote local =
  match local with
  | [] ->
      (match List.rev remote with
      | [] -> None
      | v :: rest -> Some (v, [], rest))
  | v :: rest -> Some (v, remote, rest)

let pop_back (Q (n, front, back)) =
  match pop_ front back with
  | Some (v, front', back') -> Some (v, Q (n - 1, front', back'))
  | None -> None

let pop_front (Q (n, front, back)) =
  match pop_ back front with
  | Some (v, back', front') -> Some (v, Q (n - 1, front', back'))
  | None -> None

let peek_back (Q (n, front, back)) =
  match pop_ front back with
  | Some (v, front', back') -> Some (v, Q (n - 1, front', v :: back'))
  | None -> None

let peek_front (Q (n, front, back)) =
  match pop_ back front with
  | Some (v, back', front') -> Some (v, Q (n - 1, v :: front', back'))
  | None -> None

let unsome x =
  match x with
  | Some v -> v
  | None -> raise Not_found

let really_pop_back q = unsome (pop_back q)
let really_pop_front q = unsome (pop_front q)

let really_peek_back q = unsome (peek_back q)
let really_peek_front q = unsome (peek_front q)

let slow_peek_back q = let (v, _) = unsome (peek_back q) in v
let slow_peek_front q = let (v, _) = unsome (peek_front q) in v

let of_list vs = Q (List.length vs, vs, [])
let of_list_rev vs = Q (List.length vs, [], vs)

let to_list (Q (_, front, back)) = List.append front (List.rev back)
let to_list_rev (Q (_, front, back)) = List.append back (List.rev front)

(* Warning: doesn't operate in order *)
let map f (Q (n, front, back)) = Q (n, List.map f front, List.rev_map f back)

let append (Q (n1, front1, back1)) (Q (n2, front2, back2)) =
  Q (n1 + n2, front1 @ List.rev_append back1 front2, back2)

let iter f (Q (_, front, back)) =
  List.iter f front;
  List.iter f (List.rev back)

let fold_left f seed (Q (_, front, back)) =
  List.fold_right (fun v s -> f s v) back (List.fold_left f seed front)

let fold_right f seed (Q (_, front, back)) =
  List.fold_right f front (List.fold_left (fun s v -> f v s) seed back)
