#use "fqueue.ml"

let q12 = append (singleton 1) (singleton 2)
let q1234 = append (of_list [1; 2]) (of_list [3; 4])

let _ = 
  assert (length empty = 0);
  assert (is_empty empty);
  assert (pop_front (push_back empty 1) = Some (1, empty));
  assert (pop_back (push_front empty 1) = Some (1, empty));
  assert (to_list (of_list [1; 2; 3]) = [1; 2; 3]);
  assert (length (of_list [1; 2; 3]) = 3);
  assert (pop_back (of_list [1; 2; 3]) = Some (3, Q(2, [], [2; 1])));
  assert (pop_front (of_list [1; 2; 3]) = Some (1, of_list [2; 3]));
  assert (to_list (push_back_all empty [1; 2; 3]) = [1; 2; 3]);
  assert (to_list (push_front_all empty [1; 2; 3]) = [1; 2; 3]);
  assert (to_list (push_back_all_rev empty [1; 2; 3]) = [3; 2; 1]);
  assert (to_list (push_front_all_rev empty [1; 2; 3]) = [3; 2; 1]);
  assert ((try slow_peek_back empty with _ -> 1) = 1);
  assert ((try slow_peek_front empty with _ -> 1) = 1);
  assert ((try slow_peek_back (singleton 1) with _ -> 2) = 1);
  assert ((try slow_peek_front (singleton 1) with _ -> 2) = 1);
  assert ((try slow_peek_back (of_list [1; 2; 3]) with _ -> 99) = 3);
  assert ((try slow_peek_front (of_list [1; 2; 3]) with _ -> 99) = 1);
  assert (to_list_rev (of_list [1; 2; 3]) = [3; 2; 1]);
  assert (to_list (of_list_rev [1; 2; 3]) = [3; 2; 1]);
  assert (to_list_rev (of_list_rev [1; 2; 3]) = [1; 2; 3]);
  assert (map ((+) 1) (of_list [1; 2; 3]) = of_list [2; 3; 4]);
  assert (to_list q12 = [1; 2]);
  assert (fold_left (fun s v -> v :: s) [] q1234 = [4; 3; 2; 1]);
  assert (fold_right (fun v s -> v :: s) [] q1234 = [1; 2; 3; 4]);
  assert (fold_left (fun s v -> v :: s) [] q12 = [2; 1]);
  assert (fold_right (fun v s -> v :: s) [] q12 = [1; 2]);
