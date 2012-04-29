open Sexp

let rec sexp_of_json j =
  match j with
  | Json.Num f -> Hint {hint = Str "num"; body = Str (Json.to_string j)}
  | Json.Str s -> Str s
  | Json.Arr js -> Arr (List.map sexp_of_json js)
  | Json.Rec kvs -> Hint {hint = Str "obj";
			  body = Arr (List.map (fun (k, v) -> Arr [Str k; sexp_of_json v]) kvs)}
  | Json.Flg f -> Hint {hint = Str "bool"; body = Str (string_of_bool f)}
  | Json.Nil -> Hint {hint = Str "null"; body = Arr []}

let rec json_of_sexp x =
  match x with
  | Hint {hint = Str "num"; body = Str n} -> Json.Num (float_of_string n)
  | Str s -> Json.Str s
  | Arr xs -> Json.Arr (List.map json_of_sexp xs)
  | Hint {hint = Str "obj"; body = Arr kvs} ->
      Json.Rec (List.map
		  (fun kv ->
		    (match kv with
		    | Arr [Str k; v] -> (k, json_of_sexp v)
		    | _ -> syntax_error "Bad JSON-SEXP key-value"))
		  kvs)
  | Hint {hint = Str "bool"; body = Str bs} -> Json.Flg (bool_of_string bs)
  | Hint {hint = Str "null"; body = Arr []} -> Json.Nil
  | Hint {hint = h; body = b} -> Json.Rec ["_hint", json_of_sexp h; "_body", json_of_sexp b]
