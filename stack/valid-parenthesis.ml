let s1 = "()"                   (* true *)
let s2 = "()[]{}"               (* true *)
let s3 = "(]"                   (* false *)

let is_opener c =
  c = '(' || c = '[' || c = '{'

exception BadInput

let opener_of c =
  match c with
  | ')' -> '('
  | ']' -> '['
  | '}' -> '{'
  | _ -> raise BadInput

let is_valid s =
  let s = String.to_seq s |> List.of_seq in
  let cache = [] in
  let rec aux' s cache =
    match s with
    | [] -> cache = []
    | c :: tail when is_opener c -> aux' tail (c :: cache)
    | c :: tail ->
      match cache with
      | [] -> false
      | c2 :: cache_tail -> if c2 = opener_of c then aux' tail cache_tail else false
  in aux' s cache

let () =
  let sol1 = is_valid s1 in
  let sol2 = is_valid s2 in
  let sol3 = is_valid s3 in
  Printf.printf "Sol 1: %B\n" sol1;
  Printf.printf "Sol 2: %B\n" sol2;
  Printf.printf "Sol 3: %B\n" sol3

