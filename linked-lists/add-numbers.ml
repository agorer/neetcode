let a1, b1 = [2;4;3], [5;6;4]
let a2, b2 = [0], [0]
let a3, b3 = [9;9;9;9;9;9;9], [9;9;9;9]

let sum a b =
  let rec aux' a b rem =
    match a,b with
    | [], [] when rem > 0 -> [rem]
    | [], [] -> []
    | [], a :: tail -> ((a + rem) mod 10) :: aux' [] tail ((a + rem) / 10)
    | a :: tail, [] -> ((a + rem) mod 10) :: aux' tail [] ((a + rem) / 10)
    | a :: tail_a, b :: tail_b -> ((a + b + rem) mod 10) :: aux' tail_a tail_b ((a + b + rem) / 10)
  in aux' a b 0

let rec pp_list elements =
  match elements with
  | [] -> print_newline()
  | elt :: tail -> print_int elt; print_char ','; pp_list tail

let () =
  let sol1 = sum a1 b1 in
  let sol2 = sum a2 b2 in
  let sol3 = sum a3 b3 in
  pp_list sol1;
  pp_list sol2;
  pp_list sol3
