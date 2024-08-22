let head1, n1 = [1;2;3;4;5], 2
let head2, n2 = [1], 1
let head3, n3 = [1;2], 1

let remove head n =
  let n = (List.length head) - n in
  let rec aux' head n =
    match head with
    | [] -> []
    | a :: tail when n = 0 -> aux' tail (n-1)
    | a :: tail -> a :: aux' tail (n-1)
  in aux' head n

let rec pp_list elements =
  match elements with
  | [] -> print_newline()
  | elt :: tail -> print_int elt; print_char ','; pp_list tail

let () =
  let sol1 = remove head1 n1 in
  let sol2 = remove head2 n2 in
  let sol3 = remove head3 n3 in
  pp_list sol1;
  pp_list sol2;
  pp_list sol3
