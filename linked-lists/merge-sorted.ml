let l1, o1 = [1;2;4], [1;3;4]
let l2, o2 = [], []
let l3, o3 = [], [0]

let rec merge one other =
  match one, other with
  | [], [] -> []
  | [], e :: tail -> e :: merge [] tail
  | e :: tail, []  -> e :: merge tail []
  | e1 :: one_tail, e2 :: other_tail when e1 <= e2 -> e1 :: merge one_tail (e2 :: other_tail)
  | one, e2 :: other_tail -> e2 :: merge one other_tail

let rec pp_list elements =
  match elements with
  | [] -> print_newline()
  | elt :: tail -> print_int elt; print_char ','; pp_list tail

let () =
  let sol1 = merge l1 o1 in
  let sol2 = merge l2 o2 in
  let sol3 = merge l3 o3 in
  pp_list sol1; print_newline();
  pp_list sol2; print_newline();
  pp_list sol3; print_newline()
