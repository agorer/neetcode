let elements = [1;2;3;4;5]

let rec reverse elements reversed =
  match elements with
  | [] -> reversed
  | elt :: tail -> reverse tail (elt :: reversed)

let rec pp_list elements =
  match elements with
  | [] -> print_newline()
  | elt :: tail -> print_int elt; print_char ','; pp_list tail

let () =
  pp_list (reverse elements [])
