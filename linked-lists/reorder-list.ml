let head1 = [1;2;3;4]
let head2 = [1;2;3;4;5]

exception BadInput
  
let rec reverse ?(reversed=[]) elements =
  match elements with
  | [] -> reversed
  | elt :: tail -> reverse tail ~reversed:(elt :: reversed)

let reorder head =
  let reversed = reverse head in
  let rec aux' head reversed left =
    if left = 0 then []
    else if (List.length head) = (List.length reversed) then
      (List.hd head) :: aux' (List.tl head) reversed (left - 1)
    else
      (List.hd reversed) :: aux' head (List.tl reversed) (left - 1)
  in aux' head reversed (List.length head)

let rec pp_list elements =
  match elements with
  | [] -> print_newline()
  | elt :: tail -> print_int elt; print_char ','; pp_list tail

let () =
  let sol1 = reorder head1 in
  let sol2 = reorder head2 in
  pp_list sol1;
  pp_list sol2
