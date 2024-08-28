let rec combination_sum nums acc_nums acc target =
  if acc < target then
    match nums with
    | h :: rest -> (combination_sum nums (h::acc_nums) (acc + h) target) @ (combination_sum rest acc_nums acc target)
    | [] -> []
  else if acc > target then
    []
  else
    [acc_nums]

let pp_surround list_str =
  "[" ^ list_str ^ "]"

let pp_int_list list =
  List.map string_of_int list
  |> String.concat ", "
  |> pp_surround

let pp_combinations combination =
  List.map pp_int_list combination
  |> String.concat ", "
  |> pp_surround
  
let () =
  let nums_1, target_1 = [2; 5; 6; 9], 9 in
  let nums_2, target_2 = [3; 4; 5], 16 in
  Printf.printf "%s\n" (pp_combinations (combination_sum nums_1 [] 0 target_1));
  Printf.printf "%s\n" (pp_combinations (combination_sum nums_2 [] 0 target_2))
