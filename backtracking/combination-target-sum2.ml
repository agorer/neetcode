let rec remove_duplicates nums num =
  match nums with
  | h :: rest -> if h <> num then nums else (remove_duplicates rest num)
  | [] -> []

let rec combination_sum nums acc_nums acc target =
  if acc < target then
    match nums with
    | h :: rest -> (combination_sum rest (h::acc_nums) (acc + h) target) @ (combination_sum (remove_duplicates rest h) acc_nums acc target)
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
  (* Only works in sorted arrays, if not sorted we should sort before calling combination_sum *)
  let nums_1, target_1 = [1; 2; 2; 4; 5; 6; 9], 8 in
  let nums_2, target_2 = [1; 2; 3; 4; 5], 7 in
  Printf.printf "%s\n" (pp_combinations (combination_sum nums_1 [] 0 target_1));
  Printf.printf "%s\n" (pp_combinations (combination_sum nums_2 [] 0 target_2))
