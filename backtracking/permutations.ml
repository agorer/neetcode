let remove nums num =
  List.filter (fun n -> n <> num) nums

let rec permutations nums acc =
  if (List.length nums) = 0 then
    [acc]
  else
    List.map (fun num -> (permutations (remove nums num) (num::acc))) nums
    |> List.fold_left (fun n1 n2 -> n1@n2) []

let pp_surround list_str =
  "[" ^ list_str ^ "]"

let pp_int_list list =
  List.map string_of_int list
  |> String.concat ", "
  |> pp_surround

let pp_permutations permutation =
  List.map pp_int_list permutation
  |> String.concat ", "
  |> pp_surround

let () =
  let nums_1 = [1; 2; 3] in
  let nums_2 = [7] in
  Printf.printf "%s\n" (pp_permutations (permutations nums_1 []));
  Printf.printf "%s\n" (pp_permutations (permutations nums_2 []))
