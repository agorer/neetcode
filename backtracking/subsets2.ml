let rec remove_duplicates nums num =
  match nums with
  | h :: rest -> if h <> num then nums else (remove_duplicates rest num)
  | [] -> []

let rec subsets input acc =
  match input with
  | h :: rest -> (subsets rest (h::acc)) @ (subsets (remove_duplicates rest h) acc)
  | [] -> [acc]

let pp_surround list_str =
  "[" ^ list_str ^ "]"

let pp_int_list list =
  List.map string_of_int list
  |> String.concat ", "
  |> pp_surround

let pp_subsets subset =
  List.map pp_int_list subset
  |> String.concat ", "
  |> pp_surround

let () =
  (* Only works in sorted arrays, if not sorted we should sort before calling subsets *)
  let example_1 = [1; 1; 2] in
  let example_2 = [7; 7] in
  Printf.printf "%s\n" (pp_subsets (subsets example_1 []));
  Printf.printf "%s\n" (pp_subsets (subsets example_2 []))
