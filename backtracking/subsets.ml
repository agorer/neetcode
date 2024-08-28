let rec subsets input acc =
  match input with
  | h :: rest -> (subsets rest (h::acc)) @ (subsets rest acc)
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
  let example_1 = [1; 2; 3] in
  let example_2 = [7] in
  Printf.printf "%s\n" (pp_subsets (subsets example_1 []));
  Printf.printf "%s\n" (pp_subsets (subsets example_2 []))
