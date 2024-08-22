module AnagramGroup = Map.Make(String)

let sort s = 
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq |> String.of_seq
                                                                               
let is_anagram s t =
  let s_sorted = sort s in
  let t_sorted = sort t in
  s_sorted = t_sorted

let rec find_anagram samples str =
  match samples with
  | [] -> None
  | sample :: tail ->
    if is_anagram sample str then Some sample
    else find_anagram tail str

let to_list groups =
  AnagramGroup.to_seq groups |> List.of_seq |> List.map (fun (_sample, group) -> group)

let group_anagrams strs =
  let groups = AnagramGroup.empty in
  let samples = [] in
  let rec group' groups samples strs =
    match strs with
    | [] -> to_list groups
    | str :: tail ->
      match find_anagram samples str with
      | None ->
        let groups = AnagramGroup.add str [str] groups in
        let samples = str :: samples in
        group' groups samples tail
      | Some sample ->
        let group = AnagramGroup.find sample groups in
        let updated_group = str :: group in
        let groups = AnagramGroup.add sample updated_group groups in
        group' groups samples tail
  in group' groups samples strs

let example1 = ["eat";"tea";"tan";"ate";"nat";"bat"] (* [["bat"],["nat","tan"],["ate","eat","tea"]] *)

let example2 = [""]             (* [[""]] *)

let example3 = ["a"]            (* [["a"]] *)

let rec pp_groups groups =
  let rec pp_group group =
    match group with
    | [] -> print_char '|'
    | str :: tail -> Printf.printf "%s," str; pp_group tail
  in
  match groups with
  | [] -> print_newline()
  | group :: tail -> pp_group group; pp_groups tail
        
let () =
  let sol1 = group_anagrams example1 in
  let sol2 = group_anagrams example2 in
  let sol3 = group_anagrams example3 in
  pp_groups sol1;
  pp_groups sol2;
  pp_groups sol3;
