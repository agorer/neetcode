let s1, t1 = "anagram", "nagaram" (* true *)

let s2, t2 = "rat", "car"       (* false *)

let sort s = 
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq |> String.of_seq
                                                                               
let is_anagram s t =
  let s_sorted = sort s in
  let t_sorted = sort t in
  s_sorted = t_sorted

let () =
  let sol1 = is_anagram s1 t1 in
  let sol2 = is_anagram s2 t2 in
  Printf.printf "Example 1 = %B\n" sol1;
  Printf.printf "Example 2 = %B\n" sol2
