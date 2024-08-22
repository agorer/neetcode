module IntMap = Map.Make(Int)

let freq num freqs =
  if IntMap.mem num freqs then
    (IntMap.find num freqs) + 1
  else 1

let rec count_freqs freqs nums =
  match nums with
  | [] -> freqs
  | num :: tail ->
    let updated_freq = freq num freqs in
    let freqs = IntMap.add num updated_freq freqs in
    count_freqs freqs tail

let top_k k freqs =
  let top = [] in
  let rec tops' k top freqs =
    match k with
    | 0 -> top
    | _ ->
      let num = List.hd freqs in
      let tail = List.tl freqs in
      tops' (k - 1) (num :: top) tail
  in tops' k top freqs
  
let top_k_frequent nums k =
  let freqs = IntMap.empty in
  count_freqs freqs nums
  |> IntMap.to_seq |> List.of_seq
  |> List.sort compare
  |> List.map (fun a -> fst a)
  |> top_k k

let nums1, k1 = [1;1;1;2;2;3], 2 (* [1,2] *)

let nums2, k2 = [1], 1          (* [1] *)

let rec pp_sol sol =
  match sol with
  | [] -> print_newline()
  | n :: tail -> print_int n; print_char ','; pp_sol tail
  
let () =
  let sol1 = top_k_frequent nums1 k1 in
  let sol2 = top_k_frequent nums2 k2 in
  pp_sol sol1;
  pp_sol sol2
