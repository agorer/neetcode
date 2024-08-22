module CharSet = Set.Make(Char)
    
let example1 = "abcabcbb"       (* 3 *)

let example2 = "bbbbb"          (* 1 *)

let example3 = "pwwkew"         (* 3 *)

let explode s =
  String.to_seq s |> List.of_seq
                       
let longest_substring str =
  let str = explode str in
  let cache = CharSet.empty in
  let rec aux' ~max ~sum ~cache str =
    match str with
    | [] -> max
    | c :: tail ->
      let repeated = CharSet.mem c cache in
      let updated_cache = if repeated then CharSet.empty else CharSet.add c cache in
      let updated_sum = if repeated then 0 else sum + 1 in
      let updated_max = if updated_sum > max then updated_sum else max in
      aux' ~max:updated_max ~sum:updated_sum ~cache:updated_cache tail
  in aux' ~max:0 ~sum:0 ~cache:cache str

let () =
  let sol1 = longest_substring example1 in
  let sol2 = longest_substring example2 in
  let sol3 = longest_substring example3 in
  Printf.printf "Example 1: %d\n" sol1;
  Printf.printf "Example 2: %d\n" sol2;
  Printf.printf "Example 3: %d\n" sol3
