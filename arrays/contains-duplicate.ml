let example1 = [1;2;3;1]        (* true *)

let example2 = [1,2,3,4]        (* false *)

let example3 = [1;1;1;3;3;4;3;2;4;2] (* true *)

let contains_duplicate nums =
  let sorted_nums = List.sort compare nums in
  let rec dups' nums =
    match nums with
    | [] -> false
    | a :: b :: tail when a = b -> true
    | a :: tail -> dups' tail
  in dups' sorted_nums

let () =
  let e1 = contains_duplicate example1 in
  let e2 = contains_duplicate example2 in
  let e3 = contains_duplicate example3 in
  Printf.printf "Example 1 = %B \n" e1;
  Printf.printf "Example 2 = %B \n" e2;
  Printf.printf "Example 3 = %B \n" e3
