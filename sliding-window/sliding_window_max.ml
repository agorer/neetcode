let nums1, k1 = [1;3;-1;-3;5;3;6;7], 3 (* [3;3;5;5;6;7] *)

let nums2, k2 = [1], 1          (* [1] *)

let rec pp_sol nums =
  match nums with
  | [] -> print_newline()
  | num :: tail -> print_int num; print_char ','; pp_sol tail

let max_window nums a b =
  let rec aux' nums a b max =
    if a > b then max
    else
      let n = nums.(a) in
      if n > max then aux' nums (a + 1) b n
      else aux' nums (a + 1) b max
  in aux' nums a b Int.min_int

let max nums k =
  let nums = Array.of_list nums in
  let len = Array.length nums in
  let rec aux' nums a b sol =
    if b = len then sol
    else
      let new_max = max_window nums a b in
      aux' nums (a + 1) (b + 1) (sol @ [new_max])
  in aux' nums 0 (k - 1) []

let () =
  let sol1 = max nums1 k1 in
  let sol2 = max nums2 k2 in
  print_string "Sol1: "; pp_sol sol1;
  print_string "Sol2: "; pp_sol sol2
