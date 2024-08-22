let nums1, target1 = [-1;0;3;5;9;12], 9 (* 4 *)
let nums2, target2 = [-1;0;3;5;9;12], 2 (* -1 *)

let search nums target =
  let nums = Array.of_list nums in
  let rec aux' nums target a b  =
    let pivot = (a + b) / 2 in
    if a = pivot || b = pivot then -1
    else if nums.(pivot) = target then pivot
    else if nums.(pivot) > target then aux' nums target a pivot
    else aux' nums target pivot b
  in aux' nums target 0 (Array.length nums - 1)

let () =
  let sol1 = search nums1 target1 in
  let sol2 = search nums2 target2 in
  Printf.printf "Sol1: %d\n" sol1;
  Printf.printf "Sol2: %d\n" sol2
