let nums1, target1 = [[1;3;5;7];[10;11;16;20];[23;30;34;60]], 3 (* true *)
let nums2, target2 = [[1;3;5;7];[10;11;16;20];[23;30;34;60]], 13 (* false *)

let rec to_array nums =
  match nums with
  | [] -> []
  | row :: tail -> (Array.of_list row) :: to_array tail

let search_row nums target =
  let rec aux' nums target a b  =
    let pivot = (a + b) / 2 in
    if a = pivot || b = pivot then false
    else if nums.(pivot) = target then true
    else if nums.(pivot) > target then aux' nums target a pivot
    else aux' nums target pivot b
  in aux' nums target 0 (Array.length nums - 1)

let contains row target =
  target >= row.(0) && target <= row.(Array.length row - 1)
                                   
let search nums target =
  let nums = Array.of_list (to_array nums) in
  let rec aux' nums target a b =
    let pivot = (a + b) / 2 in
    if contains nums.(pivot) target then search_row nums.(pivot) target
    else if target > nums.(pivot).(0) then aux' nums target pivot b
    else aux' nums target a pivot
  in aux' nums target 0 (Array.length nums - 1)
  

let () =
  let sol1 = search nums1 target1 in
  let sol2 = search nums2 target2 in
  Printf.printf "Sol1: %B\n" sol1;
  Printf.printf "Sol2: %B\n" sol2
