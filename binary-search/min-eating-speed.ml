let piles1, h1 = [3;6;7;11], 8  (* 4 *)
let piles2, h2 = [30;11;23;4;20], 5 (* 30 *)
let piles3, h3 = [30;11;23;4;20], 6 (* 23 *)

let rec min_max piles min max =
  match piles with
  | [] -> min, max
  | elt :: tail ->
    let min = if elt < min then elt else min in
    let max = if elt > max then elt else max in
    min_max tail min max

let time piles k =              (* Uses rounded up division *)
  List.fold_left (fun sum elt ->  sum + ((elt + k - 1) / k)) 0 piles 
  
let min piles h =
  let min, max = min_max piles Int.max_int Int.min_int in
  let rec aux' left right =
    let middle = (left + right) / 2 in
    let hours = time piles middle in
    if left >= right then left
    else if hours <= h then aux' left middle
    else aux' (middle + 1) right
  in aux' min max

let () =
  let sol1 = min piles1 h1 in
  let sol2 = min piles2 h2 in
  let sol3 = min piles3 h3 in
  Printf.printf "Sol 1: %d\n" sol1;
  Printf.printf "Sol 2: %d\n" sol2;
  Printf.printf "Sol 3: %d\n" sol3
