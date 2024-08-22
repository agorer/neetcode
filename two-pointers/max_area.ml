let example1 = [1;8;6;2;5;4;8;3;7] (* 49 *)

let example2 = [1;1]       (* 1 *)

let area heights a b =
  if b > a then (b - a) * (min heights.(a) heights.(b))
  else 0

let rec reduce heights a b current_max =
  let left_area = area heights (a + 1) b in
  let right_area = area heights a (b - 1) in
  if a = b then current_max
  else if left_area > current_max then reduce heights (a + 1) b left_area
  else if right_area > current_max then reduce heights a (b - 1) right_area
  else if b > a then reduce heights (a + 1) b current_max
  else reduce heights a (b - 1) current_max
            
let max_area heights =
  let heights = Array.of_list heights in
  let a = 0 in
  let b = Array.length heights - 1 in
  reduce heights a b (area heights a b)

let () =
  (* let sol1 = max_area example1 in *)
  let sol2 = max_area example2 in
  (* Printf.printf "Example 1: %d\n" sol1; *)
  Printf.printf "Example 2: %d\n" sol2;
