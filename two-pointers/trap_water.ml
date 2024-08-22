let example1 = [0;1;0;2;1;0;1;3;2;1;2;1] (* 6 *)

let example2 = [4;2;0;3;2;5]    (* 9 *)

let trap height =
  let totalWater = ref 0 in
  let height = Array.of_list height in
  let left, right = ref 1, ref (Array.length height - 2) in
  let maxLeft, maxRight = ref height.(0), ref height.(Array.length height - 1) in
  while !left <= !right do
    if height.(!left) >= !maxLeft then
      (maxLeft := height.(!left);
       left := !left + 1)
    else if height.(!right) >= !maxRight then
      (maxRight := height.(!right);
       right := !right - 1)
    else if !maxLeft <= !maxRight && height.(!left) < !maxLeft then
      let newWater = !maxLeft - height.(!left) in
      (totalWater := !totalWater + newWater;
       left := !left + 1)
    else
      let newWater = !maxRight - height.(!right) in
      (totalWater := !totalWater + newWater;
       right := !right - 1)
  done;
  !totalWater

let () =
  let sol1 = trap example1 in
  let sol2 = trap example2 in
  Printf.printf "Example 1: %d\n" sol1;
  Printf.printf "Example 2: %d\n" sol2;
