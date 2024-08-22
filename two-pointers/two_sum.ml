let example1, target1 = [2;7;11;15], 9

let example2, target2 = [2;3;4], 6

let example3, target3 = [-1;0], -1

let two_sum nums target =
  let nums = List.to_seq nums |> Array.of_seq in
  let rec aux' nums target a b =
    if nums.(a) + nums.(b) = target then a + 1, b + 1
    else if nums.(a) + nums.(b) > target then aux' nums target a (b - 1)
    else aux' nums target (a + 1) b
  in aux' nums target 0 (Array.length nums - 1)

let pp_tuple (a, b) =
  Printf.printf "%d, %d\n" a b
    
let () =
  let sol1 = two_sum example1 target1 in
  let sol2 = two_sum example2 target2 in
  let sol3 = two_sum example3 target3 in
  pp_tuple sol1;
  pp_tuple sol2;
  pp_tuple sol3
