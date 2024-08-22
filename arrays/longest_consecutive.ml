module IntSet = Set.Make(Int)
    
let example1 = [100;4;200;1;3;2] (* 4 *)

let example2 = [0;3;7;2;5;8;4;6;0;1] (* 9 *)

let rec fill set nums =
  match nums with
  | [] -> set
  | num :: tail -> fill (IntSet.add num set) tail

let rec seq_size ?(size=0) set num =
  let previous = num - 1 in
  let next = num + 1 in
  if size = 0 && IntSet.mem previous set then 0
  else if IntSet.mem next set then seq_size set next ~size:(size + 1)
  else size + 1

let rec max_seq set nums =
  let rec aux' set nums current_max =
    match nums with
    | [] -> current_max
    | num :: tail ->
      let num_seq_size = seq_size set num in
      let current_max = max current_max num_seq_size in
      aux' set tail current_max
  in aux' set nums 0

let pp_set set =
  IntSet.to_seq set |> Seq.iter (fun num -> print_int num; print_newline()) |> print_newline
  
let longest_consecutive nums =
  let seen = fill IntSet.empty nums in
  max_seq seen nums

let () =
  let sol1 = longest_consecutive example1 in
  let sol2 = longest_consecutive example2 in
  Printf.printf "Example 1: %d\n" sol1;
  Printf.printf "Example 2: %d\n" sol2
