(* FIXME deduplicate solutions with a better compare function that returns 0 if already exists (e.g. sorting tuple) *)
module Solutions = Set.Make(struct type t = int * int * int let compare = compare end)
    
let example1 = [-1;0;1;2;-1;-4] (* [[-1,-1,2],[-1,0,1]] *)

let example2 = [0;1;1]          (* [] *)

let example3 = [0;0;0]          (* [[0;0;0]] *)

let two_sum nums target =
  let rec aux' nums target a b =
    if a = b then None
    else if a = target then aux' nums target (a + 1) b
    else if b = target then aux' nums target a (b - 1)
    else if nums.(a) + nums.(b) = -nums.(target) then Some (nums.(a), nums.(b), nums.(target))
    else if nums.(a) + nums.(b) > -nums.(target) then aux' nums target a (b - 1)
    else aux' nums target (a + 1) b
  in aux' nums target 0 (Array.length nums - 1)

let three_sum nums =
  let solutions = ref Solutions.empty in
  let nums = List.sort compare nums |> List.to_seq |> Array.of_seq in
  for i = 0 to Array.length nums - 1 do
    match two_sum nums i with
    | None -> ()
    | Some tuple -> solutions := Solutions.add tuple !solutions
  done;
  !solutions

let to_list solutions =
  Solutions.to_seq solutions |> List.of_seq
                                  
let rec pp_sol tuples =
  match tuples with
  | [] -> print_newline()
  | (a,b,c) :: tail -> Printf.printf "%d, %d, %d\n" a b c; pp_sol tail
    
let () =
  let sol1 = three_sum example1 |> to_list in
  let sol2 = three_sum example2 |> to_list in
  let sol3 = three_sum example3 |> to_list in
  pp_sol sol1;
  pp_sol sol2;
  pp_sol sol3
