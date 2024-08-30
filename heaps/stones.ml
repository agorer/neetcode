type element =
  | Empty
  | Node of int

let make_heap n = Array.make n Empty

let to_string e =
  match e with
  | Empty -> "-"
  | Node n -> string_of_int n
  
let pp = Array.iter (fun x -> print_string (to_string x); print_string " ")

let size heap = Array.fold_left (fun count e -> if e = Empty then count else count + 1) 0 heap

let left_child_index i = (2 * i) + 1

let has_left_child i heap = (left_child_index i) < (size heap)

let left_child i heap = heap.(left_child_index i)

let right_child_index i = (2 * i) + 2

let has_right_child i heap = (right_child_index i) < (size heap)

let right_child i heap = heap.(right_child_index i)

let has_parent i = i > 0

let parent_index i = int_of_float (ceil ((float_of_int (i - 2)) /. 2.0))

let parent i heap = heap.(parent_index i)

let swap i1 i2 heap =
  let temp = heap.(i1) in
  heap.(i1) <- heap.(i2);
  heap.(i2) <- temp

let peek heap = heap.(0)

let rec order_up i heap =
  if (has_parent i) && (parent i heap) < heap.(i) then
    (swap (parent_index i) i heap;
     order_up (parent_index i) heap)
  else
    ()

let add n heap =
  let last_index = size heap in
  heap.(last_index) <- Node(n);
  order_up last_index heap

let rec order_down i heap =
  if has_left_child i heap then
    let bigger_child_index =
      if (has_right_child i heap) && (right_child i heap) > (left_child i heap) then
        right_child_index i
      else left_child_index i
    in
    if heap.(i) < heap.(bigger_child_index) then
      (swap i bigger_child_index heap;
       order_down bigger_child_index heap)
    else
      ()
  else
    ()
  
let poll heap =
  let item = heap.(0) in
  let size = size heap in
  heap.(0) <- heap.(size - 1);
  heap.(size - 1) <- Empty;
  order_down 0 heap;
  item

let rec load_stones stones heap =
  match stones with
  | [] -> heap
  | h :: rest -> add h heap; load_stones rest heap

let weight stone =
  match stone with
  | Empty -> 0
  | Node n -> n
    
let rec fight_stones heap =
  if has_left_child 0 heap then
    let heavier_stone = heap.(0) in
    let bigger_child_index =
      if (has_right_child 0 heap) && (right_child 0 heap) > (left_child 0 heap) then
        right_child_index 0
      else left_child_index 0
    in
    if heavier_stone = heap.(bigger_child_index) then
      let _, _ = poll heap, poll heap in
      pp heap; print_newline();
      fight_stones heap
    else
      let new_weight = (weight heavier_stone) - (weight heap.(bigger_child_index)) in
      let _, _ = poll heap, poll heap in
      add new_weight heap;
      pp heap; print_newline();
      fight_stones heap
  else
    weight heap.(0)

let () =
  let heap = load_stones [2; 3; 6; 2; 4] (make_heap 20) in
  let weight = fight_stones heap in
  print_string "Last stone weight: "; print_int weight
