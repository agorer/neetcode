type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let t1 =
  Node(1,
       Node(2,
            Node(4, Empty, Empty),
            Node(5, Empty, Empty)),
       Node(3, Empty, Empty))

let t2 =
  Node(1, Node(2, Empty, Empty), Empty)
  

(* Diameter of a node is the max of (left_depth + right_depth), diameter(left) and diameter(right) *)
let diameter t =
  let rec search' t =
    match t with
    | Empty -> 0, 0       (* depth, diameter *)
    | Node (_, left, right) ->
      let left_depth, left_diameter = search' left in
      let right_depth, right_diameter = search' right in
      let depth = (max left_depth right_depth) + 1 in
      let diameter = (left_depth + right_depth) |> max left_diameter |> max right_diameter in
      depth, diameter
  in
  let _, diameter = search' t in
  diameter


let () =
  let sol1 = diameter t1 in
  let sol2 = diameter t2 in
  Printf.printf "Sol1: %d\n" sol1;
  Printf.printf "Sol2: %d\n" sol2
