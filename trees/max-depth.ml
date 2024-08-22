type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let t1 =
  Node(3,
       Node(9, Empty, Empty),
       Node(20,
            Node(15, Empty, Empty),
            Node(7, Empty, Empty)))

let max_depth t =
  let rec max' t depth =
    match t with
    | Empty -> depth
    | Node (_, left, right) ->
      let new_depth = depth + 1 in
      max (max' left new_depth) (max' right new_depth)
  in max' t 0

let () =
  let sol1 = max_depth t1 in
  Printf.printf "Sol1: %d\n" sol1
