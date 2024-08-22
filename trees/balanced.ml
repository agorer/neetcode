type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let t1 =
  Node(3,
       Node(9, Empty, Empty),
       Node(20,
            Node(15, Empty, Empty),
            Node(7, Empty, Empty)))

let t2 =
  Node(1,
       Node(2,
            Node(3,
                 Node(4, Empty, Empty),
                 Node(4, Empty, Empty)),
            Node(3, Empty, Empty)),
       Node(2, Empty, Empty))


let t3 = Empty

let is_balanced t =
  let rec check' t =
    match t with
    | Empty -> 0, true          (* height, balanced *)
    | Node(_, left, right) ->
      let left_height, left_balanced = check' left in
      let right_height, right_balanced = check' right in
      let height = (max left_height right_height) + 1 in
      let balanced = left_balanced && right_balanced && (Int.abs (left_height - right_height)) <= 1 in
      height, balanced
  in
  let _, balanced = check' t in
  balanced

let () =
  let sol1 = is_balanced t1 in
  let sol2 = is_balanced t2 in
  let sol3 = is_balanced t3 in
  Printf.printf "Sol1: %B\n" sol1;
  Printf.printf "Sol2: %B\n" sol2;
  Printf.printf "Sol3: %B\n" sol3;
    
