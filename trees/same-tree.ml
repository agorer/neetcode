type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let t1 =
  Node(1,
       Node(2, Empty, Empty),
       Node(3, Empty, Empty))

let t2 =
  Node(1,
       Node(2, Empty, Empty),
       Node(3, Empty, Empty))

let t3 =
  Node(1,
       Node(2, Empty, Empty),
       Empty)

let t4 =
  Node(1,
       Empty,
       Node(2, Empty, Empty))

let rec is_same t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Empty, _ | _, Empty -> false
  | Node(v1, l1, r1), Node(v2, l2, r2) -> v1 = v2 && is_same l1 l2 && is_same r1 r2

let () =
  let sol1 = is_same t1 t2 in
  let sol2 = is_same t3 t4 in
  Printf.printf "Sol1: %B\n" sol1;
  Printf.printf "Sol2: %B\n" sol2

