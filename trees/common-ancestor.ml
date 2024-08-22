type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

exception BadInput

let t1 =
  Node(6,
       Node(2,
            Node(0, Empty, Empty),
            Node(4,
                 Node(3, Empty, Empty),
                 Node(5, Empty, Empty))),
       Node(8,
            Node(7, Empty, Empty),
            Node(9, Empty, Empty)))

let rec search t x =
  match t with
  | Empty -> raise BadInput     (* x not found in t *)
  | Node(y, _, _) when x = y -> y :: []
  | Node(y, left, _)  when y > x -> y :: search left x
  | Node(y, _, right) -> y :: search right x

let rec search_parents ps qs =
  match ps, qs with
  | p :: _, q :: _ when p = q -> p
  | _ :: ps_tail, _ when List.length ps >= List.length qs -> search_parents ps_tail qs
  | _, _ :: qs_tail when List.length ps < List.length qs -> search_parents ps qs_tail
  | _, _ -> raise BadInput

let common_ancestor t p q =
  let p_ancestors = search t p in
  let q_ancestors = search t q in
  search_parents p_ancestors q_ancestors

let () =
  let sol1 = common_ancestor t1 2 8 in
  Printf.printf "Sol1: %d\n" sol1
