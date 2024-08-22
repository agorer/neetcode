type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let t1 =
  Node(4,
       Node(2,
            Node(1, Empty, Empty),
            Node(3, Empty, Empty)),
       Node(7,
            Node(6, Empty, Empty),
            Node(9, Empty, Empty)))

let rec invert t =
  match t with
  | Empty -> Empty
  | Node (value, left, right) -> Node (value, (invert right), (invert left))

let rec pp_tree t =
  match t with
  | Empty -> ()
  | Node (value, left, right) -> print_char ','; print_int value; pp_tree left; pp_tree right

let () =
  let sol1 = invert t1 in
  pp_tree t1; print_newline(); pp_tree sol1
