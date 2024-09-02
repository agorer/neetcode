type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree

let rec bfs q =
  if Queue.is_empty q then
    ()
  else
    match Queue.pop q with
    | Leaf -> bfs q
    | Node (value, left, right) ->
      Queue.add left q;
      Queue.add right q;
      print_int value; print_newline();
      bfs q

let () =
  let t = Node(1,
               Node(2,
                    Node(3, Leaf, Leaf),
                    Node(4, Leaf, Leaf)),
               Node(5,
                    Node(6, Leaf, Leaf),
                    Node(7, Leaf, Leaf))) in
  let q = Queue.create() in
  Queue.push t q;
  bfs q
