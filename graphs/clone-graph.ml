(* We are using a node array even though is memory inefficient for ease of implementation *)
(* For more speed a list or a Hashtbl. could be used *)
(* We also have the limitation of 10 edges per vertex *)
type node =
  | Empty
  | Node of (int * node array ref)

let pp_edge edge =
  print_string "";
  match edge with
  | Empty -> print_string "-"
  | Node(value, _) -> print_int value
  
let pp_edges edges =
  print_string "[";
  let _ = Array.map (fun edge -> pp_edge edge; edge) edges in
  print_string "]";
  print_newline()

let empty_edges n =
  ref (Array.make n Empty)

let rec clone g visited =
  match g with
  | Empty -> Empty
  | Node(value, edges) ->
    let cached_node = Hashtbl.find_opt visited value in
    if cached_node <> None then
      Option.get cached_node
    else
      let new_edges = empty_edges 10 in
      let new_node = Node(value, new_edges) in
      Hashtbl.add visited value new_node;
      new_edges := Array.map (fun edge -> clone edge visited) !edges;
      new_node

let add_edge node child =
  match node, child with
  | Empty, _ -> ()
  | _, Empty -> ()
  | Node(_, edges), Node(value, _) ->
    !edges.(value) <- child

let () =
  let visited = Hashtbl.create 100 in
  let node1 = Node(1, empty_edges 10) in
  let node2 = Node(2, empty_edges 10) in
  let node3 = Node(3, empty_edges 10) in
  add_edge node1 node2;
  add_edge node2 node1;
  add_edge node2 node3;
  add_edge node3 node2;
  let _ = clone node1 visited in
  ()
