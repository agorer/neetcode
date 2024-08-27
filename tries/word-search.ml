type trie =
  | Empty
  | Some of { is_word: bool; children: trie array }

exception BadInput

let empty_node is_end_of_word = Some({ is_word=is_end_of_word; children=Array.make 26 Empty })

let explode word = List.init (String.length word) (String.get word)

let implode word = String.of_seq (List.to_seq word)

let index_of c = (Char.code c) - 97

let rec add word dict =
  match (explode word), dict with
  | _, Empty -> raise BadInput
  | [], _ -> raise BadInput
  | h :: [], Some node ->
    (match node.children.(index_of h) with
     | Empty ->
       let () = node.children.(index_of h) <- empty_node true in
       dict
     | Some last_node ->
       let () = node.children.(index_of h) <- Some({ last_node with is_word = true }) in
       dict
    )
  | h :: rest, Some node ->
    if node.children.(index_of h) = Empty then
      let () = node.children.(index_of h) <- empty_node false in
      let _ = add (implode rest) node.children.(index_of h) in
      dict
    else
      let _ = add (implode rest) node.children.(index_of h) in
      dict

let check_child child =
  match child with
  | Empty -> false
  | Some node -> if node.is_word then true else false

let check_children children =
  Array.map check_child children
  |> Array.fold_left (fun acc other -> acc || other) false

let rec search word dict =
  match (explode word), dict with
  | _, Empty -> false
  | [], _ -> false
  | '.' :: [], Some node ->
    check_children node.children
  | '.' :: rest, Some node ->
    Array.map (search (implode rest)) node.children
    |> Array.fold_left (fun acc other -> acc || other) false
  | h :: [], Some node ->
    let last_node = node.children.(index_of h) in
    (match last_node with
     | Empty -> false
     | Some node -> if node.is_word then true else false)
  | h :: rest, Some node ->
    let next_node = node.children.(index_of h) in
    (match next_node with
     | Empty -> false
     | node -> (search (implode rest) node))
    
let () =
  let dict = (empty_node false)
          |> add "day"
          |> add "bay"
          |> add "may" in
  let search_1 = search "say" dict in
  let search_2 = search "day" dict in
  let search_3 = search ".ay" dict in
  let search_4 = search "b.." dict in
  let search_5 = search "b.o" dict in
  Printf.printf "%B\n" search_1;
  Printf.printf "%B\n" search_2;
  Printf.printf "%B\n" search_3;
  Printf.printf "%B\n" search_4;
  Printf.printf "%B\n" search_5
  
