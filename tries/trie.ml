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

let rec search word dict =
  match (explode word), dict with
  | _, Empty -> false
  | [], _ -> false
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

let rec starts_with prefix dict =
    match (explode prefix), dict with
  | _, Empty -> false
  | [], _ -> false
  | h :: [], Some node ->
    let last_node = node.children.(index_of h) in
    (match last_node with
     | Empty -> false
     | Some node -> true)
  | h :: rest, Some node ->
    let next_node = node.children.(index_of h) in
    (match next_node with
     | Empty -> false
     | node -> (starts_with (implode rest) node))

let () =
  let t = (empty_node false) |> add "apple" in
  let apple = search "apple" t in
  let app = search "app" t in
  let prefix_app = starts_with "app" t in
  let t = add "app" t in
  let app_2 = search "app" t in
  Printf.printf "%B\n" apple;
  Printf.printf "%B\n" app;
  Printf.printf "%B\n" prefix_app;
  Printf.printf "%B\n" app_2
  

