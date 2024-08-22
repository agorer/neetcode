let example1 = ["lint";"code";"love";"you"]

let example2 = ["we"; "say"; ":"; "yes"]

let rec encode strs =
  match strs with
  | [] -> ""
  | str :: tail -> str ^ ":;" ^ encode tail

let explode s =
  String.to_seq s |> List.of_seq

let decode input =
  let output = [""] in
  let rec aux' input output =
    match input with
    | [] -> output
    | ':' :: ';' :: tail ->
      let current = List.hd output in
      let rest = List.tl output in
      aux' tail ("" :: rest @ [current])
    | a :: tail ->
      let current = (List.hd output) ^ (String.make 1 a) in
      let rest = List.tl output in
      aux' tail (current :: rest)
  in aux' input output

let rec pp_sol sol =
  match sol with
  | [] -> print_newline()
  | n :: tail -> print_string n; print_char ','; pp_sol tail

let () =
  let sol1 = encode example1 |> explode |> decode in
  let sol2 = encode example2 |> explode |> decode in
  pp_sol sol1;
  pp_sol sol2
