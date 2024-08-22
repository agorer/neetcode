let example1 = "amanaplanacanalpanama"

let example2 = "raceacar"
  

let is_valid str =
  let str = String.to_seq str |> Array.of_seq in
  let rec aux' str a b =
    if a = b then true
    else if str.(a) <> str.(b) then false
    else aux' str (a + 1) (b - 1)
  in aux' str 0 (Array.length str - 1)

let () =
  let sol1 = is_valid example1 in
  let sol2 = is_valid example2 in
  Printf.printf "Example 1: %B\n" sol1;
  Printf.printf "Example 2: %B\n" sol2
