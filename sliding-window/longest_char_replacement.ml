let example1, k1 = "ABAB", 2    (* 4 *)

let example2, k2 = "AABABBA", 1 (* 4 *)

let example3, k3 = "ABABBBBB", 1 (* 7 *)

let replacement s k =
  let arr = String.to_seq s |> Array.of_seq in
  let rec aux' ~current_char ~max ~sum ~subs ~first_sub ~pos str_array =
    if pos = Array.length str_array then max
    else
      let char = str_array.(pos) in
      let same_char = char = current_char in
      let sum = sum + 1 in
      let first_sub = if first_sub = (-1) && not same_char then pos else first_sub in
      if same_char then
        let max = if sum > max then sum else max in
        aux' ~current_char:char ~max:max ~sum:sum ~subs:subs ~first_sub:first_sub ~pos:(pos+1) str_array
      else if subs = 0 then
        aux' ~current_char:char ~max:max ~sum:0 ~subs:k ~first_sub:(-1) ~pos:first_sub str_array
      else
        let max = if sum > max then sum else max in
        aux' ~current_char:current_char ~max:max ~sum:sum ~subs:(subs-1) ~first_sub:first_sub ~pos:(pos+1) str_array
  in aux' ~current_char:arr.(0) ~max:1 ~sum:1 ~subs:k ~first_sub:(-1) ~pos:1 arr

let () =
  let sol1 = replacement example1 k1 in
  let sol2 = replacement example2 k2 in
  let sol3 = replacement example3 k3 in
  Printf.printf "E1: %d\n" sol1;
  Printf.printf "E2: %d\n" sol2;
  Printf.printf "E3: %d\n" sol3;
