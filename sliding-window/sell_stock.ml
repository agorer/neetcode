let example1 = [7;1;5;3;6;4]    (* 5 *)

let example2 = [7;6;4;3;1]      (* 0 *)

let rec max_profit prices  =
  let first = List.hd prices in
  let rec aux' ~current_max ~current_sum ~previous prices =
    match prices with
    | [] -> if current_max > 0 then current_max else 0
    | price :: tail ->
      let value = price - previous in
      let sum = current_sum + value in
      let updated_sum = if sum > 0 then sum else 0 in
      let updated_max = if updated_sum > current_max then updated_sum else current_max in
      aux' ~current_max:updated_max ~current_sum:updated_sum ~previous:price tail
  in aux' ~current_max:0 ~current_sum:0 ~previous:first prices

let () =
  let sol1 = max_profit example1 in
  let sol2 = max_profit example2 in
  Printf.printf "Example 1: %d\n" sol1;
  Printf.printf "Example 2: %d\n" sol2
