let tokens1 = ["2";"1";"+";"3";"*"]
let tokens2 = ["4";"13";"5";"/";"+"]
let tokens3 = ["10";"6";"9";"3";"+";"-11";"*";"/";"*";"17";"+";"5";"+"]

exception BadInput

let is_number s = 
  try int_of_string s |> ignore; true
  with Failure _ -> false

let exec op a b =
  let a = int_of_string a in
  let b = int_of_string b in
  match op with
  | "+" -> string_of_int (a + b)
  | "-" -> string_of_int (a - b)
  | "*" -> string_of_int (a * b)
  | "/" -> string_of_int (a / b)
  | _ -> raise BadInput
           
let rpn tokens =
  let stack = [] in
  let rec aux' tokens stack =
    match tokens with
    | [] -> List.hd stack
    | elt :: tail when is_number elt -> aux' tail (elt :: stack)
    | op :: tail ->
      match stack with
      | b :: a :: stack_tail -> aux' tail ((exec op a b) :: stack_tail)
      | _ -> raise BadInput
  in aux' tokens stack
                                  
  
let () =
  let sol1 = rpn tokens1 in
  let sol2 = rpn tokens2 in
  let sol3 = rpn tokens3 in
  Printf.printf "Sol1: %s\n" sol1;
  Printf.printf "Sol2: %s\n" sol2;
  Printf.printf "Sol3: %s\n" sol3
