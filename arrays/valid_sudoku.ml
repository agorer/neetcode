module TupleSet = Set.Make(struct type t = int * int let compare = compare end)

exception BadInput

let board1 = [
  ["5";"3";".";".";"7";".";".";".";"."];
  ["6";".";".";"1";"9";"5";".";".";"."];
  [".";"9";"8";".";".";".";".";"6";"."];
  ["8";".";".";".";"6";".";".";".";"3"];
  ["4";".";".";"8";".";"3";".";".";"1"];
  ["7";".";".";".";"2";".";".";".";"6"];
  [".";"6";".";".";".";".";"2";"8";"."];
  [".";".";".";"4";"1";"9";".";".";"5"];
  [".";".";".";".";"8";".";".";"7";"9"]] (* true *)

let board2 = [
  ["8";"3";".";".";"7";".";".";".";"."];
  ["6";".";".";"1";"9";"5";".";".";"."];
  [".";"9";"8";".";".";".";".";"6";"."];
  ["8";".";".";".";"6";".";".";".";"3"];
  ["4";".";".";"8";".";"3";".";".";"1"];
  ["7";".";".";".";"2";".";".";".";"6"];
  [".";"6";".";".";".";".";"2";"8";"."];
  [".";".";".";"4";"1";"9";".";".";"5"];
  [".";".";".";".";"8";".";".";"7";"9"]] (* false *)

let to_int str =
  if str = "." then 0 else int_of_string str
      
let rec load_matrix board x matrix =
  match board with
  | [] -> matrix
  | line :: tail ->
    let line = List.map to_int line in
    matrix.(x) <- Array.of_list line;
    load_matrix tail (x + 1) matrix

let toQuad x y =
  if x < 3 && y < 3 then 1
  else if x < 6 && y < 3 then 2
  else if x < 9 && y < 3 then 3
  else if x < 3 && y < 6 then 4
  else if x < 6 && y < 6 then 5
  else if x < 9 && y < 6 then 6
  else if x < 3 && y < 9 then 7
  else if x < 6 && y < 9 then 8
  else if x < 9 && y < 9 then 8
  else raise BadInput

let is_valid_sudoku board =
  let valid = ref true in
  let n = List.length board in
  let board = load_matrix board 0 (Array.make_matrix n n 0) in
  let columns = ref TupleSet.empty in
  let rows = ref TupleSet.empty in
  let quads = ref TupleSet.empty in
  for x = 0 to n - 1 do
    for y = 0 to n - 1 do
      let value = board.(x).(y) in
      let quad = toQuad x y in
      let onColumn = TupleSet.mem (x,value) !columns in
      let onRow = TupleSet.mem (y,value) !rows in
      let onQuad = TupleSet.mem (quad,value) !quads in
      if value = 0 then ()      (* empty squares *)
      else if onColumn || onRow || onQuad then (
        valid := false;
      ) else (
        columns := TupleSet.add (x, value) !columns;
        rows := TupleSet.add (y, value) !rows;
        quads := TupleSet.add (quad, value) !quads
      )
    done
  done;
  !valid

let () =
  let sol1 = is_valid_sudoku board1 in
  let sol2 = is_valid_sudoku board2 in
  Printf.printf "Board 1 = %B\n" sol1;
  Printf.printf "Board 2 = %B\n" sol2
