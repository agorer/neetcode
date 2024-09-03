let empty = 0

let fresh = 1

let rotten = 2

let valid_coordinates grid x y =
  let max_y = (Array.length grid) - 1 in
  let max_x = (Array.length grid.(0) - 1) in
  not (x < 0 || x > max_x || y < 0 || y > max_y)

let rot grid q x y fresh_fruit minute =
  grid.(y).(x) <- rotten;
  Queue.push (x, y, minute) q;
  fresh_fruit := !fresh_fruit - 1

let rec bfs grid q fresh_fruit minute =
  if Queue.is_empty q then
    if !fresh_fruit = 0 then minute else -1
  else
    let x, y, minute = Queue.pop q in
    (if (valid_coordinates grid (x+1) y) && grid.(y).(x+1) = fresh then rot grid q (x+1) y fresh_fruit (minute+1);
     if (valid_coordinates grid (x-1) y) && grid.(y).(x-1) = fresh then rot grid q (x-1) y fresh_fruit (minute+1);
     if (valid_coordinates grid x (y+1)) && grid.(y+1).(x) = fresh then rot grid q x (y+1) fresh_fruit (minute+1);
     if (valid_coordinates grid x (y-1)) && grid.(y-1).(x) = fresh then rot grid q x (y-1) fresh_fruit (minute+1);
     bfs grid q fresh_fruit minute
    )

let simulate grid q =
  let max_y = (Array.length grid) - 1 in
  let max_x = (Array.length grid.(0) - 1) in
  let fresh_fruit = ref 0 in
  for x = 0 to max_x do
    for y = 0 to max_y do
      if grid.(y).(x) = rotten then Queue.push (x, y, 0) q;
      if grid.(y).(x) = fresh then fresh_fruit := !fresh_fruit + 1
    done;
  done;
  bfs grid q fresh_fruit 0
  
let () =
  let grid = [|
    [| fresh; fresh; empty |];
    [| empty; fresh; fresh |];
    [| empty; fresh; rotten |];
  |] in
  let grid2 = [|
    [| fresh; empty; fresh |];
    [| empty; rotten; empty |];
    [| fresh; empty; fresh |];
  |] in
  let q = Queue.create() in
  print_int (simulate grid q); print_newline();
  print_int (simulate grid2 q)
