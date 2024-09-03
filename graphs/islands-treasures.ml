let pp_map map =
  let max_y = (Array.length map) - 1 in
  let max_x = (Array.length map.(0)) - 1 in
  for y = 0 to max_y do
    for x = 0 to max_x do
      print_int map.(y).(x); print_string " "
    done;
    print_newline()
  done;
  print_string "----"; print_newline()

let inf = Int.max_int - 1

let rec bfs map q =
  if Queue.is_empty q then
    ()
  else
    let max_y = (Array.length map) - 1 in
    let max_x = (Array.length map.(0)) - 1 in
    let x, y, distance = Queue.pop q in
    if x < 0 || x > max_x || y < 0 || y > max_y || map.(y).(x) = -1 then
      bfs map q
    else if map.(y).(x) < distance then
      bfs map q
    else
      (Queue.push (x+1, y, distance+1) q; (* The number of calls can be reduced if checking x,y before pushing to the queue *)
       Queue.push (x-1, y, distance+1) q;
       Queue.push (x, y+1, distance+1) q;
       Queue.push (x, y-1, distance+1) q;
       map.(y).(x) <- distance;
       bfs map q)

let find_distances map =
  let max_y = (Array.length map) - 1 in
  let max_x = (Array.length map.(0)) - 1 in
  let q = Queue.create() in
  for y = 0 to max_y do
    for x = 0 to max_x do
      if map.(y).(x) = 0 then
        Queue.push (x, y, 0) q
    done;
  done;
  bfs map q;
  map

let () =
  let map1 = [|
    [| inf;  -1;   0;  inf |];
    [| inf; inf; inf;   -1 |];
    [| inf;  -1; inf;   -1 |];
    [| 0;    -1; inf;  inf |]
  |] in
  let map2 = [|
    [| 0;   -1 |];
    [| 1;  inf |]
  |] in
  pp_map (find_distances map1);
  pp_map (find_distances map2)
