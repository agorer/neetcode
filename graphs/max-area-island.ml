let rec travel_island map visited x y =
  if x >= 0 && x < 5 && y >= 0 && y < 4 && not visited.(y).(x) then
    let () = visited.(y).(x) <- true in
    if map.(y).(x) = 1 then
      1
      + (travel_island map visited x (y+1))
      + (travel_island map visited x (y-1))
      + (travel_island map visited (x+1) y)
      + (travel_island map visited (x-1) y)
    else
      0
  else
    0

let max_area map =
  let visited = Array.make_matrix 4 5 false in
  let max_area = ref 0 in
  for x = 0 to 4 do
    for y = 0 to 3 do
      if map.(y).(x) = 1 && not visited.(y).(x) then
        max_area := max !max_area (travel_island map visited x y)
      else
        ()
    done;
  done;
  !max_area

let () =
  let map1 = [|
    [|0; 1; 1; 0; 1|];
    [|1; 0; 1; 0; 1|];
    [|0; 1; 1; 0; 1|];
    [|0; 1; 0; 0; 1|]|] in
  print_int (max_area map1); print_newline();
