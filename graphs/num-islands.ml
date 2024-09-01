(* Hardcoded height and width of the matrix, if needed could be parametrized *)

let rec travel_island map visited x y =
  (* print_string "Traveling: "; print_int x; print_string " "; print_int y; print_newline(); *)
  if x >= 0 && x < 5 && y >= 0 && y < 4 && not visited.(y).(x) then
    let () = visited.(y).(x) <- true in
    if map.(y).(x) = 1 then
       (travel_island map visited x (y+1);
        travel_island map visited x (y-1);
        travel_island map visited (x+1) y;
        travel_island map visited (x-1) y)
    else
      ()
  else
    ()

let num_islands map =
  let visited = Array.make_matrix 4 5 false in
  let num_islands = ref 0 in
  for x = 0 to 4 do
    for y = 0 to 3 do
      if map.(y).(x) = 1 && not visited.(y).(x) then
        (num_islands := !num_islands + 1;
         (* print_string "Island: "; print_int x; print_string " "; print_int y; print_newline(); *)
         travel_island map visited x y)
      else
        ()
    done;
  done;
  !num_islands

let () =
  let map1 = [|
    [|0; 1; 1; 1; 0|];
    [|0; 1; 0; 1; 0|];
    [|1; 1; 0; 0; 0|];
    [|0; 0; 0; 0; 0|]|] in
  let map2 = [|
    [|1; 1; 0; 0; 1|];
    [|1; 1; 0; 1; 1|];
    [|0; 0; 1; 0; 0|];
    [|0; 0; 0; 1; 1|]|] in
  print_int (num_islands map1); print_newline();
  print_int (num_islands map2); print_newline();
  
