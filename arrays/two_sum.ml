let two_sum nums target =
  let sol = ref (-1, -1) in
  for i = 0 to (Array.length nums) - 1 do
    for j = 0 to (Array.length nums) -1 do
      if j <> i && nums.(i) + nums.(j) = target then sol := j,i
      else ()
    done
  done;
  !sol

let nums1, target1 = Array.of_list [2;7;11;15], 9 (* 0, 1 *)

let nums2, target2 = Array.of_list [3;2;4], 6 (* 1, 2 *)

let nums3, target3 = Array.of_list [3;3], 6   (* 0, 1 *)

let () =
  let (i1, j1) = two_sum nums1 target1 in
  let i2, j2 = two_sum nums2 target2 in
  let i3, j3 = two_sum nums3 target3 in
  Printf.printf "Example 1 = %d, %d\n" i1 j1;
  Printf.printf "Example 2 = %d, %d\n" i2 j2;
  Printf.printf "Example 3 = %d, %d\n" i3 j3;
  
