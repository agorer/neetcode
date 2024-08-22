(* O(n) solution would be multiply all (except one of the zeros) and then divide by each position (n + n) *)
let product_except_self nums =
  let nums = Array.of_list nums in
  let output = Array.make (Array.length nums) 1 in
  for i = 0 to Array.length nums - 1 do
    for j = 0 to Array.length nums - 1 do
      if i <> j then output.(i) <- output.(i) * nums.(j)
      else ()
    done
  done;
  output

let rec pp_sol sol =
  match sol with
  | [] -> print_newline()
  | n :: tail -> print_int n; print_char ','; pp_sol tail

let nums1 = [1;2;3;4]           (* [24,12,8,6] *)

let nums2 = [-1;1;0;-3;3]       (* [0,0,9,0,0] *)

let () =
  let sol1 = product_except_self nums1 in
  let sol2 = product_except_self nums2 in
  pp_sol (Array.to_list sol1);
  pp_sol (Array.to_list sol2)
    
