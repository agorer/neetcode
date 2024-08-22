module FrequencyMap = Map.Make(Char)
    
let s1, t1 = "ADOBECODEBANC", "ABC" (* "BANC" *)

let s2, t2 =  "a", "a"          (* "a" *)

let s3, t3 = "a", "aa"          (* "" *)

let rec fill_cache s1 cache =
  match s1 with
  | [] -> cache
  | c :: tail ->
    let found = (FrequencyMap.mem c cache) in
    if found then
      let freq = FrequencyMap.find c cache in
      let updated_freq = freq + 1 in
      fill_cache tail (FrequencyMap.add c updated_freq cache)
    else
      fill_cache tail (FrequencyMap.add c 1 cache)

let min_window s t =
  let s = String.to_seq s |> List.of_seq in
  let t = String.to_seq t |> List.of_seq in
  let t_length = List.length t in
  let cache_orig = FrequencyMap.empty |> fill_cache t in
  let rec aux' ~sol ~current_length ~current_str s cache =
    if current_length = t_length then
      let sol = if sol = "" || String.length current_str < String.length sol then current_str else sol in
      aux' ~sol:sol ~current_length:0 ~current_str:"" s cache_orig
    else
      match s with
      | [] -> sol
      | c :: tail ->
        let found = (FrequencyMap.mem c cache) in
        if found then
          let current_str = current_str ^ String.make 1 c in
          if FrequencyMap.find c cache > 0 then
            let freq = FrequencyMap.find c cache in
            let updated_cache = FrequencyMap.add c (freq - 1) cache in
            aux' ~sol:sol ~current_length:(current_length+1) ~current_str:current_str tail updated_cache
          else
            aux' ~sol:sol ~current_length:current_length ~current_str:current_str tail cache
        else
          let current_str = if current_length = 0 then "" else (current_str ^ String.make 1 c) in
          aux' ~sol:sol ~current_length:current_length ~current_str:current_str tail cache
  in aux' ~sol:"" ~current_length:0 ~current_str:"" s cache_orig
    
let () =
  let sol1 = min_window s1 t1 in
  let sol2 = min_window s2 t2 in
  let sol3 = min_window s3 t3 in
  Printf.printf "Sol1: %s\n" sol1;
  Printf.printf "Sol2: %s\n" sol2;
  Printf.printf "Sol3: %s\n" sol3
