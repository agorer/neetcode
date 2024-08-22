module FrequencyMap = Map.Make(Char)

let s1_1, s1_2 = "ab", "eidbaooo"   (* true *)

let s2_1, s2_2 = "ab", "eidboaoo" (* false *)

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

let permutation s1 s2 =
  let s1 = String.to_seq s1 |> List.of_seq in
  let s1_length = List.length s1 in
  let s2 = String.to_seq s2 |> List.of_seq in
  let cache_orig = FrequencyMap.empty |> fill_cache s1 in
  let rec aux' current s2 cache =
    if current = s1_length then true
    else
      match s2 with
      | [] -> false
      | c :: tail ->
        let found = (FrequencyMap.mem c cache) in
        if found then
          if FrequencyMap.find c cache > 0 then
            let freq = FrequencyMap.find c cache in
            let updated_cache = FrequencyMap.add c (freq - 1) cache in
            aux' (current + 1) tail updated_cache
          else
            let freq = FrequencyMap.find c cache_orig in
            let updated_cache = FrequencyMap.add c (freq - 1) cache_orig in
            aux' 1 tail updated_cache
        else
          aux' 0 tail cache_orig
  in aux' 0 s2 cache_orig

let () =
  let sol1 = permutation s1_1 s1_2 in
  let sol2 = permutation s2_1 s2_2 in
  Printf.printf "Sol1: %B\n" sol1;
  Printf.printf "Sol2: %B\n" sol2
