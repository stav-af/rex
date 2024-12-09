let max_by metric_func lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      List.fold_left (fun acc x -> if metric_func x > metric_func acc then x else acc) hd tl

let min_by metric_func lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      List.fold_left (fun acc x -> if metric_func x < metric_func acc then x else acc) hd tl


let rec power_set lst =
  match lst with
  | [] -> [[]]
  | x :: xs ->
      let rest = power_set xs in
      rest @ List.map (fun subset -> x :: subset) rest


let rec take n lst =
    match (n, lst) with
    | (0, _) -> []
    | (_, []) -> []
    | (n, x :: xs) -> x :: take (n - 1) xs


let are_lists_equal l1 l2 = 
  let sl1 = List.sort compare l1 in
  let sl2 = List.sort compare l2 in 
  sl1 = sl2

