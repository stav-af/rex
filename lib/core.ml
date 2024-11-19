(* open Formatter *)
open Partition

open Torch

type partition_range = (int*int) list

let importance = ref (Array.make 12 0.0)
let occlusion_value = 1

let t_stable_softmax logits =
  let max_logit = Tensor.maximum logits in
  let logits_shifted = Tensor.sub logits max_logit in
  let exp_logits = Tensor.exp logits_shifted in
  let sum_exp = Tensor.sum exp_logits in
  Tensor.div exp_logits sum_exp

let f_stable_softmax (logits: float array) : float array =
  let max_logit = Array.fold_left max neg_infinity logits in
  let logits_shifted = Array.map (fun x -> x -. max_logit) logits in
  let exp_logits = Array.map exp logits_shifted in
  let sum_exp = Array.fold_left (+.) 0.0 exp_logits in
  Array.map (fun x -> x /. sum_exp) exp_logits

let max_by metric_func lst =
  match lst with
  | [] -> None
  | hd :: tl -> Some (
      List.fold_left (fun acc x -> if metric_func x > metric_func acc then x else acc) hd tl)


let rec power_set lst =
  match lst with
  | [] -> [[]]
  | x :: xs ->
      let rest = power_set xs in
      rest @ List.map (fun subset -> x :: subset) rest


let rec unfold_right f init =
    match f init with
    | None -> []
    | Some (x, next) -> x :: unfold_right f next


let range n =
    let irange x = if x > n then None else Some (x, x + 1) in
    unfold_right irange 1


let combinations_n n_partitions =
  let all_subsets = power_set (List.init (n_partitions) Fun.id) in
  List.filter (fun subset -> let l = List.length subset in l > 0  &&
                                l < n_partitions) all_subsets


let get_indices source index_lists =
  List.map (fun index_list ->
    List.map (fun idx -> List.nth source idx) index_list
  ) index_lists


let occlude (data: int list) (partitions: partition_range) = 
  List.mapi ( fun i x_i -> 
    if List.exists (
      fun (lb, ub) -> (i >= lb) && (i < ub)
    ) partitions then 
    occlusion_value else x_i
  ) data


let gen_mutants (data) (partition_combinations: partition_range list) =
  List.map (fun partition ->
    (partition, occlude data partition)
  ) partition_combinations


let cmp_by_length (ranges1, _) (ranges2, _) =
    compare (List.length ranges1) (List.length ranges2)


let compare_ranges (a1, b1) (a2, b2) =
  match compare a1 a2 with
  | 0 -> compare b1 b2
  | c -> c


let sort_ranges ranges =
  List.sort compare_ranges ranges


let are_lists_equal lst1 lst2 =
  let sorted_lst1 = sort_ranges lst1 in
  let sorted_lst2 = sort_ranges lst2 in
  sorted_lst1 = sorted_lst2


let diffm cst_set partition= 
  let shown_part = List.filter (fun (ranges, _) -> not (List.mem partition ranges)) cst_set in
  let max_mut = max_by (fun (r, _) -> List.length r) shown_part in

  match max_mut with
    | Some (r, _) -> List.length r
    | None -> (match shown_part with 
      | (r, _)::[] -> List.length r
      | _ -> 0)


let forward model data = 
  let float_data = data |> Array.of_list |> Array.map float_of_int in
  let tens = Tensor.of_float1 float_data in
  let raw =(Module.forward model [tens]) in
  let pred = t_stable_softmax raw in
  let activ = Tensor.argmax pred ~dim:0  in
  match Tensor.to_float0 activ with
  | Some x -> int_of_float x
  | None -> failwith "conversion failed"

  
let apply_diff range occluded_partitions total_partions =
  let (start_idx, end_idx) = range in 
  let f_partition_size = float_of_int (end_idx - start_idx) in
  
  let f_occluded_partitions = float_of_int occluded_partitions in
  let f_total_partitions = float_of_int total_partions in

  let f_partition_responsibility = 1. /. (f_total_partitions -. f_occluded_partitions) in
  let f_split_responsibility = f_partition_responsibility /. f_partition_size in

  (* Printf.printf "Range (%d, %d) has resp: %f\n" start_idx end_idx f_split_responsibility; *)
  for i = start_idx to end_idx - 1 do
    (* Printf.printf "Applying split to %d\n" i; *)
    !importance.(i) <- !importance.(i) +. f_split_responsibility
  done

let rec take n lst =
    match (n, lst) with
    | (0, _) -> []
    | (_, []) -> []
    | (n, x :: xs) -> x :: take (n - 1) xs


let rex model data =
  let init_partitions = 6 in 
  importance := Array.make (List.length data) 0.0;
  
  let plb = 0 in
  let pub = 12 in
  let rec refine lower upper max_partitions depth = 
    let initial_prediction = forward model data in

    let partitions = partition !importance lower upper max_partitions in
    let n_partitions = List.length partitions in

    let occlusion_idx_combinations = combinations_n n_partitions in   
    
    let partition_occlusion_combinations = get_indices partitions occlusion_idx_combinations in  
    let mutants = gen_mutants data partition_occlusion_combinations in

    let consistent_set = List.filter (fun (_, mut) -> forward model mut = initial_prediction) mutants in
    let curr_diffm = diffm consistent_set in

    List.iter (
      fun subject_partition -> 
        apply_diff 
          subject_partition 
          (curr_diffm subject_partition) 
          (List.length partitions)
    ) partitions;
    
    let k = n_partitions / 2 in
    let sorted = List.sort (fun px py -> compare (curr_diffm py) (curr_diffm px)) partitions in
      
    let refine_subjects = List.filter(fun (lb, ub) -> ub - lb > 2) (take k sorted) in
    List.iter (
        fun (lb, ub) -> refine lb ub ((ub - lb - 1) / 2) (depth + 1)
    ) refine_subjects
  in
  
  let () = for _ = 0 to 10 do
    refine plb pub init_partitions 0;
  done in
  (* pretty_print_lf (Array.to_list !importance); *)
  !importance

  