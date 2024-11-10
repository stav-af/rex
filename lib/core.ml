(* open Formatter *)
open Torch

type partition_range = (int*int) list


let data = [2;2;2;2;2;2;2;2;2;2;2;2]
let occlusion_value = 1

let minimum_by cmp lst =
  match lst with
  | [] -> failwith "Minby called with empty list"  (* Return None for an empty list *)
  | hd :: tl ->
      List.fold_left (fun acc x -> if cmp x acc < 0 then x else acc) hd tl

let rec power_set lst =
  match lst with
  | [] -> [[]]
  | x :: xs ->
      let rest = power_set xs in
      rest @ List.map (fun subset -> x :: subset) rest

let combinations_012 =
  let all_subsets = power_set [0; 1; 2] in
  List.filter (fun subset -> List.length subset > 0) all_subsets


let get_indices source index_lists =
  List.map (fun index_list ->
    List.map (fun idx -> List.nth source idx) index_list
  ) index_lists

let gen_partition start_idx end_idx =
  let mid = (start_idx + end_idx)/2 in 
  let lb = Random.int_in_range ~min:start_idx ~max:mid in
  let ub = Random.int_in_range ~min:mid ~max:end_idx in
  [(start_idx, lb); (lb, ub); (ub, end_idx)]

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

let diffm partition cst_set = 
  let others = List.filter (fun (ranges, _) -> List.mem partition ranges) cst_set in
  let min_ocl = minimum_by cmp_by_length others in
  let (min_ranges, _) = min_ocl in
  List.length min_ranges
  

let forward (_) = (* will need to incorporate pytorch models here later *)
  2

let test() =
  Random.self_init();
  let model = 
  let occlusion_idx_combinations = List.filter (
    fun l -> List.length l < 3
  ) combinations_012 in
  let initial_prediction = 2 in
  let partitions = gen_partition 0 (List.length data) in
  let partition_occlusion_combinations = get_indices partitions occlusion_idx_combinations in 
  let mutants = gen_mutants data partition_occlusion_combinations in
  let consistent_set = List.filter (fun (_, mut) -> forward mut = initial_prediction) mutants in
  List.iter ( fun (lb, ub) -> Printf.printf "(%d, %d) had k <- 1/(%d + 1)" lb ub (diffm (lb, ub) consistent_set)) partitions