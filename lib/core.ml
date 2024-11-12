open Formatter
open Torch

type partition_range = (int*int) list


let data = [0;2;2;2;2;2;2;2;2;2;2;2]
let importance = ref (Array.make (List.length data) 0.0)
let occlusion_value = 1
  

let stable_softmax logits =
  let max_logit = Tensor.maximum logits in
  let logits_shifted = Tensor.sub logits max_logit in
  let exp_logits = Tensor.exp logits_shifted in
  let sum_exp = Tensor.sum exp_logits in
  Tensor.div exp_logits sum_exp

let minimum_by cmp lst =
  match lst with
  | [] -> None
  | hd :: tl -> Some (List.fold_left 
        (fun acc x -> if cmp x acc < 0 then x else acc
      ) hd tl)

let min_by metric_func lst =
  match lst with
  | [] -> failwith "Empty list"
  | hd :: tl ->
      List.fold_left (fun acc x -> if metric_func x < metric_func acc then x else acc) hd tl

let rec power_set lst =
  match lst with
  | [] -> [[]]
  | x :: xs ->
      let rest = power_set xs in
      rest @ List.map (fun subset -> x :: subset) rest

let combinations_012 =
  let all_subsets = power_set [0; 1] in
  List.filter (fun subset -> List.length subset > 0) all_subsets


let get_indices source index_lists =
  List.map (fun index_list ->
    List.map (fun idx -> List.nth source idx) index_list
  ) index_lists

let gen_partition start_idx end_idx =  
  [(start_idx, end_idx/2); (end_idx/2, end_idx)]

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

let diffm partition cst_set = 
  let shown_part = List.filter (fun (ranges, _) -> not (List.mem partition ranges)) cst_set in
  if (List.length shown_part) = (List.length cst_set)
    then 0
  else 
    let subj = List.filter (
      fun (ranges, _) -> not (List.for_all (
        fun (oth_range, _) -> 
          are_lists_equal (partition::ranges) oth_range
      ) cst_set )
    ) shown_part in
    match minimum_by cmp_by_length subj with
    | Some min_ocl ->
      let (min_ranges, _) = min_ocl in
      List.length min_ranges
    | None -> 1
    

let forward model data = (* will need to incorporate pytorch models here later *)
  let float_data = data |> Array.of_list |> Array.map float_of_int in
  let tens = Tensor.of_float1 float_data in
  let raw =(Module.forward model [tens]) in
  let pred = stable_softmax raw in
  let activ = Tensor.argmax pred ~dim:0  in
  let result = match Tensor.to_float0 activ with
  | Some x -> int_of_float x

  | None -> failwith "conversin failed" in
  result

let apply_diff range value refinement_depth =
  List.iter (fun (start_idx, end_idx) ->
    for i = start_idx to end_idx - 1 do
      !importance.(i) <- (!importance.(i) +. (1.0/.(1.0+.(float_of_int value)))/. (float_of_int refinement_depth)  )
    done
  ) range

let test() =
  let model = Module.load "/home/stav/dev/uni/xai/rsandbox/rex/models/xor_and_xor.pt" in
  let occlusion_idx_combinations = List.filter (
    fun l -> List.length l < 2
  ) combinations_012 in
  let plb = 0 in
  let pub = 12 in
  let rec refine lower upper depth = 
    if upper != lower then
      let initial_prediction = forward model data in
      let partitions = gen_partition lower upper in
      let partition_occlusion_combinations = get_indices partitions occlusion_idx_combinations in 
      let mutants = gen_mutants data partition_occlusion_combinations in
      let consistent_set = List.filter (fun (_, mut) -> forward model mut = initial_prediction) mutants in
      pretty_print_lf (Array.to_list !importance);
      let important = List.filter (
        fun (lb, ub) -> (diffm (lb, ub) consistent_set) = 0) partitions in
      Printf.printf "Range: (%d, %d)\n Important part: " lower upper;
      pp_partition_range important;
      print_endline "";
      List.iter (
        fun (lb, ub) -> apply_diff [(lb, ub)] (diffm (lb, ub) consistent_set) depth
      ) important;
      pp_partition_range partitions;
      let important = List.filter (fun (lb, ub) -> lb != lower || ub != upper) important in
     (List.iter (fun (l, u) -> refine l u (depth + 1) ) important)
    else
      () in
  refine plb pub 1
  