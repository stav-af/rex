(* open Formatter *)
open Partition
open Utils

open Torch

let importance = ref (Array.make 12 1.)
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

let rec unfold_right f init =
    match f init with
    | None -> []
    | Some (x, next) -> x :: unfold_right f next


let range n =
    let irange x = if x >= n then None else Some (x, x + 1) in
    unfold_right irange 0


let combinations_n n_partitions =
  let all_subsets = power_set (List.init (n_partitions) Fun.id) in
  List.filter (fun subset -> let l = List.length subset in l > 0 &&
                                l < n_partitions) all_subsets


let occlude data partitions = 
  List.mapi ( fun i x_i -> 
    if List.exists (
      fun p_list -> List.mem i p_list
    ) partitions then 
    occlusion_value else x_i
  ) data


let get_indices partitions index_combinations = 
  List.map (
    fun idx_combo ->
      List.filteri (
        fun i _ -> List.mem i idx_combo
      ) partitions
  ) index_combinations


let gen_mutants data partition_combinations =
  List.map (fun partition ->
    (partition, occlude data partition)
  ) partition_combinations


let forward model data = 
  let float_data = data |> Array.of_list |> Array.map float_of_int in
  let tens = Tensor.of_float1 float_data in
  let raw =(Module.forward model [tens]) in
  let pred = t_stable_softmax raw in
  let activ = Tensor.argmax pred ~dim:0  in
  match Tensor.to_float0 activ with
  | Some x -> int_of_float x
  | None -> failwith "conversion failed"

    
let unique l =
  let rec aux l acc =
    match l with
    | [] ->
      acc
    | h :: t ->
      if List.mem h acc then aux t acc else aux t (h :: acc)
  in aux l [] 


let assign_responsibility part responsibility =
  let resp_distributed = responsibility /. (float (List.length part)) in
  for i = 0 to 12 do 
    if List.mem i part then
      !importance.(i) <- resp_distributed
    else ()
  done
    

let responsibility part consistent_set = 
  let consistent_with_partition = 
    List.filter (fun mut -> not (List.mem part mut)) consistent_set 
  in 

  let adding_part_changes_prediction = 
    List.filter (fun mut -> 
      not (List.exists 
      (fun consistent -> are_lists_equal consistent (part :: mut)) consistent_set)
    ) consistent_with_partition 
  in

  match adding_part_changes_prediction with
  | [] -> 0.0  (* If no such mutants, responsibility is 0 *)
  | _ ->
    let min_mutant = min_by List.length adding_part_changes_prediction in
    let minpart = List.length min_mutant in
    1. /. (float minpart)

let rex model data = 
  let init_partitions = 6 in 
  importance := Array.make (List.length data) 0.;
  let max_depth = 1 in
  let initial_prediction = forward model data in
  let refine choices max_partitions depth = 
    if depth >= max_depth || List.length choices < max_partitions then () else
    let importance_lst = Array.to_list !importance in

    let partitions = partition importance_lst choices max_partitions in
    let n_partitions = List.length partitions in
    let occlusion_idx_combinations = combinations_n n_partitions in   
    
    let partition_occlusion_combinations = get_indices partitions occlusion_idx_combinations in  
    let mutants = gen_mutants data partition_occlusion_combinations in

    let consistent_set = List.filter (fun (_, mut) -> forward model mut = initial_prediction) mutants in
    let consistent_set_occlusions = List.map (fun (indices, _) -> indices) consistent_set in

    if List.length consistent_set_occlusions != 0 then
    List.iter (fun partition -> 
        let resp = responsibility partition consistent_set_occlusions in
        assign_responsibility partition resp
      ) partitions
    
  in

    (* List.iter
    (
      fun partition ->
        refine partition max_partitions (depth + 1)
    ) to_search; () in *)

  let () = for _ = 0 to 5 do
    refine (range 12) init_partitions 0;
  done in
  !importance