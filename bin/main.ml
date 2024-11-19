open Torch

open Rex.Dataloader
(* open Rex.Formatter *)
open Rex.Core
(* open Rex.Partition *)


let modelmap = [
    (* "and.pt", "and.csv"; *)
    (* "or.pt", "or.csv"; *)
    (* "xor.pt", "xor.csv"; *)
    (* "xor_and_xor.pt", "xor_and_xor.csv"; *)
    (* "xor_and_3.pt", "xor_and_3.csv";
    "xor_and_4.pt", "xor_and_4.csv";
    "xor_and_5.pt", "xor_and_5.csv";
    "xor_and_6.pt", "xor_and_6.csv";
    "xor_and_7.pt", "xor_and_7.csv";
    "xor_and_8.pt", "xor_and_8.csv";
    "xor_and_9.pt", "xor_and_9.csv"; *)
    "xor_and_10.pt", "xor_and_10.csv";
    "and_or_3.pt", "and_or_3.csv";
    "and_or_4.pt", "and_or_4.csv";
    "and_or_5.pt", "and_or_5.csv";
    "and_or_6.pt", "and_or_6.csv";
    "and_or_7.pt", "and_or_7.csv";
    "and_or_8.pt", "and_or_8.csv";
    "and_or_9.pt", "and_or_9.csv";
    "and_or_10.pt", "and_or_10.csv"
]

let shuffle arr =
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int() ~max:(i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done

let initialize_centroids data k =
  let n = Array.length data in
  if k > n then
    failwith "Number of clusters k cannot exceed number of data points.";
  let data_copy = Array.copy data in
  shuffle data_copy;
  Array.sub data_copy 0 k

let assign_clusters data centroids =
  let k = Array.length centroids in
  let assignments = Array.make (Array.length data) 0 in
  Array.iteri (fun i point ->
    let min_dist = ref (abs_float (point -. centroids.(0))) in
    let cluster = ref 0 in
    for c = 1 to k - 1 do
      let dist = abs_float (point -. centroids.(c)) in
      if dist < !min_dist then begin
        min_dist := dist;
        cluster := c
      end
    done;
    assignments.(i) <- !cluster
  ) data;
  assignments

let update_centroids data assignments k =
  let sums = Array.make k 0.0 in
  let counts = Array.make k 0 in
  Array.iteri (fun i point ->
    let cluster = assignments.(i) in
    sums.(cluster) <- sums.(cluster) +. point;
    counts.(cluster) <- counts.(cluster) + 1
  ) data;
  Array.init k (fun c ->
    if counts.(c) = 0 then
      data.(Random.int() ~max:(Array.length data))
    else
      sums.(c) /. float_of_int counts.(c)
  )

let has_converged old_centroids new_centroids epsilon =
  Array.for_all2 (fun old_c new_c -> abs_float (old_c -. new_c) < epsilon) old_centroids new_centroids

let kmeans data k =
  let max_iterations = 100 in
  let epsilon = 1e-4 in
  let centroids = initialize_centroids data k in
  let rec iterate current_centroids iter =
    if iter > max_iterations then
      failwith "k-means did not converge within the maximum number of iterations.";
    let assignments = assign_clusters data current_centroids in
    let new_centroids = update_centroids data assignments k in
    if has_converged current_centroids new_centroids epsilon then
      new_centroids, assignments
    else
      iterate new_centroids (iter + 1)
  in
  let final_centroids, final_assignments = iterate centroids 0 in
  let highest_cluster_idx =
    let max_val = ref final_centroids.(0) in
    let idx = ref 0 in
    for c = 1 to k - 1 do
      if final_centroids.(c) > !max_val then begin
        max_val := final_centroids.(c);
        idx := c
      end
    done;
    !idx
  in
  let highest_cluster_indices =
    Array.fold_left (fun acc (i, cluster) ->
      if cluster = highest_cluster_idx then i :: acc else acc
    ) [] (Array.mapi (fun i cluster -> (i, cluster)) final_assignments)
    |> List.rev
    |> Array.of_list
  in
  highest_cluster_indices


let split_at index lst =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | x :: xs ->
        if i = 0 then (List.rev acc, x :: xs)
        else aux (i - 1) (x :: acc) xs
  in
  if index < 0 then invalid_arg "split_at: negative index"
  else aux index [] lst


let () =
  let sample_size = 500 in 
  List.iter (
    fun (model_name, dataset_name) ->
      let cwd = Sys.getcwd() in
      
      let model_path = Printf.sprintf "%s/%s/%s" cwd "models" model_name in
      let model = Module.load model_path in

      let dataset_path =  Printf.sprintf "%s/%s/%s" cwd "data" dataset_name in
      let data = load dataset_path in

      let correct_count = ref 0 in 
      List.iteri (
        fun i row -> 
          let (input_raw, rest) = split_at 12 row in
          let (_, explanation_raw) = split_at 1 rest in
          
          let input = List.map (fun x -> if x = 1 then 2 else x) input_raw in
          let explanation = List.map (fun x -> x - 1) explanation_raw in 

          let importance_explanation = f_stable_softmax (f_stable_softmax (rex model input)) in
          let k3_explanation = kmeans (importance_explanation) 2 in
          
          (* pretty_print_l explanation;
          pretty_print_l (Array.to_list k3_explanation); *)
          if k3_explanation = Array.of_list explanation then 
            correct_count := !correct_count + 1;

      
          Printf.printf "%s_%n: %f\n%!" model_name i ((float_of_int !correct_count) /. float_of_int i);
          ()
      ) (take sample_size data);
      
      Printf.printf "%s: %f\n%!" model_name ((float_of_int !correct_count) /. float_of_int sample_size);
  ) modelmap


  (* let arr = [| 100.0; 100.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0 |] in
  partition arr 0 12 6 *)