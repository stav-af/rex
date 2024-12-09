open Torch

open Rex.Dataloader
(* open Rex.Formatter *)
open Rex.Core
open Rex.Utils
(* open Rex.Partition *)


let modelmap = [
    "and.pt", "and.csv";
    "or.pt", "or.csv"; 
    "xor.pt", "xor.csv";
    "xor_and_xor.pt", "xor_and_xor.csv";
    "xor_and_3.pt", "xor_and_3.csv";
    "xor_and_4.pt", "xor_and_4.csv";
    "xor_and_5.pt", "xor_and_5.csv";
    "xor_and_6.pt", "xor_and_6.csv";
    "xor_and_7.pt", "xor_and_7.csv";
    "xor_and_8.pt", "xor_and_8.csv";
    "xor_and_9.pt", "xor_and_9.csv";
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

module Array = struct
  include Array

  let iter2 f x y = iteri (fun i xi -> f xi y.(i)) x

  let foldi f init x =
    snd (fold_left (fun (i, acc) xi -> (i+1, f i acc xi)) (0, init) x)

  let min f x =
    foldi
      (fun i (i0, v0) xi -> let v = f xi in if v0 > v then (i, v) else (i0, v0))
      (-1, max_float) x

  let fold_left2 f init x y = foldi (fun i acc xi -> f acc xi y.(i)) init x

  let map2_sum f = fold_left2 (fun acc xi yi -> acc +. f xi yi) 0.0
end

(** [distance x y] returns the square of the L2 norm of the distance between
    vectors [x] and [y], i.e., [||x - y||^2]. *)
let distance = Array.map2_sum (fun xi yi -> let diff = xi -. yi in diff *. diff)

(** [kmeans k xs] performs [k]-means clustering algorithm for data set [xs].
    @return [(means, cs)] where [means] is an array of mean vectors, and [cs] is
    an array such that the [i]-th element is the class number of [xs.(i)]. *)
let kmeans k xs =
  let d = Array.length xs.(0) in (* the dimension of a sample *)
  let calc_means cs = (* Compute the mean of each class *)
    let z = Array.init k (fun _ -> (ref 0, Array.make d 0.0)) in
    let sum_up ci xi =
      let (n, sum) = z.(ci) in
      Array.iteri (fun j xij -> sum.(j) <- sum.(j) +. xij) xi; (* sum += xi *)
      incr n
    in
    let normalize (n, sum) =
      let c = 1.0 /. float !n in
      Array.map (( *. ) c) sum
    in
    Array.iter2 sum_up cs xs;
    Array.map normalize z
  in
  let update means cs = (* Update class assignment *)
    Array.foldi (fun i updated xi ->
      let ci', _ = Array.min (distance xi) means in
      if cs.(i) <> ci' then (cs.(i) <- ci' ; true) else updated)
    false xs
  in
  let m = Array.length xs in (* the number of samples *)
  let cs = Array.init m (fun _ -> Random.int() ~max:2) in (* class assignment *)
  let rec loop () =
    let means = calc_means cs in
    if update means cs then loop () else (means, cs)
  in
  loop ()(* loop until convergence *)


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
  let sample_size = 100 in 
  List.iter (
    fun (model_name, dataset_name) ->
      let cwd = Sys.getcwd() in
      
      let model_path = Printf.sprintf "%s/%s/%s" cwd "models" model_name in
      let model = Module.load model_path in

      let dataset_path =  Printf.sprintf "%s/%s/%s" cwd "data" dataset_name in
      let data = load dataset_path in

      let correct_count = ref 0 in 
      List.iter (
        fun row -> 
          let (input_raw, rest) = split_at 12 row in
          let (_, explanation_raw) = split_at 1 rest in
          
          let input = List.map (fun x -> if x = 1 then 2 else x) input_raw in
          let explanation = List.map (fun x -> x - 1) explanation_raw in      

          (* print_endline "input";
          pretty_print_l input;
          print_endline ""; *)

          let importance_explanation = (rex model input) in
          
          let rec safe_cluster prev_expl attempt =
            let (cntr, k2_exp) = kmeans 2 (Array.map (fun x -> [| x |]) importance_explanation) in
            if k2_exp = prev_expl || attempt = 100 then
              cntr, k2_exp
            else
              safe_cluster k2_exp (attempt + 1) 
            in

          let (means, k2_explanation) = safe_cluster [||] 0 in 

          let explanation_cluster = if means.(1) > means.(0) then 1 else 0 in
          let acc = ref [] in
          
          Array.iteri (fun i x ->
            if x = explanation_cluster then acc := i :: !acc
          ) k2_explanation;
          let rex_explanation = List.rev !acc in
          
          (* print_endline "\nRex importance values";
          pretty_print_lf (Array.to_list importance_explanation);
          print_endline "\nKmeansed";
          pretty_print_l (rex_explanation);
          print_endline "\npurported exlanation";
          pretty_print_l explanation;
          print_endline "\n\n\n";
           *)

          if rex_explanation = explanation then 
            correct_count := !correct_count + 1;

      
          (* Printf.printf "%s_%n: %f\n%!" model_name i ((float_of_int !correct_count) /. float_of_int (i + 1)); *)
          ()
      ) (take sample_size data);
      
      Printf.printf "%s: %f\n%!" model_name ((float_of_int !correct_count) /. float_of_int sample_size);
  ) modelmap


  (* let arr = [| 100.0; 100.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0 |] in
  partition arr 0 12 6 *)