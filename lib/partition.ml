(* open Formatter *)
open Utils

open List


let rec cdf_tail arr acc = 
  let lst = if length acc = 0 then 0. else hd acc in
  match arr with
  | [] -> rev acc
  | x::xs -> cdf_tail xs (lst+.x::acc)

let cdf arr = 
  cdf_tail arr []

let norm arr = 
  let sum = fold_left (+.) 0. arr in
  if sum = 0. then 
    let n = length arr in
    init n (fun _ -> 1./.(float_of_int n))
  else
      map (fun x -> x /. sum) arr

let find_index f lst =
  let rec aux i = function
    | [] -> None
    | x :: xs -> if f x then Some i else aux (i + 1) xs
  in
  aux 0 lst

let partition responsibility choices n_parts = 
  let nonzero_resp = 
    if for_all (fun x -> x = 0.) responsibility then
      init (length responsibility) (fun _ -> 1./.(float_of_int (length responsibility)))
    else
      responsibility
    in

  
  let rec aux acc curr score used = 

    let resp_options = mapi (fun i x -> if (mem i choices && not (mem i used))then x else 0.) nonzero_resp in
    let steps = cdf (norm resp_options) in 
    let avg_resp = 1. /. float_of_int (length resp_options) in   

    if are_lists_equal used choices 
      || length acc = n_parts 
      || for_all (fun x -> x = 0.) resp_options
    then acc else
    
    let rand = Random.float() ~max:1. in
    let curr_choice = 
      match find_index (fun x -> x > rand) steps with
      | Some x -> x
      | _ -> failwith "shoudln't really be here" in

    let curr_resp = nth resp_options curr_choice in

    let next_partition = curr_choice::curr in
    let next_score = score +. curr_resp in
    
    if next_score > avg_resp then
      aux (next_partition::acc) [] 0. (curr_choice::used)
    else
      aux acc next_partition next_score (curr_choice::used)
    in
  aux [] [] 0. []
    
    



