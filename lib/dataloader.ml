open Str
(* open Printf *)

(* open Formatter *)

let extract_numbers line = 
  let regex = regexp "[0-9]+" in  (* Correct regex to match one or more digits *)
  let rec find_all acc pos =
    try 
      ignore (search_forward regex line pos);  (* Search for the next match *)
      let substr = matched_string line in      (* Get the matched substring *)
      let retval = int_of_string substr in     (* Convert substring to int *)
      find_all (retval :: acc) (match_end ()) (* Recurse with updated position *)
    with Not_found ->
      List.rev acc  (* Return the accumulated list in correct order *)
  in
  find_all [] 0  (* Initialize accumulator and position *)



let process_file filename =
  let ic = open_in filename in
  let rec process_lines acc =
    try
      let line = input_line ic in
      let numbers = extract_numbers line in
      process_lines (numbers :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  process_lines []



let load filename =
  process_file filename