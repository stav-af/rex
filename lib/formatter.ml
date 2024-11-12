let pretty_print_l lst = 
  Printf.printf "[";
  List.iter (
    fun elem -> Printf.printf "%d, " elem
  ) lst;
  Printf.printf "]"

let pretty_print_lf lst = 
  Printf.printf "[";
  List.iter (
    fun elem -> Printf.printf "%f, " elem
  ) lst;
  Printf.printf "]"


let string_of_partition partition =
  partition
  |> List.map (fun (lb, ub) -> Printf.sprintf "(%d, %d)" lb ub)
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let string_of_int_list lst =
  lst
  |> List.map string_of_int
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let pp_mutants mutants =
  mutants
  |> List.iter (fun (partition, occluded_data) ->
        Printf.printf "Partition: %s, Occluded Data: %s\n"
          (string_of_partition partition)
          (string_of_int_list occluded_data)
      )

let pp_partition_range pr =
  let pr_str =
    pr
    |> List.map (fun (lb, ub) -> Printf.sprintf "(%d, %d)" lb ub)
    |> String.concat "; "
  in
  Printf.printf "[%s]" pr_str

let pp_mutant (ranges, data) =
  Printf.printf "Ranges: ";
  pp_partition_range ranges;
  Printf.printf ", Data: [";
  data |> List.iter (fun x -> Printf.printf "%d; " x);
  Printf.printf "]\n"

let pp_cst_set cst_set =
  Printf.printf "Consistent Set:\n";
  List.iter pp_mutant cst_set