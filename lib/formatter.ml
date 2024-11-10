let pretty_print_l lst = 
  Printf.printf "[";
  List.iter (
    fun elem -> Printf.printf "%d, " elem
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