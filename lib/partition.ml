

let partition distribution start_idx end_idx n_partitions=
  let total_weight = Array.fold_left ( +. ) 0. distribution in
  let rec generate_partition_sizes n remaining start n_partitions acc =
    if remaining = 0 || n = n_partitions then List.rev acc
    else
      let part_size = (end_idx - start_idx) in
      let avg_part_size = part_size / n_partitions in

      let weight = distribution.(start_idx) in
      let weight_ratio = weight /. total_weight in
      
      let size =
        if Random.float() ~max:1.0 < weight_ratio 
          then (Random.int() ~max:avg_part_size)
          else max 1 (Random.int() ~max:(part_size / 2))
      in

      if size <= remaining then
        if size = 0 then generate_partition_sizes n remaining start n_partitions acc
        else generate_partition_sizes (n + 1) (remaining - size) (start + size) n_partitions (size :: acc)
      else List.rev (remaining::acc)
  in

  let partition_sizes = generate_partition_sizes 0 end_idx start_idx n_partitions [] in

  let rec sizes_to_bounds sizes start acc =
    match sizes with
    | [] -> (match acc with
      | (l, u)::rest when l > end_idx || u > end_idx -> List.rev rest
      | (_, n)::_ when n = end_idx -> List.rev acc
      | (_, n)::_                  -> List.rev ((n, end_idx)::acc)
      | [] -> failwith "No partitions generated")
    | size :: rest ->
      let end_index = start + size in
      sizes_to_bounds rest end_index ((start, end_index) :: acc)
  in

  let partitions = sizes_to_bounds partition_sizes start_idx [] in
  partitions
