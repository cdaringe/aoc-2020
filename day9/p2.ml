open Base
open Fixture

module XMAS = struct
  let of_string_list = CCList.map Int.of_string

  let find_continguous_summing_to target l =
    let open CCList in
    List.find_mapi l ~f:(fun i _ ->
        let num_subset = drop i l in
        let closet_sum, count_els =
          fold_while
            (fun (total, count) n ->
              let next_total = total + n in
              let is_continuing = next_total < target in
              ( (next_total, count + 1),
                if is_continuing then `Continue else `Stop ))
            (0, 0) num_subset
        in
        if closet_sum = target then Some (take count_els num_subset) else None)
    |> fun v -> Option.value_exn v
end

let sum_min_max nums =
  let open List in
  let compare = Int.compare in
  let value v = Option.value_exn v in
  (min_elt ~compare nums |> value) + (max_elt ~compare nums |> value)

let solve () =
  get_lines (fun s -> s)
  |> XMAS.of_string_list
  |> XMAS.find_continguous_summing_to 15353384
  |> sum_min_max
