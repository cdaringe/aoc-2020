open Base
open Fixture

let p = Caml.print_string

let l_to_s l = Sexp.to_string @@ List.sexp_of_t Int.sexp_of_t l

module JoltLyfe = struct
  let test_is_compatible r r' =
    let diff = r' - r in
    diff >= 1 && diff <= 3

  let get_compatible rating ratings =
    CCList.take_while (test_is_compatible rating) ratings

  let rec count_compatible_sub_combinations ~cache ~device_joltage l =
    match Hashtbl.find cache l with
    | Some c -> c
    | _ -> (
        let open CCList in
        match l with
        | [] -> 1
        | [ adaptor ] -> (
            match get_compatible adaptor [ device_joltage ] with
            | [] -> 0
            | _ -> 1 )
        | current :: rest ->
            let count_of_child_combos =
              fold_left
                (fun total compat_with_current ->
                  let adapters_in_bag =
                    filter (fun y -> y >= compat_with_current) rest
                  in
                  let child_count =
                    count_compatible_sub_combinations ~cache ~device_joltage
                      adapters_in_bag
                  in
                  total + child_count)
                0
                (get_compatible current rest)
            in
            Hashtbl.set cache ~key:l ~data:count_of_child_combos;
            count_of_child_combos )

  let count_adapter_combinations unsorted ~device_joltage =
    let open CCList in
    let cache : (int list, int) Base.Hashtbl.t =
      Hashtbl.Poly.create ~growth_allowed:true ()
    in
    let compare = Int.compare in
    let l = sort compare unsorted in
    let compatible_roots = take_while (fun x -> x <= 3) l in
    let root_counts =
      mapi
        (fun i _ ->
          count_compatible_sub_combinations ~cache ~device_joltage (drop i l))
        compatible_roots
    in
    fold_left ( + ) 0 root_counts

  let get_device_joltage l =
    let v v' = Option.value_exn v' in
    List.max_elt ~compare l |> v |> fun j -> j + 3
end

let solve () =
  let open JoltLyfe in
  get_lines Int.of_string |> fun adapters ->
  let device_joltage = get_device_joltage adapters in
  count_adapter_combinations adapters ~device_joltage
