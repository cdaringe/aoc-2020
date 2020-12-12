open Base
open Fixture

module JoltLyfe = struct
  let test_is_compatible r r' =
    let diff = r' - r in
    diff >= 1 && diff <= 3

  let get_compatible rating ratings =
    CCList.filter (test_is_compatible rating) ratings

  let p = Caml.print_string

  let rec find_graph l ~device_joltage =
    let open CCList in
    match take_drop 1 l with
    | [], _ -> Some []
    | [ current ], [] -> (
        match get_compatible current [ device_joltage ] with
        | [ _ ] -> Some [ current ]
        | _ -> None )
    | [ current ], rest ->
        let compatible_adapters = get_compatible current rest in
        find_map
          (fun adapter ->
            let adapters_in_bag = filter (fun y -> y <> adapter) rest in
            find_graph adapters_in_bag ~device_joltage |> function
            | Some sub -> Some (current :: adapter :: sub)
            | _ -> None)
          compatible_adapters
    | _ -> failwith "invalid find_graph input"

  let get_device_joltage l =
    let v v' = Option.value_exn v' in
    List.max_elt ~compare l |> v |> fun j -> j + 3
end

let solve () =
  let open CCList in
  let open JoltLyfe in
  let compare = Int.compare in
  get_lines Int.of_string |> sort compare |> fun adapters ->
  let device_joltage = get_device_joltage adapters in
  find_graph adapters ~device_joltage |> function
  | None -> failwith "no adapter combo found"
  | Some adapters' ->
      let l_to_s l = Sexp.to_string @@ List.sexp_of_t Int.sexp_of_t l in
      let joltages = [ 0 ] @ adapters' @ [ device_joltage ] in
      let jolt_diffs =
        foldi
          (fun acc i n ->
            if i = 0 then acc
            else
              let last = nth joltages (i - 1) in
              acc @ [ n - last ])
          [] joltages
      in
      p [%string "diffs %{l_to_s jolt_diffs}\n"];
      let one_jolt_diffs = (count (Int.equal 1)) jolt_diffs in
      let three_jolt_diffs = (count (Int.equal 3)) jolt_diffs in
      one_jolt_diffs * three_jolt_diffs
