open Base
open Fixture

module XMAS = struct
  let of_string_list = CCList.map Int.of_string

  let test_satisfies_preamble preamble n =
    let open CCList in
    exists
      (fun a ->
        exists
          (fun b ->
            let sum = a + b in
            let is_summable = a <> b && sum = n in
            is_summable)
          preamble)
      preamble

  let find_corrupt nums =
    let find_corrupt_in_ith_subset i _ =
      Caml.print_string (Printf.sprintf "testing subset %d\n" i);
      let open CCList in
      let preamble, post = drop i nums |> take_drop 25 in
      not (test_satisfies_preamble preamble (hd post))
    in
    List.findi ~f:find_corrupt_in_ith_subset nums |> fun v ->
    Option.value_exn v |> fun (i, _) ->
    i + 25 |> fun i -> List.nth_exn nums i
end

let solve () =
  get_lines (fun s -> s) |> XMAS.of_string_list |> XMAS.find_corrupt
