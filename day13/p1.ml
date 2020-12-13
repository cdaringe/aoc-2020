open Base
open Fixture

module BusPlan = struct
  open CCList

  type t = { start : int; buses : int list }

  let from_lines lines =
    take 1 lines |> function
    | [ buses_s; start_s ] :: _rest ->
        {
          buses =
            String.split buses_s ~on:','
            |> filter (fun c -> not (String.equal "x" c))
            |> map Int.of_string;
          start = Int.of_string start_s;
        }
    | _ -> failwith "invalid bus input"

  let ride_next { start; buses } =
    let next_arrival start interval = interval - Int.rem start interval in
    let get_arrival = next_arrival start in
    let arrivals = combine buses (map get_arrival buses) in

    List.min_elt ~compare:(fun (_, da) (_, db) -> Int.compare da db) arrivals
    |> fun o -> Option.value_exn o
end

let solve () =
  let open BusPlan in
  get_newline_delimited_lines () |> from_lines |> ride_next |> fun (id, wait) ->
  id * wait
