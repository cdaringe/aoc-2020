open Base
open Fixture

module BusPlan = struct
  open CCList

  type idx_step = int * int [@@deriving show]

  type buses = idx_step list [@@deriving show]

  let from_lines lines =
    take 1 lines |> function
    | [ buses_s; _ ] :: _rest ->
        String.split buses_s ~on:','
        |> List.filter_mapi ~f:(fun i c ->
               if String.equal "x" c then None else Some (i, Int.of_string c))
    | _ -> failwith "invalid bus input"

  let get_aligned_time buses =
    let result = ref 1 and mode = ref 1 in
    CCList.iter
      (fun (offset, id) ->
        while Int.rem (!result + offset) id <> 0 do
          result := !result + !mode
        done;
        mode := !mode * id)
      buses;
    !result
end

let solve () =
  let open BusPlan in
  get_newline_delimited_lines () |> from_lines |> fun buses ->
  (* let base_step = List.hd_exn buses in
     let max_step =
       List.max_elt
       ~compare:(fun (_, busa) (_, busb) -> Int.compare busa busb)
       buses
       |> fun v -> Option.value_exn v
     in *)
  Caml.print_string [%string "%{show_buses buses}  \n\n"];
  get_aligned_time buses
