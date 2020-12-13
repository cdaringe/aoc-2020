open Base
open Fixture

module Lobby = struct
  module Seat = struct
    type state = Floor | Occ | Open

    let ( = ) t1 t2 =
      match (t1, t2) with
      | Floor, Floor | Occ, Occ | Open, Open -> true
      | _ -> false

    let test_free = function Open -> true | _ -> false

    let test_occ = function Occ -> true | _ -> false

    let test_floor = function Floor -> true | _ -> false

    let to_char = function Occ -> '#' | Open -> 'L' | _ -> '.'
  end

  type t = Seat.state list list

  let char_to_state c =
    let open Seat in
    match c with '.' -> Floor | 'L' -> Open | _ -> Occ

  let line_to_state line = String.to_list line |> CCList.map char_to_state

  let of_lines lines = CCList.map line_to_state lines

  let ( = ) (a : t) (b : t) =
    let open List in
    let is_row_equal a b = Seat.(a = b) in
    let test_equal_rows r1 r2 = equal is_row_equal r1 r2 in
    equal test_equal_rows a b

  let adjacency_offsets =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

  let test_freeish ~lobby (x, y) =
    let open CCList in
    try
      match nth_opt lobby y with
      | Some row -> (
          match nth_opt row x with
          | None -> true
          | Some cell -> Seat.test_free cell || Seat.test_floor cell )
      | None -> true
    with Invalid_argument _ -> true

  let test_occupied ~lobby (x, y) =
    let open CCList in
    try
      match nth_opt lobby y with
      | Some row -> (
          match nth_opt row x with
          | None -> false
          | Some cell -> Seat.test_occ cell )
      | None -> false
    with Invalid_argument _ -> false

  let test_all_adjacent_free lobby (x, y) =
    CCList.(
      for_all
        (fun (dx, dy) ->
          let is_free_ish = test_freeish ~lobby (x + dx, y + dy) in
          (* Caml.print_string [%string "test_all_adjacent_free - (%{x#Int},%{y#Int}) - (%{dx#Int},%{dy#Int}) - %{is_free_ish#Bool})\n"]; *)
          is_free_ish)
        adjacency_offsets)

  let test_adjacent_should_free lobby (x, y) =
    let count_occ =
      CCList.(
        count
          (fun (dx, dy) -> test_occupied ~lobby (x + dx, y + dy))
          adjacency_offsets)
    in
    count_occ >= 4

  let count_occupied lobby =
    let open CCList in
    let sum_row row = count Seat.test_occ row in
    fold_left ( + ) 0 (map sum_row lobby)

  let update_seat ~pos ~(seat : Seat.state) ~(lobby : t) =
    let open Seat in
    match seat with
    | Floor -> seat
    | Open -> if test_all_adjacent_free lobby pos then Occ else seat
    | Occ -> if test_adjacent_should_free lobby pos then Open else seat

  let update : t -> t =
   fun lobby ->
    let open CCList in
    mapi
      (fun y row ->
        mapi (fun x seat -> update_seat ~pos:(x, y) ~seat ~lobby) row)
      lobby

  let print (l : t) =
    let open CCList in
    iter
      (fun row ->
        let chars = map Seat.to_char row in
        Caml.print_string [%string "%{String.of_char_list chars}\n"])
      l;
    Caml.print_string "\n";
    l

  let rec settle l =
    let next = update l in
    if l = next then l else settle next
end

let solve () =
  let open Lobby in
  get_lines (fun f -> f) |> of_lines |> settle |> count_occupied
