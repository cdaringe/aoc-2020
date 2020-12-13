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

  let get_scaled_offset ~pos:(x, y) ~offset:(dx, dy) ~scalar =
    (x + (dx * scalar), y + (dy * scalar))

  let rec test_freeish ~lobby ~scalar ~pos ~offset =
    let open CCList in
    let x, y = get_scaled_offset ~scalar ~pos ~offset in
    try
      match nth_opt lobby y with
      | Some row -> (
          match nth_opt row x with
          | None -> true
          | Some cell ->
              if Seat.test_free cell then true
              else if Seat.test_occ cell then false
              else test_freeish ~lobby ~pos ~offset ~scalar:(scalar + 1) )
      | None -> true
    with Invalid_argument _ -> true

  let rec test_occupied ~lobby ~scalar ~pos ~offset =
    let open CCList in
    let x, y = get_scaled_offset ~scalar ~pos ~offset in
    try
      match nth_opt lobby y with
      | Some row -> (
          match nth_opt row x with
          | None -> false
          | Some cell ->
              if Seat.test_occ cell then true
              else if Seat.test_free cell then false
              else test_occupied ~lobby ~scalar:(scalar + 1) ~pos ~offset )
      | None -> false
    with Invalid_argument _ -> false

  let test_all_adjacent_free lobby pos =
    let all_adjacent_free =
      CCList.(
        for_all
          (fun offset -> test_freeish ~scalar:1 ~lobby ~pos ~offset)
          adjacency_offsets)
    in
    all_adjacent_free

  let test_adjacent_should_free lobby pos =
    let count_occ =
      CCList.(
        count
          (fun offset -> test_occupied ~scalar:1 ~lobby ~pos ~offset)
          adjacency_offsets)
    in
    count_occ >= 5

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

  let get_next lobby : t =
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
    (* ignore @@ print l; *)
    let next = get_next l in
    if l = next then l else settle next
end

let solve () =
  let open Lobby in
  get_lines (fun f -> f) |> of_lines |> settle |> count_occupied
