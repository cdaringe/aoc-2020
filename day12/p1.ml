open Base
open Fixture

type pos = int * int [@@deriving show]

type angle = int [@@deriving show]

type boaty = { pos : pos; angle : angle } [@@deriving show]

type cmd =
  | N of int
  | S of int
  | E of int
  | W of int
  | L of int
  | R of int
  | F of int
  [@@deriving show]

let parse_cmd s =
  Caml.Scanf.sscanf s "%c%d" (fun ch num ->
      match ch with
      | 'N' -> N num
      | 'S' -> S num
      | 'E' -> E num
      | 'W' -> W num
      | 'L' -> L num
      | 'R' -> R num
      | 'F' -> F num
      | _ -> failwith [%string "invalid char %{ch#Char}"])

module Boat = struct

  let get_next_angle angle da =
    let v = Int.rem (angle + da) 360 in
    if v > 0 then v else v + 360

  let rec move boat cmd =
    let cmd_s = show_cmd cmd in
    Caml.print_string [%string "%{cmd_s}\n"];
    let { pos = x, y; angle } = boat in
    let boat' = match cmd with
    | N num -> { boat with pos = (x, y + num) }
    | S num -> { boat with pos = (x, y - num) }
    | E num -> { boat with pos = (x + num, y) }
    | W num -> { boat with pos = (x - num, y) }
    | L num ->
      let next_angle = get_next_angle angle num in
      Caml.print_string [%string "angle last %{angle#Int} next %{next_angle#Int}\n"];
      { boat with angle = next_angle }
    | R num ->
      let next_angle = get_next_angle angle (-num) in
      Caml.print_string [%string "angle last %{angle#Int} next %{next_angle#Int}\n"];
      { boat with angle = next_angle }
    | F num -> (
        match angle with
        | 0 | 360 -> move boat (E num)
        | 90 -> move boat (N num)
        | 180 -> move boat (W num)
        | 270 -> move boat (S num)
        | _ -> failwith [%string "unhandled angle %{show_angle angle}"] )
    in
    Caml.print_string [%string "next_boat: %{show_boaty boat'}\n"];
    boat'

  let distance (ax,ay) (bx,by) =
    Int.((abs (bx-ax)) + (abs (by-ay)))
end

let solve () =
  let open Boat in
  get_lines parse_cmd |> CCList.fold_left move { pos = (0, 0); angle = 0 }
  |> fun boat -> distance (0,0) boat.pos
