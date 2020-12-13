open Base
open Fixture

type pos = int * int [@@deriving show]

type angle = int [@@deriving show]

type boaty = { pos : pos; waypoint : pos } [@@deriving show]

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

  let to_positive_angle angle = get_next_angle angle 0

  let rec move boat cmd =
    let { pos = x, y; waypoint = wx, wy } = boat in
    match cmd with
    | N num -> { boat with waypoint = (wx, wy + num) }
    | S num -> { boat with waypoint = (wx, wy - num) }
    | E num -> { boat with waypoint = (wx + num, wy) }
    | W num -> { boat with waypoint = (wx - num, wy) }
    | L angle -> (
        to_positive_angle angle |> function
        | 0 -> boat
        | 90 -> { boat with waypoint = (-wy, wx) }
        | deg -> move (move boat (L (deg - 90))) (L 90) )
    | R num -> move boat (L (-num))
    | F num -> { boat with pos = (x + (num * wx), y + (num * wy)) }

  let distance (ax, ay) (bx, by) = Int.(abs (bx - ax) + abs (by - ay))
end

let solve () =
  let open Boat in
  get_lines parse_cmd
  |> CCList.fold_left move { waypoint = (10, 1); pos = (0, 0) }
  |> fun boat -> distance (0, 0) boat.pos
