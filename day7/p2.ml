open Base
open Fixture

type bag = int * string

let parse_bag_str s =
  try
    Caml.Scanf.sscanf s "%d %s %s %s" (fun n c1 c2 _ -> Some (n, c1 ^ " " ^ c2))
  with _ -> None

let parse_bags_str bags_str =
  CCList.(
    String.split ~on:',' bags_str |> map Caml.String.trim |> map parse_bag_str)

let parse_bag_rule rule =
  let open CCList in
  let parts = CCString.split ~by:"bags contain" rule |> map Caml.String.trim in
  let biggin = "1 " ^ hd parts ^ " bags." in
  let biggie = parse_bag_str biggin |> fun bago -> Option.value_exn bago in
  let smallies =
    parse_bags_str @@ nth parts 1
    |> filter Option.is_some
    |> map (fun v -> Option.value_exn v)
  in
  (biggie, smallies)

let to_bags_by_color =
  CCList.fold_left
    (fun acc ((_, key), data) -> Map.set acc ~key ~data)
    (Map.empty (module String))

let rec count_inner_bags bags_by_color color =
  let open CCList in
  match Map.find bags_by_color color with
  | None -> 0
  | Some bags ->
      fold_left
        (fun total (n, color) ->
          total + n + (n * count_inner_bags bags_by_color color))
        0 bags

let solve () =
  get_lines parse_bag_rule |> to_bags_by_color |> fun bags_by_color ->
  count_inner_bags bags_by_color "shiny gold"
