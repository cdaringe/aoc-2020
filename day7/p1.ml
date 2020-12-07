open Base
open Fixture
open Printf

module TwoPartColor = struct
  module T = struct
    type t = string * string

    let compare (t1a, t1b) (t2a, t2b) =
      let a = String.compare t1a t2a in
      if a = 0 then String.compare t1b t2b else a

    let sexp_of_t (a, b) : Sexp.t = Atom (sprintf "%s %s" a b)
  end

  include T
  include Comparator.Make (T)
end

type bag = (int * string * string) option

type bags_by_color_map =
  (TwoPartColor.t, bag list, TwoPartColor.comparator_witness) Map.t

let parse_bag_str : string -> bag =
 fun s ->
  try Caml.Scanf.sscanf s "%d %s %s %s" (fun n c1 c2 _ -> Some (n, c1, c2))
  with _ -> None

let parse_bags_str : string -> bag list =
 fun bags_str ->
  let open CCList in
  String.split ~on:',' bags_str |> map Caml.String.trim |> map parse_bag_str

let bag_to_str bag =
  match bag with Some (_n, c1, c2) -> sprintf "%s %s" c1 c2 | None -> "no bag"

let parse_bag_rule rule =
  let open CCList in
  let parts = CCString.split ~by:"bags contain" rule |> map Caml.String.trim in
  let biggin = "1 " ^ hd parts ^ " bags." in
  (* Caml.print_string @@ sprintf "\t%s \n" biggin; *)
  let bigger : bag = parse_bag_str biggin in
  let smaller = parse_bags_str @@ nth parts 1 in
  (* Caml.print_string @@ sprintf "%s .. %s\n" (bag_to_str bigger_bag) (bag_to_str (nth smaller_bags 0)); *)
  (bigger, smaller)

let to_fits_inside_map (l : (bag * bag list) list) =
  let open CCList in
  let fill acc (big, smalls) =
    let insert_big_by_smalls acc' small =
      match small with
      | None -> acc'
      | Some (_, c1, c2) ->
          Map.update acc' (c1, c2) ~f:(function
            | None -> [ big ]
            | Some colors -> big :: colors)
    in
    fold_left insert_big_by_smalls acc smalls
  in
  fold_left fill (Map.empty (module TwoPartColor)) l

let rec fold_bag_graph ~acc ~map ~f ~key =
  match Map.find map key with
  | None -> acc
  | Some bags ->
      List.fold
        ~f:(fun acc' bag ->
          let next_acc = f acc' bag in
          match bag with
          | None -> next_acc
          | Some (_, c1, c2) ->
              fold_bag_graph ~acc:next_acc ~map ~f ~key:(c1, c2))
        ~init:acc bags

let find_all_container_colors ((c1, c2) : string * string)
    (fits_in : bags_by_color_map) =
  let se = String.equal in
  Map.fold
    ~init:(Set.empty (module TwoPartColor))
    ~f:(fun ~key ~data:_ acc ->
      let c1', c2' = key in
      if se c1 c1' && se c2 c2' then
        fold_bag_graph ~acc
          ~f:(fun acc' bag ->
            bag |> function
            | None -> acc'
            | Some (_, ca, cb) -> Set.add acc' (ca, cb))
          ~map:fits_in ~key
      else acc)
    fits_in

let solve () =
  get_lines parse_bag_rule |> to_fits_inside_map
  |> find_all_container_colors ("shiny", "gold")
  |> Set.length
