open Base
open Fixture

type range = int * int [@@deriving show]

type field = string * range list [@@deriving show]

type int_list = int list [@@deriving show]

type int_int_list = range list [@@deriving show]

type bool_mat = bool list list [@@deriving show]

let range_tuple_of_string s = Caml.Scanf.sscanf s "%d-%d" (fun a b -> (a, b))

let mapf f = CCList.map f

let tap_assert_l_20 l =
  if CCList.length l = 20 then l else failwith "invalid list"

let parse_ticket line =
  CCString.split_on_char ',' line |> CCList.map Int.of_string |> tap_assert_l_20

let parse_field : string -> field =
 fun field_str ->
  let open CCString in
  let fail () = failwith [%string "invalid field %{field_str}"] in
  match split_on_char ':' field_str with
  | [ desc; constraints ] ->
      split ~by:" or " constraints |> mapf trim |> mapf range_tuple_of_string
      |> fun constraints' -> (desc, constraints')
  | _ -> fail ()

let parse_fields = mapf parse_field

let parse_tickets = mapf parse_ticket

let parse_train_chaos lines =
  match lines with
  | [ fields; me; them ] ->
      let open CCList in
      let fields' = parse_fields fields in
      (fields', parse_ticket (nth me 1), parse_tickets (drop 1 them))
  | _ -> failwith "unexpected ticket format"

let test_is_in_range v (a, b) = v >= a && v <= b

let test_satisfies_field (v : int) ((_, ranges) : field) =
  CCList.exists (test_is_in_range v) ranges

let test_field_exists_for_value fields num =
  CCList.exists (fun field -> test_satisfies_field num field) fields

let test_ticket_has_valid_values fields ticket =
  CCList.for_all (fun num -> test_field_exists_for_value fields num) ticket

let filter_invalid_tickets (fields, me, nearby) =
  let open CCList in
  ( fields,
    me,
    filter (fun ticket -> test_ticket_has_valid_values fields ticket) nearby )

let column_satisfies_field (column : int list) (field : field) =
  CCList.for_all
    (fun v ->
      (* Caml.print_endline [%string " - v %{v#Int}"]; *)
      test_satisfies_field v field)
    column

let to_satisies_bin_matrix fields columns =
  let open CCList in
  map
    (fun field ->
      map (fun column -> column_satisfies_field column field) columns)
    fields

let matrix_apply l f = CCList.(map (fun l' -> map f l') l)

let matrix_applyi l f =
  CCList.(mapi (fun i l' -> mapi (fun j v -> f i j v) l') l)

let bip_sum vars =
  let open Lp in
  let open CCList in
  match vars with
  | [] -> failwith "empty vars summed"
  | [ vars ] -> vars
  | vars -> fold_left ( ++ ) (hd vars) (tl vars)

let solve () =
  let open CCList in
  let open Lp in
  get_newline_delimited_lines () |> parse_train_chaos |> filter_invalid_tickets
  |> fun chaos ->
  let fields, me, nearby_tickets = chaos in
  (*
  format is cM_fN where true = columnN satisfies field N
    c1_f1 c1_f2 .. c1_fn
    c2_f1 c2_f2 .. c2_fn
    c3_f1 c3_f2 .. c3_fn
  *)
  let columns = List.transpose nearby_tickets |> fun v -> Option.value_exn v in
  let satmat = to_satisies_bin_matrix fields columns in
  let bipmat =
    matrix_applyi satmat (fun i j v ->
        let bi = binary [%string "col_field_%{i#Int}_%{j#Int}"] in
        if v then Lp.expand bi one else zero)
  in
  let field_constraints =
    mapi
      (fun fi _ ->
        let cx_fi = map (fun column -> nth column fi) bipmat in
        bip_sum cx_fi <~ c 1.)
      fields
  in
  let column_constraints =
    mapi
      (fun _ column ->
        let ci_fx = mapi (fun fi _ -> nth column fi) fields in
        bip_sum ci_fx <~ c 1.)
      bipmat
  in
  let columns_sum = bip_sum (flatten bipmat) in
  let objective = maximize columns_sum in
  let all_bips_num_fields = columns_sum <~ c 20. in
  let constraints =
    [ all_bips_num_fields ] @ field_constraints @ column_constraints
  in
  let problem = Lp.make objective constraints in
  let solve_lp () =
    match Lp_glpk.solve problem with
    | Ok (obj, xs) ->
        Caml.Printf.printf "Objective: %.2f\n" obj;
        CCList.mapi
          (fun ci column ->
            find_mapi
              (fun fi _ ->
                try
                  let bin = Lp.PMap.find (nth column fi) xs in
                  if Float.equal bin 1. then Some (ci, fi) else None
                with _ -> None)
              fields)
          bipmat
        |> filter_map (fun it -> it)
    | Error msg -> failwith msg
  in
  solve_lp () |> fun coords ->
  map (fun (i, _) -> nth me i) coords |> fun me' ->
  combine fields me' |> fun z ->
  iter
    (fun ((desc, _), v) -> Caml.print_endline [%string "%{desc}, %{v#Int}"])
    z;
  z |> fun myfields ->
  let is_departure_field ((desc, _), _) =
    String.is_substring desc ~substring:"departure"
  in
  filter is_departure_field myfields |> fun my_departure_fields ->
  if length my_departure_fields <> 6 then failwith "bogus departure fields";
  let vals = map (fun (_, it) -> it) my_departure_fields in
  fold_left Int.( * ) 1 vals
