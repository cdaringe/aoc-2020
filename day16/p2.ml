(* this is all garbage. do not reference. i ejected to JS for this exercise *)
open Base
open Fixture

type range = int * int [@@deriving show]

type field = string * range list [@@deriving show]

type int_list = int list [@@deriving show]
type bool_list = bool list [@@deriving show]
type int_int_list = range list [@@deriving show]

type bool_mat = bool array array [@@deriving show]

let range_tuple_of_string s = Caml.Scanf.sscanf s "%d-%d" (fun a b -> (a, b))

let sum_true = CCArray.fold_left (fun a b -> a + if b then 1 else 0) 0

let test_list_in_list needle haystack =
  CCList.(exists (fun l' -> equal Bool.equal l' needle) haystack)

let find_unsolved (mat : bool array array) =
  let open CCArray in
  CCArray.find_map
    (fun col ->
      if sum_true col = 1 then
        let field_i, _ =
          Array.findi col ~f:(fun _ v -> Bool.equal v true) |> fun v ->
          Option.value_exn v
        in
        Some field_i
      else None)
    mat
  |> fun v -> Option.value_exn v

let get_cols_by_field m =
  CCArray.(
    foldi
      (fun acc col_i _ ->
        let field_i = find_unsolved m in
        (* erase the feature column *)
        iter (fun col -> col.(field_i) <- false) m;
        (field_i, col_i) :: acc)
      [] m)

let mapf f = CCList.map f

let parse_ticket line =
  CCString.split_on_char ',' line |> CCList.map Int.of_string

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
      let fields' = parse_fields fields in
      CCList.(fields', parse_ticket (nth me 1), parse_tickets (drop 1 them))
  | _ -> failwith "unexpected ticket format"

let test_is_in_range v (a, b) = v >= a && v <= b

let test_satisfies_field (v : int) ((_, ranges) : field) =
  CCList.exists (test_is_in_range v) ranges

let test_field_exists_for_value fields num =
  CCList.exists (fun field -> test_satisfies_field num field) fields

let test_ticket_has_valid_values fields ticket =
  CCList.for_all (fun num -> test_field_exists_for_value fields num) ticket

let filter_invalid_tickets (fields, me, nearby) =
  CCList.
    ( fields,
      me,
      filter (fun ticket -> test_ticket_has_valid_values fields ticket) nearby
    )

let column_satisfies_field column field =
  CCList.for_all (fun v -> test_satisfies_field v field) column

(* let to_satisies_bin_matrix fields columns =
  CCList.(map
    (fun field ->
      map (fun column -> column_satisfies_field column field) columns
      |> List.to_array)
    fields)
  |> List.to_array *)

let to_satisies_bin_matrix fields columns =
  CCList.(
    map
      (fun c ->
        map (fun f -> column_satisfies_field c f) fields |> List.to_array)
      columns)
  |> List.to_array

let matrix_apply l f = CCList.(map (fun l' -> map f l') l)

let matrix_applyi l f =
  CCList.(mapi (fun i l' -> mapi (fun j v -> f i j v) l') l)

let solve () =
  let open Lp in
  let open CCList in
  get_newline_delimited_lines () |> parse_train_chaos |> filter_invalid_tickets
  |> fun (fields, me, nearby_tickets) ->
  (*
  format is cM_fN where true = column M satisfies field N
    c1_f1 c1_f2 .. c1_fn
    c2_f1 c2_f2 .. c2_fn
    c3_f1 c3_f2 .. c3_fn
  *)
  let columns = List.transpose nearby_tickets |> fun v -> Option.value_exn v in
  let satmat = to_satisies_bin_matrix fields columns in
  Caml.print_endline (show_bool_mat satmat);
  (* CCArray.(iter (fun cf ->Caml.print_endline (show_bool_list cf))  satmat); *)
  get_cols_by_field satmat |> fun coords ->
  let sorted_coords = sort (fun (f1, _) (f2, _) -> Int.compare f1 f2) coords in
  Caml.print_endline (show_int_int_list sorted_coords);
  let my_ordred_ticket =
    map
      (fun (fi, col_i) ->
        Caml.print_endline
          [%string "getting %{fi#Int}th field - col %{col_i#Int}"];
        nth me col_i)
      sorted_coords
  in
  combine fields my_ordred_ticket |> fun ticket ->
  (* debug output *)
  iter
    (fun ((desc, _), v) -> Caml.print_endline [%string "%{desc}, %{v#Int}"])
    ticket;
  ticket |> fun ticket ->
  let is_departure_field ((desc, _), _) =
    String.is_substring desc ~substring:"departure"
  in
  filter is_departure_field ticket |> fun my_departure_fields ->
  if length my_departure_fields <> 6 then failwith "bogus departure fields";
  let vals = map (fun (_, it) -> it) my_departure_fields in
  fold_left Int.( * ) 1 vals
