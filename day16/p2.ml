open Base
open Fixture

type range = int * int

type rule = string * range list

type int_list = int list [@@deriving show]

let range_tuple_of_string s = Caml.Scanf.sscanf s "%d-%d" (fun a b -> (a, b))

let mapf f = CCList.map f

let tap_assert_l_20 l =
  if CCList.length l = 20 then l else failwith "invalid list"

let parse_ticket line =
  CCString.split_on_char ',' line |> CCList.map Int.of_string |> tap_assert_l_20

let parse_rule : string -> rule =
 fun rule_str ->
  let open CCString in
  let fail () = failwith [%string "invalid rule %{rule_str}"] in
  match split_on_char ':' rule_str with
  | [ desc; constraints ] ->
      split ~by:" or " constraints |> mapf trim |> mapf range_tuple_of_string
      |> fun constraints' -> (desc, constraints')
  | _ -> fail ()

let parse_rules = mapf parse_rule

let parse_tickets = mapf parse_ticket

let parse_train_chaos lines =
  match lines with
  | [ rules; me; them ] ->
      let open CCList in
      let rules' = parse_rules rules in
      (rules', parse_ticket (nth me 1), parse_tickets (drop 1 them))
  | _ -> failwith "unexpected ticket format"

let test_is_in_range v (a, b) = v >= a && v <= b

let test_satisfies_rule (v : int) ((_, ranges) : rule) =
  CCList.exists (test_is_in_range v) ranges

let test_satisfies_rule_opt (v : int) ((_, ranges) : rule) =
  CCList.find_opt (test_is_in_range v) ranges

let filter_invalid_ticket rules ticket_numbers =
  let open CCList in
  let test_num_satisfies_no_rules num =
    let test_num_satisfies_rule rule = test_satisfies_rule_opt num rule in
    find_map test_num_satisfies_rule rules |> Option.is_some
  in
  filter test_num_satisfies_no_rules ticket_numbers

let filter_invalid_tickets (rules, me, nearby) =
  let open CCList in
  let nearby' =
    filter
      (fun ticket_nums ->
        length (filter_invalid_ticket rules ticket_nums) <> 20)
      nearby
  in
  (rules, me, map tap_assert_l_20 nearby')

let column_satisfies_rule (column : int list) (rule : rule) =
  CCList.for_all (fun v ->
    Caml.print_endline [%string " - v %{v#Int}"];
    test_satisfies_rule v rule) column

let rec satisfy_constraints ?(min_col = 0) state rules free_columns =
  let open CCList in
  match
    ( Array.findi ~f:(fun i x -> Option.is_none x && i >= min_col) state,
      head_opt free_columns )
  with
  | None, _ | _, None -> None
  | Some (free_i, _), Some column ->
      let try_next () =
        satisfy_constraints state rules free_columns ~min_col:(free_i + 1)
      in
      Caml.print_endline [%string "trying col at index %{free_i#Int}"];
      Caml.print_endline (show_int_list column);
      if column_satisfies_rule column (nth rules free_i) then (
        state.(free_i) <- Some column;
        match satisfy_constraints state rules (tl free_columns) with
        | None ->
            state.(free_i) <- None;
            try_next ()
        | Some _ -> Some true )
      else try_next ()

let solve () =
  let open CCList in
  get_newline_delimited_lines () |> parse_train_chaos |> filter_invalid_tickets
  |> fun chaos ->
  let rules, me, nearby = chaos in
  let num_rules = length rules in
  let state = Array.(init num_rules ~f:(fun _ -> None)) in
  let tickets = me :: nearby in
  iter (fun x -> Caml.print_endline (show_int_list x)) tickets;
  Caml.print_endline "\n\n--------\n";
  let columns = List.transpose tickets |> fun v -> Option.value_exn v in
  iter (fun x -> Caml.print_endline (show_int_list x)) columns;
  satisfy_constraints state rules columns |> function
  | Some _ -> 111
  | None -> failwith "no solution found"
