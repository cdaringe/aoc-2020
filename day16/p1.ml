open Base
open Fixture

type range = int * int

type rule = string * range list

let range_tuple_of_string s = Caml.Scanf.sscanf s "%d-%d" (fun a b -> (a, b))

let mapf f = CCList.map f

let parse_ticket line =
  CCString.split_on_char ',' line |> CCList.map Int.of_string

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

let test_satisfies_rule (v : int) ((_, ranges) : rule) =
  let open CCList in
  let test_is_in_range (a, b) = v >= a && v <= b in
  find_opt test_is_in_range ranges

let extract_invalid_values rules ticket_numbers =
  let open CCList in
  let test_num_satisfies_no_rules num =
    let test_num_satisfies_rule rule = test_satisfies_rule num rule in
    find_map test_num_satisfies_rule rules |> function
    | Some _ -> None
    | None -> Some num
  in
  filter_map test_num_satisfies_no_rules ticket_numbers

let find_invalid_ticket_values (rules, _, nearby) =
  CCList.flat_map
    (fun ticket_nums -> extract_invalid_values rules ticket_nums)
    nearby

let solve () =
  let open CCList in
  get_newline_delimited_lines ()
  |> parse_train_chaos |> find_invalid_ticket_values |> fold_left ( + ) 0
