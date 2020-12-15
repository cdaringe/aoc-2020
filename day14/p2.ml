open Base
open Fixture

(* addr * value *)
type masked_write = int * int [@@deriving show]

type cmd = string * masked_write list [@@deriving show]

let group_mask_and_writes acc s =
  match acc with
  | (mask, writes) :: rest when String.contains s '[' ->
      (mask, writes @ [ Caml.Scanf.sscanf s "mem[%d] = %d" tuple ]) :: rest
  | _ -> (Caml.Scanf.sscanf s "mask = %s" id |> CCString.trim, []) :: acc

let rec left_pad ch len chars =
  if CCList.length chars < len then left_pad ch len (ch :: chars) else chars

let binary_of_int v =
  let rec bits_of_int ?rem bits v' =
    let rem' = Option.value rem ~default:0 in
    match (bits, v') with
    | [], 0 -> [ 0 ]
    | _, 0 -> bits
    | _ -> bits_of_int (Int.rem v' 2 :: bits) (v' / 2) ~rem:(rem' + 1)
  in
  bits_of_int [] v

let to_binary_chars chars =
  CCList.map
    (function
      | 0 -> '0'
      | 1 -> '1'
      | n -> failwith [%string "invalid binary digit %{n#Int}"])
    chars

let binary_chars v = left_pad '0' 36 (to_binary_chars @@ binary_of_int v)

let apply_addr_mask mask b =
  let f m c = match (m, c) with '0', c -> c | m, _ -> m in
  CCList.map2 f mask b

let apply_mask mask b =
  let f m c = match (m, c) with 'X', c -> c | m, _ -> m in
  CCList.map2 f mask b

let int_of_binary_chars chars =
  let open CCList in
  rev chars
  |> foldi
       (fun acc i c ->
         match c with
         | '0' -> acc
         | '1' -> acc + (2 ** i)
         | c -> failwith [%string "invalid binary char %{c#Char}\n"])
       0

let rec mad_to_bin_chars masked =
  let open CCList in
  let is_x = Char.equal 'X' in
  if find_opt is_x masked |> Option.is_some then
    let a, b = take_drop_while (fun c -> not @@ is_x c) masked in
    let z' = mad_to_bin_chars @@ a @ ('0' :: tl b) in
    let o' = mad_to_bin_chars @@ a @ ('1' :: tl b) in
    z' @ o'
  else
    (* let _ = Caml.print_string (String.of_char_list masked) in
       let _ = Caml.print_string "\n\n" in *)
    [ masked ]

let write_cmd store cmd =
  let open CCList in
  let mask, writes = cmd in
  let of_addr_masked = apply_addr_mask @@ String.to_list mask in
  let f (addr, v) =
    let data = v in
    let masked_addr = binary_chars addr |> of_addr_masked in
    let all_addr_combos = mad_to_bin_chars masked_addr in
    iter
      (fun combo ->
        let key = int_of_binary_chars combo in
        Hashtbl.set store ~key ~data)
      all_addr_combos
  in
  iter f writes

let write_all_cmds store cmds = CCList.iter (write_cmd store) cmds

let solve () =
  let open CCList in
  let store = Hashtbl.Poly.create () in
  get_all_lines ()
  |> fold_left group_mask_and_writes []
  |> rev |> write_all_cmds store
  |> fun _ -> Hashtbl.data store |> fold_left (fun total i -> total + i) 0
