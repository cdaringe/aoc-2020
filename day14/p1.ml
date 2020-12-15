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

let write_cmd store cmd =
  let open CCList in
  let mask, writes = cmd in
  let of_masked = apply_mask @@ String.to_list mask in
  let f (addr, v) =
    let masked = binary_chars v |> of_masked in
    let data = int_of_binary_chars masked in
    Hashtbl.set store ~key:addr ~data
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
