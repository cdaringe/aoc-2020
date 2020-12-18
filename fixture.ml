open Printf

let id x = x
let tuple x y = (x,y)

let line_stream_of_channel : 'a -> string Stream.t =
 fun channel ->
  let read () = input_line channel in
  Stream.from (fun _ -> try Some (read ()) with End_of_file -> Option.None)

let get_input_filename () = match Sys.getenv_opt "INPUT" with None -> "input.txt" | Some x -> x

let get_lines ?on_eof parse_line =
  let lines = ref [] in
  let in_channel = open_in (get_input_filename ()) in
  try
    Stream.iter
      (fun line -> lines := parse_line line :: !lines)
      (line_stream_of_channel in_channel);
    close_in in_channel;
    !lines |> List.rev
  with e ->
    match on_eof with Some f -> f () | _ -> ();
    close_in in_channel;
    raise e

let get_all_lines () = get_lines (fun f -> f)

let group_by_newline lines =
  List.fold_left
    (fun acc s ->
      match String.trim s with
      | "" -> [] :: acc
      | _ -> (
          match acc with
          | hd :: tail -> (s :: hd) :: tail
          | _ -> failwith @@ sprintf "bad lines %s" s ))
    [ [] ] lines

let get_newline_delimited_lines () = get_lines (fun i -> i) |> List.rev |> group_by_newline
