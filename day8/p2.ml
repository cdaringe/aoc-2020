open Base
open Fixture

module Boot = struct
  module Cmd = struct
    type t = string * int

    let of_string s = Caml.Scanf.sscanf s "%s %d" (fun cmd n -> (cmd, n))
  end

  let test_cmd_invoked visits i = visits.(i) |> Option.is_some

  let get_next_hack cmds curr =
    let num_cmds = Array.length cmds in
    let toggle cmd = if String.equal "jmp" cmd then "nop" else "jmp" in
    Array.findi cmds ~f:(fun i (cmd, _) ->
        match cmd with
        | ("nop" | "jmp") when curr < i && i < num_cmds -> true
        | _ -> false)
    |> function
    | Some (i, (cmd, n)) ->
        let next_cmds = Array.copy cmds in
        next_cmds.(i) <- (toggle cmd, n);
        (i, next_cmds)
    | None -> failwith "no remaining commands to hack"

  let run (cmds : Cmd.t array) =
    let open Array in
    let num_cmds = length cmds in
    let create_visits () = init num_cmds ~f:(fun _ -> None) in
    let visits = ref (create_visits ()) in
    let last_fix_guess_idx = ref (-1) in
    let rec run_cmd ~cmds' cmdi state =
      if test_cmd_invoked !visits cmdi then (
        let next_guess, next_cmds = get_next_hack cmds !last_fix_guess_idx in
        visits := create_visits ();
        last_fix_guess_idx := next_guess;
        run_cmd ~cmds':next_cmds 0 0 )
      else
        let _ = !visits.(cmdi) <- Some 1 in
        ( match cmds'.(cmdi) with
        | "acc", n -> (cmdi + 1, state + n)
        | "jmp", n -> (cmdi + n, state)
        | "nop", _ -> (cmdi + 1, state)
        | _ -> failwith "invalid cmd" )
        |> fun (next_i, next_state) ->
        if next_i = num_cmds then next_state
        else run_cmd ~cmds' next_i next_state
    in
    run_cmd ~cmds':cmds 0 0
end

let solve () = get_lines Boot.Cmd.of_string |> List.to_array |> Boot.run
