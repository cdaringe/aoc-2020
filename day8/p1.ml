open Base
open Fixture

module Boot = struct
  module Cmd = struct
    type t = string * int

    let of_string s = Caml.Scanf.sscanf s "%s %d" (fun cmd n -> (cmd, n))
  end

  let test_cmd_invoked visits i = visits.(i) |> Option.is_some

  let run (cmds : Cmd.t list) ~until =
    let open Array in
    let visits = init (List.length cmds) ~f:(fun _ -> None) in
    let rec run_cmd cmdi state =
      let cmd = List.nth_exn cmds cmdi in
      if until visits cmdi then (cmd, state)
      else
        let _ = visits.(cmdi) <- Some 1 in
        match cmd with
        | "acc", n -> run_cmd (cmdi + 1) (state + n)
        | "jmp", n -> run_cmd (cmdi + n) state
        | "nop", _ -> run_cmd (cmdi + 1) state
        | _ -> failwith "cmd not found"
    in
    run_cmd 0 0
end

let solve () =
  get_lines Boot.Cmd.of_string |> Boot.run ~until:Boot.test_cmd_invoked
  |> fun (_, state) -> state
