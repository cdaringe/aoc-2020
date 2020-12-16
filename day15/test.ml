open Printf
open Base

let _ =
  Caml.print_string @@ sprintf "";
  List.append

let test_of_thing () = Alcotest.(check int) "arst" 1 1

let () =
  let open Alcotest in
  run "test"
    [ ("fns", [ test_case "test_int_of_binary_ints" `Quick test_of_thing ]) ]
