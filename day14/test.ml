open Printf
open Base
open CCList

let _ =
  Caml.print_string @@ sprintf "";
  List.append

let binary_of_int_cases =
  [
    (P1.binary_of_int 0, [ 0 ]);
    (P1.binary_of_int 1, [ 1 ]);
    (P1.binary_of_int 2, [ 1; 0 ]);
    (P1.binary_of_int 3, [ 1; 1 ]);
    (P1.binary_of_int 4, [ 1; 0; 0 ]);
    (P1.binary_of_int 8, [ 1; 0; 0; 0 ]);
  ]

let binary_of_int_tests =
  mapi
    (fun j (i, o) ->
      let open Alcotest in
      let desc = [%string "binary_of_int_%{j#Int}"] in
      let t () = Alcotest.(check (list int)) desc o i in
      test_case desc `Quick t)
    binary_of_int_cases

let test_left_pad () =
  Alcotest.(check (list char))
    "lp"
    [ '*'; '*'; '*'; 'a'; 'b' ]
    (P1.left_pad '*' 5 [ 'a'; 'b' ])

let test_binary_chars () =
  Alcotest.(check (list char))
    "binchars"
    [
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '0';
      '1';
      '0';
      '0';
      '0';
    ]
    (P1.binary_chars 8)

let () =
  let open Alcotest in
  run "test"
    [
      ("binary_of_int", binary_of_int_tests);
      ("left_pad", [ test_case "" `Quick test_left_pad ]);
      ("test_binary_chars", [ test_case "" `Quick test_binary_chars ]);
    ]
