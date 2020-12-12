open Base

let _ =
  let name = "wee" in
  let huh = 2 in
  Caml.print_string [%string "Hello %{huh#Int} %{name}!\n"];
  List.append

let test_jolt_get_compatible () =
  Alcotest.(check (list int))
    "arst" [ 1; 2; 3 ]
    (P1.JoltLyfe.get_compatible 0 [ 0; 1; 2; 3; 4; 5 ])

let test_jolt_get_compatible_2 () =
  Alcotest.(check (list int))
    "arst" [ 2; 3; 4 ]
    (P1.JoltLyfe.get_compatible 1 [ 0; 1; 2; 3; 4; 5 ])

let test_jolt_get_compatible_3 () =
  Alcotest.(check (list int))
    "arst" [ 2; 3; 4; 4 ]
    (P1.JoltLyfe.get_compatible 1 [ 0; 1; 2; 3; 4; 4; 5 ])

let test_jolt_find_graph_1 () =
  Alcotest.(check (option (list int)))
    "arst" (Some [])
    (P1.JoltLyfe.find_graph [] ~device_joltage:3)

let test_jolt_find_graph_2 () =
  Alcotest.(check (option (list int)))
    "arst"
    (Some [ 1 ])
    (P1.JoltLyfe.find_graph [ 1 ] ~device_joltage:4)

let test_jolt_find_graph_3 () =
  Alcotest.(check (option (list int)))
    "arst"
    (Some [ 1; 2; 5 ])
    (P1.JoltLyfe.find_graph [ 1; 2; 5 ] ~device_joltage:8)

let test_jolt_find_graph_4 () =
  Alcotest.(check (option (list int)))
    "arst"
    (Some [ 2; 5; 8; 11; 12 ])
    (P1.JoltLyfe.find_graph [ 2; 5; 8; 11; 12 ] ~device_joltage:15)

let () =
  let open Alcotest in
  run "test"
    [
      ( "fns",
        List.map
          ~f:(fun (name, x) -> test_case name `Quick x)
          [
            ("test_jolt_get_compatible", test_jolt_get_compatible);
            ("test_jolt_get_compatible_2", test_jolt_get_compatible_2);
            ("test_jolt_get_compatible_3", test_jolt_get_compatible_3);
            ("test_jolt_find_graph_1", test_jolt_find_graph_1);
            ("test_jolt_find_graph_2", test_jolt_find_graph_2);
            ("test_jolt_find_graph_3", test_jolt_find_graph_3);
            ("test_jolt_find_graph_4", test_jolt_find_graph_4);
          ] );
    ]
