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

let demo_input = [ 16; 10; 15; 5; 1; 11; 7; 19; 6; 12; 4 ]

let test_count_combos_0 () =
  Alcotest.(check int)
    "arst" 8
    (P2.JoltLyfe.count_adapter_combinations demo_input ~device_joltage:22)

let test_count_combos_1 () =
  Alcotest.(check int)
    "arst" 1
    (P2.JoltLyfe.count_adapter_combinations [ 1 ] ~device_joltage:4)

let test_count_combos_2 () =
  Alcotest.(check int)
    "arst" 2
    (P2.JoltLyfe.count_adapter_combinations [ 1; 2 ] ~device_joltage:5)

let test_count_combos_3 () =
  Alcotest.(check int)
    "arst" 4
    (P2.JoltLyfe.count_adapter_combinations [ 1; 2; 3 ] ~device_joltage:6)

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
            ("test_count_combos_0", test_count_combos_0);
            ("test_count_combos_1", test_count_combos_1);
            ("test_count_combos_2", test_count_combos_2);
            ("test_count_combos_3", test_count_combos_3);
          ] );
    ]
