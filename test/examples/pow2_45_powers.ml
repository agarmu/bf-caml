open Runner

let powers_of_two =
  String.concat "" (List.init 62 (fun k -> Printf.sprintf "%d\n" (1 lsl k)))

let%expect_test "pow2" =
  check_prefix (example "pow2") ~max_cycles:300_000 ~expected:powers_of_two;
  [%expect {| 367 characters match |}]
