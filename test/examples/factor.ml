open Runner

let%expect_test "factor" =
  run (example "factor") ~input:"60\n";
  [%expect {| 60: 2 2 3 5 |}]
