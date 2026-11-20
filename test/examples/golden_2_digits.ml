open Runner

let%expect_test "golden" =
  run (example "golden") ~max_cycles:200_000;
  [%expect {|
    1.6
    [cycle limit]
    |}]
