open Runner

let%expect_test "e" =
  run (example "e") ~max_cycles:1_000_000;
  [%expect {|
    2.71828182845
    [cycle limit]
    |}]
