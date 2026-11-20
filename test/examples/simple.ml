open Runner

let%expect_test "hello world" =
  run (example "hello-world");
  [%expect {| Hello World! |}]

let%expect_test "countdown" =
  run (example "countdown");
  [%expect {| 9876543210 |}]

let%expect_test "tape wrap" =
  run (example "tape-wrap");
  [%expect {| OK |}]

let%expect_test "empty program" =
  run "";
  [%expect {||}]

let%expect_test "comments only" =
  run "no instructions here";
  [%expect {||}]

let%expect_test "skipped loop" =
  run "[+.]";
  [%expect {||}]
