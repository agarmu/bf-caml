open Runner

let%expect_test "add digits" =
  run (example "add-digits") ~input:"34";
  [%expect {| 7 |}]

let%expect_test "cat" =
  run (example "cat") ~input:"hello\004";
  [%expect {| hello |}]

let%expect_test "cat stops at first ^D" =
  run (example "cat") ~input:"hi\004ignored";
  [%expect {| hi |}]

let%expect_test "upcase" =
  run (example "upcase") ~input:"abc\004";
  [%expect {| ABC |}]

let%expect_test "echo one byte" =
  run ",." ~input:"x";
  [%expect {| x |}]
