open Runner

let thue_morse length =
  let rec ones n = if n = 0 then 0 else (n land 1) + ones (n lsr 1) in
  String.init length (fun i -> if ones i mod 2 = 0 then '0' else '1')

let%expect_test "thue" =
  check_prefix ~setups:extremes (example "thue") ~max_cycles:50_000
    ~expected:(thue_morse 100_000);
  [%expect {| 434 characters match |}]
