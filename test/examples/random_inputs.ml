open Runner

let string_of ~max_length char state =
  String.init (Random.State.int state (max_length + 1)) (fun _ -> char state)

let lowercase state = Char.chr (Char.code 'a' + Random.State.int state 26)

let not_eot state =
  let byte = Random.State.int state 255 in
  Char.chr (if byte >= 4 then byte + 1 else byte)

let%expect_test "upcase uppercases random lowercase strings" =
  check (example "upcase")
    ~generate:(string_of ~max_length:20 lowercase)
    ~input:(fun s -> s ^ "\004")
    ~expected:String.uppercase_ascii;
  [%expect {||}]

let%expect_test "cat echoes random bytes" =
  check (example "cat")
    ~generate:(string_of ~max_length:20 not_eot)
    ~input:(fun s -> s ^ "\004")
    ~expected:Fun.id;
  [%expect {||}]

let%expect_test "add digits sums every pair" =
  check (example "add-digits")
    ~generate:(fun state ->
      (Random.State.int state 10, Random.State.int state 10))
    ~input:(fun (a, b) -> Printf.sprintf "%d%d" a b)
    ~expected:(fun (a, b) -> Printf.sprintf "%d\n" (a + b));
  [%expect {||}]
