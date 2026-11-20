open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 4096
  let tape_length = 1024
end)

module Sim =
  Simulation.Make (Config) (Ram_memory) (Source.Loaded (Config) (Ram_memory))

let reader input =
  let pending = ref (String.to_seq input) in
  fun () ->
    Option.map
      (fun (byte, rest) ->
        pending := rest;
        byte)
      (Seq.uncons !pending)

let output ?(input = "") source =
  let buffer = Buffer.create 64 in
  match Sim.run ~read:(reader input) ~write:(Buffer.add_char buffer) source with
  | Ok _ -> Buffer.contents buffer
  | Error _ -> "error"

let run ?input source = print_string (output ?input source)

let example name =
  In_channel.with_open_bin ("../examples/" ^ name ^ ".bf") In_channel.input_all

let check ?(count = 50) ~generate ~input ~expected source =
  let state = Random.State.make [| 0 |] in
  for _ = 1 to count do
    let case = generate state in
    let actual = output ~input:(input case) source in
    if actual <> expected case then
      Printf.printf "input %S: got %S, expected %S\n" (input case) actual
        (expected case)
  done

let string_of ~max_length char state =
  String.init (Random.State.int state (max_length + 1)) (fun _ -> char state)

let lowercase state = Char.chr (Char.code 'a' + Random.State.int state 26)

let not_eot state =
  let byte = Random.State.int state 255 in
  Char.chr (if byte >= 4 then byte + 1 else byte)
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

let%expect_test "factor" =
  run (example "factor") ~input:"60\n";
  [%expect {| 60: 2 2 3 5 |}]

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
