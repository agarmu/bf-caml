open Bf_caml

let describe max_program_size tape_length =
  match
    let module C = Config.Make (struct
      let max_program_size = max_program_size
      let tape_length = tape_length
    end) in
    (C.program_bits, C.pointer_bits, C.program_capacity)
  with
  | program_bits, pointer_bits, capacity ->
      Printf.printf "%d/%d: program_bits=%d pointer_bits=%d capacity=%d\n"
        max_program_size tape_length program_bits pointer_bits capacity
  | exception Invalid_argument message ->
      Printf.printf "%d/%d: %s\n" max_program_size tape_length message

let%expect_test "valid sizes" =
  describe 2 2;
  describe 64 16;
  describe 32768 32768;
  [%expect
    {|
    2/2: program_bits=1 pointer_bits=1 capacity=1
    64/16: program_bits=6 pointer_bits=4 capacity=63
    32768/32768: program_bits=15 pointer_bits=15 capacity=32767
    |}]

let%expect_test "invalid sizes" =
  describe 0 16;
  describe 1 16;
  describe 3 16;
  describe 64 100;
  describe 64 (-8);
  [%expect
    {|
    0/16: max_program_size must be a power of two of at least 2
    1/16: max_program_size must be a power of two of at least 2
    3/16: max_program_size must be a power of two of at least 2
    64/100: tape_length must be a power of two of at least 2
    64/-8: tape_length must be a power of two of at least 2
    |}]
