open Helpers
open Hardcaml.Signal

(*
  Use named opcodes in OCaml to make writing code easier
  Though the circuit will receive/work with the raw characters
*)
type t =
  | Move_right
  | Move_left
  | Increment
  | Decrement
  | Output
  | Input
  | Loop_start
  | Loop_end

let opcode_chars : (t * char) list =
  [
    (Move_right, '>');
    (Move_left, '<');
    (Increment, '+');
    (Decrement, '-');
    (Output, '.');
    (Input, ',');
    (Loop_start, '[');
    (Loop_end, ']');
  ]

let char_opcodes = List.map Pair.swap opcode_chars
let of_char c = List.assoc_opt c char_opcodes
let to_char c = List.assoc c opcode_chars
let to_bits = Hardcaml.Bits.of_char %> to_char

let matches signal opcode =
  let expected = Hardcaml.Signal.of_char (to_char opcode) in
  signal ==: expected
