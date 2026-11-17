open Hardcaml
open Helpers

module Cases = struct
  type t =
    | Move_right
    | Move_left
    | Increment
    | Decrement
    | Output
    | Input
    | Loop_start
    | Loop_end
  [@@deriving compare, enumerate, sexp_of]
end

include Cases
module Opcode = Enum.Make_binary (Cases)

let bits = Opcode.to_raw Opcode.port_widths
let is = Opcode.Of_signal.is

let to_char = function
  | Move_right -> '>'
  | Move_left -> '<'
  | Increment -> '+'
  | Decrement -> '-'
  | Output -> '.'
  | Input -> ','
  | Loop_start -> '['
  | Loop_end -> ']'

let of_char c = List.find_opt (fun op -> to_char op = c) all
let encode = Opcode.to_raw %> Opcode.Of_signal.of_enum

let decode byte =
  let hit op =
    let code = Char.code (to_char op) in
    { With_valid.valid = Signal.(byte ==:. code); value = encode op }
  in
  Signal.priority_select (List.map hit all)
