open Hardcaml
open Signal

(* wires entering the cell*)
module Input = struct
  type 'a t = { clock : 'a; reset : 'a; enable : 'a; opcode : 'a [@bits 8] }
  [@@deriving hardcaml]
end

(* output the current value of the cell *)
module Output = struct
  type 'a t = { cell : 'a [@bits 8] } [@@deriving hardcaml]
end

(* build the cell register from the input wires *)
let create (inputs : _ Input.t) =
  (* Create a register specification using `inputs.clock` as clock, and to reset
     if `inputs.reset` is high
  *)
  let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
  let should_incr =
    inputs.enable &: Instruction.matches inputs.opcode Instruction.Increment
  in
  let should_decr =
    inputs.enable &: Instruction.matches inputs.opcode Instruction.Decrement
  in
  let change = uresize should_incr 8 -: uresize should_decr 8 in
  let cell =
    (* On each clock edge, we:
       - go to 0 if `reset` is high
       - increment or decrement for the corresponding opcode
       - otherwise, keep the same value
    *)
    reg_fb spec ~enable:(should_incr |: should_decr) ~width:8 ~f:(fun value ->
        value +: change)
  in
  (* expose the register value as the cell output *)
  { Output.cell }
