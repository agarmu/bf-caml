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
  let cell =
    (* On each clock edge, we:
       - go to 0 if `reset` is high
       - increment if `should_incr` is high
       - otherwise, keep the same value
    *)
    reg_fb spec ~enable:should_incr ~width:8 ~f:(fun value -> value +:. 1)
  in
  (* expose the register value as the cell output *)
  { Output.cell }
