open Hardcaml
open Signal

module Input = struct
  type 'a t = {
    clock : 'a;
    reset : 'a;
    enable : 'a;
    opcode : 'a; [@bits 8]
    load : 'a;
    next_address : 'a; [@bits 4]
  }
  [@@deriving hardcaml]
end

module Output = struct
  type 'a t = { address : 'a [@bits 4] } [@@deriving hardcaml]
end

let create (inputs : _ Input.t) =
  let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
  let move_right =
    inputs.enable &: Instruction.matches inputs.opcode Instruction.Move_right
  in
  let move_left =
    inputs.enable &: Instruction.matches inputs.opcode Instruction.Move_left
  in
  let movement = uresize move_right 4 -: uresize move_left 4 in
  let update = inputs.load |: move_right |: move_left in
  (* Loading an address takes priority over a move on the same clock edge. *)
  let address =
    reg_fb spec ~enable:update ~width:4 ~f:(fun current ->
        mux2 inputs.load inputs.next_address (current +: movement))
  in
  { Output.address }
