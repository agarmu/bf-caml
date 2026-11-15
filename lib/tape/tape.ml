open Hardcaml
open Signal

module Make_with_memory (C : Config.S) (Memory : Memory.S) = struct
  let pointer_bits = C.pointer_bits

  module Input = struct
    type 'a t = {
      enable : 'a;
      opcode : 'a; [@bits 8]
      input_byte : 'a; [@bits 8]
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      pointer : 'a; [@bits pointer_bits]
      cell : 'a; [@bits 8]
      ready : 'a;
      output_valid : 'a;
    }
    [@@deriving hardcaml]
  end

  let create ~clock ~reset (inputs : _ Input.t) =
    let ready = wire 1 in
    let enable = inputs.enable &: ready in
    let move_right =
      enable &: Instruction.matches inputs.opcode Instruction.Move_right
    in
    let move_left =
      enable &: Instruction.matches inputs.opcode Instruction.Move_left
    in
    let pointer =
      reg_fb (Reg_spec.create ~clock ~clear:reset ())
        ~enable:(move_right |: move_left) ~width:pointer_bits ~f:(fun current ->
          current
          +: uresize move_right pointer_bits
          -: uresize move_left pointer_bits)
    in
    let cell = wire 8 in
    let increment = Instruction.matches inputs.opcode Instruction.Increment in
    let decrement = Instruction.matches inputs.opcode Instruction.Decrement in
    let input = Instruction.matches inputs.opcode Instruction.Input in
    let write_data =
      mux2 input inputs.input_byte
        (cell +: uresize increment 8 -: uresize decrement 8)
    in
    let memory =
      Memory.create ~address_bits:pointer_bits ~clear:reset ~clock ~reset
        ~address:pointer
        ~write_enable:(enable &: (increment |: decrement |: input))
        ~write_data
    in
    cell <== memory.data;
    ready <== memory.ready;
    {
      Output.pointer;
      cell;
      ready;
      output_valid =
        enable &: ~:reset
        &: Instruction.matches inputs.opcode Instruction.Output;
    }
end

module Make (C : Config.S) = Make_with_memory (C) (Register_memory)
