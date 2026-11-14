open Hardcaml
open Signal

module Make_with_memory (C : Config.S) (Memory : Memory.S) = struct
  let pointer_bits = C.pointer_bits

  let () =
    if Memory.address_bits <> pointer_bits then
      invalid_arg "tape and memory address widths must match"

  module Input = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      enable : 'a;
      opcode : 'a; [@bits 8]
      input_byte : 'a; [@bits 8]
      pointer_load : 'a;
      pointer_next : 'a; [@bits pointer_bits]
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

  let create (inputs : _ Input.t) =
    (* Cell writes use the current pointer. A pointer load takes effect after the
     same clock edge, so simultaneous load and write target the old address. *)
    let ready = wire 1 in
    let enable = inputs.enable &: ready in
    let pointer =
      let module Pointer = Pointer.Make (C) in
      Pointer.create
        {
          Pointer.Input.clock = inputs.clock;
          reset = inputs.reset;
          enable;
          opcode = inputs.opcode;
          load = inputs.pointer_load;
          next_address = inputs.pointer_next;
        }
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
      Memory.create ~clock:inputs.clock ~reset:inputs.reset
        ~address:pointer.address
        ~write_enable:(enable &: (increment |: decrement |: input))
        ~write_data
    in
    cell <== memory.data;
    ready <== memory.ready;
    {
      Output.pointer = pointer.address;
      cell;
      ready;
      output_valid =
        enable &: ~:(inputs.reset)
        &: Instruction.matches inputs.opcode Instruction.Output;
    }
end

module Make (C : Config.S) = Make_with_memory (C) (Register_memory.Make (C))
