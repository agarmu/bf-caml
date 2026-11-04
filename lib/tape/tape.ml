open Hardcaml
open Signal

module Make (C : Config.S) = struct
  let pointer_bits = C.pointer_bits
  let cell_count = Config.entries pointer_bits

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
      cells : 'a; [@bits cell_count * 8]
      output_valid : 'a;
    }
    [@@deriving hardcaml]
  end

  let create (inputs : _ Input.t) =
    (* Cell writes use the current pointer. A pointer load takes effect after the
     same clock edge, so simultaneous load and write target the old address. *)
    let pointer =
      let module Pointer = Pointer.Make (C) in
      Pointer.create
        {
          Pointer.Input.clock = inputs.clock;
          reset = inputs.reset;
          enable = inputs.enable;
          opcode = inputs.opcode;
          load = inputs.pointer_load;
          next_address = inputs.pointer_next;
        }
    in
    let cells =
      List.init cell_count (fun index ->
          let selected = pointer.address ==: of_int ~width:pointer_bits index in
          (Cell.create
             {
               Cell.Input.clock = inputs.clock;
               reset = inputs.reset;
               enable = inputs.enable &: selected;
               opcode = inputs.opcode;
               input_byte = inputs.input_byte;
             })
            .cell)
    in
    {
      Output.pointer = pointer.address;
      cell = mux pointer.address cells;
      cells = concat_lsb cells;
      output_valid =
        inputs.enable &: ~:(inputs.reset)
        &: Instruction.matches inputs.opcode Instruction.Output;
    }
end
