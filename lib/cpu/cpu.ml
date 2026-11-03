open Hardcaml
open Signal

module Input = struct
  type 'a t = {
    clock : 'a;
    reset : 'a;
    new_program : 'a;
    byte_valid : 'a;
    byte : 'a; [@bits 8]
    finish : 'a;
    step : 'a;
    input_byte : 'a; [@bits 8]
  }
  [@@deriving hardcaml]
end

module Output = struct
  type 'a t = {
    pc : 'a; [@bits 8]
    opcode : 'a; [@bits 8]
    halted : 'a;
    pointer : 'a; [@bits 4]
    cells : 'a; [@bits 128]
    output_valid : 'a;
    output_byte : 'a; [@bits 8]
  }
  [@@deriving hardcaml]
end

let create (inputs : _ Input.t) =
  let program_reset = inputs.reset |: inputs.new_program in
  let loader =
    Loader.create
      {
        Loader.Input.clock = inputs.clock;
        reset = program_reset;
        byte_valid = inputs.byte_valid;
        byte = inputs.byte;
        finish = inputs.finish;
      }
  in
  (* Start once, on the first clock after loading finishes. *)
  let spec = Reg_spec.create ~clock:inputs.clock ~clear:program_reset () in
  let start =
    loader.finished &: ~:(reg spec loader.finished) &: ~:program_reset
  in
  let program =
    Program.create
      {
        Program.Input.clock = inputs.clock;
        reset = program_reset;
        load = loader.load;
        load_address = loader.load_address;
        load_opcode = loader.load_opcode;
        program_size = loader.program_size;
        start;
        step = inputs.step;
      }
  in
  (* The fetched opcode executes on the same edge that advances the program. *)
  let execute = inputs.step &: ~:(program.halted) &: ~:program_reset in
  let tape =
    Tape.create
      {
        Tape.Input.clock = inputs.clock;
        reset = inputs.reset;
        enable = execute;
        opcode = program.opcode;
        input_byte = inputs.input_byte;
        pointer_load = gnd;
        pointer_next = zero 4;
      }
  in
  {
    Output.pc = program.pc;
    opcode = program.opcode;
    halted = program.halted;
    pointer = tape.pointer;
    cells = tape.cells;
    output_valid = reg spec tape.output_valid;
    (* [.] leaves the selected cell unchanged across the clock edge. *)
    output_byte = tape.cell;
  }
