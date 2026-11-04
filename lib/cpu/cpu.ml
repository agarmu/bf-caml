open Hardcaml
open Signal

module Make (C : Config.S) = struct
  module Loader = Loader.Make (C)
  module Program = Program.Make (C)
  module Brackets = Brackets.Make (C)
  module Tape = Tape.Make (C)

  let program_bits = C.program_bits
  let pointer_bits = C.pointer_bits
  let cell_count = Config.entries pointer_bits

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
      pc : 'a; [@bits program_bits]
      opcode : 'a; [@bits 8]
      halted : 'a;
      pointer : 'a; [@bits pointer_bits]
      cells : 'a; [@bits cell_count * 8]
      invalid_program : 'a;
      input_needed : 'a;
      output_valid : 'a;
      output_byte : 'a; [@bits 8]
    }
    [@@deriving hardcaml]
  end

  let create (inputs : _ Input.t) =
    let program_reset = inputs.reset |: inputs.new_program in
    let spec = Reg_spec.create ~clock:inputs.clock ~clear:program_reset () in
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
    (* Connect these after Program exposes its counter and opcode. *)
    let start = wire 1 in
    let step = wire 1 in
    let execute_step = wire 1 in
    let jump = wire 1 in
    let jump_address = wire program_bits in
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
          step;
          jump;
          jump_address;
        }
    in
    let brackets =
      Brackets.create
        {
          Brackets.Input.clock = inputs.clock;
          reset = program_reset;
          load_finished = loader.finished;
          program_halted = program.halted;
          pc = program.pc;
          opcode = program.opcode;
        }
    in
    execute_step
    <== (inputs.step &: brackets.executing &: ~:(program.halted)
       &: ~:program_reset);
    let tape =
      Tape.create
        {
          Tape.Input.clock = inputs.clock;
          reset = inputs.reset;
          enable = execute_step;
          opcode = program.opcode;
          input_byte = inputs.input_byte;
          pointer_load = gnd;
          pointer_next = zero pointer_bits;
        }
    in
    start <== brackets.start;
    step <== (brackets.scan |: execute_step);
    let opening = Instruction.matches program.opcode Instruction.Loop_start in
    let closing = Instruction.matches program.opcode Instruction.Loop_end in
    jump
    <== (execute_step
        &: (opening &: (tape.cell ==:. 0) |: (closing &: (tape.cell <>:. 0))));
    jump_address <== brackets.match_address +:. 1;
    {
      Output.pc = program.pc;
      opcode = program.opcode;
      halted = brackets.executing &: program.halted |: brackets.invalid_program;
      pointer = tape.pointer;
      cells = tape.cells;
      invalid_program = brackets.invalid_program;
      input_needed =
        execute_step &: Instruction.matches program.opcode Instruction.Input;
      output_valid = reg spec tape.output_valid;
      (* [.] leaves the selected cell unchanged across the clock edge. *)
      output_byte = tape.cell;
    }
end
