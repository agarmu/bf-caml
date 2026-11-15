open Hardcaml
open Signal

module Make_with_memory (C : Config.S) (Memory : Memory.S) = struct
  module Loader = Loader.Make (C)
  module Program = Program.Make (C)
  module Brackets = Brackets.Make (C)
  module Tape = Tape.Make_with_memory (C) (Memory)
  module Pipeline = Instruction_pipeline.Make (C)

  let program_bits = C.program_bits
  let pointer_bits = C.pointer_bits

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
      input_ready : 'a;
      output_ready : 'a;
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      pc : 'a; [@bits program_bits]
      opcode : 'a; [@bits 8]
      halted : 'a;
      pointer : 'a; [@bits pointer_bits]
      cell : 'a; [@bits 8]
      invalid_program : 'a;
      input_needed : 'a;
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
    let running =
      inputs.step &: brackets.ready &: ~:(program.halted) &: ~:program_reset
    in
    let cell = wire 8 in
    let cell_ready = wire 1 in
    let execution_ready = wire 1 in
    let instruction =
      Pipeline.create ~clock:inputs.clock ~reset:program_reset ~enable:running
        ~pc:program.pc ~opcode:program.opcode ~cell ~cell_ready
        ~ready:execution_ready
    in
    let opcode = instruction.value.opcode in
    let is_input = Instruction.matches opcode Instruction.Input in
    let is_output = Instruction.matches opcode Instruction.Output in
    let tape =
      Tape.create
        {
          Tape.Input.clock = inputs.clock;
          reset = inputs.reset;
          enable = execute_step;
          opcode;
          input_byte = inputs.input_byte;
          pointer_load = gnd;
          pointer_next = zero pointer_bits;
        }
    in
    cell <== tape.cell;
    cell_ready <== tape.ready;
    let run_step = running &: instruction.valid in
    execution_ready
    <== (running
        &: (~:is_input |: inputs.input_ready)
        &: (~:is_output |: inputs.output_ready));
    execute_step <== (instruction.valid &: execution_ready);
    start <== brackets.start;
    step <== (brackets.scan |: execute_step);
    let opening = Instruction.matches opcode Instruction.Loop_start in
    let closing = Instruction.matches opcode Instruction.Loop_end in
    jump
    <== (execute_step
        &: (opening
           &: (instruction.value.cell ==:. 0)
           |: (closing &: (instruction.value.cell <>:. 0))));
    jump_address <== brackets.match_address +:. 1;
    {
      Output.pc = mux2 instruction.valid instruction.value.pc program.pc;
      opcode = mux2 instruction.valid opcode program.opcode;
      halted = brackets.ready &: program.halted |: brackets.invalid_program;
      pointer = tape.pointer;
      cell = tape.cell;
      invalid_program = brackets.invalid_program;
      input_needed = run_step &: is_input;
      output_valid = run_step &: is_output;
      output_byte = instruction.value.cell;
    }
end

module Make (C : Config.S) = Make_with_memory (C) (Register_memory.Make (C))
