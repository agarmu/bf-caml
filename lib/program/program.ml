open Hardcaml
open Signal

module Make (C : Config.S) = struct
  let program_bits = C.program_bits
  let capacity = Config.entries program_bits - 1

  (* Load one opcode per clock edge. [program_size] is the number of loaded opcodes,
   from 0 to [capacity]. [start] begins a run; [step] advances the counter. *)
  module Input = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      load : 'a;
      load_address : 'a; [@bits program_bits]
      load_opcode : 'a; [@bits 8]
      program_size : 'a; [@bits program_bits]
      start : 'a;
      step : 'a;
      jump : 'a;
      jump_address : 'a; [@bits program_bits]
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      pc : 'a; [@bits program_bits]
      opcode : 'a; [@bits 8]
      halted : 'a;
    }
    [@@deriving hardcaml]
  end

  let create (inputs : _ Input.t) =
    let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
    (* These registers reset to zero, so the program starts halted. *)
    let pc = wire program_bits in
    let running = wire 1 in
    (* The opcode at [pc] is available without advancing the program. *)
    let opcode =
      memory capacity
        ~write_port:
          {
            Write_port.write_clock = inputs.clock;
            (* The count has room for [capacity], which lies outside the store. *)
            write_enable = inputs.load &: (inputs.load_address <:. capacity);
            write_address = inputs.load_address;
            write_data = inputs.load_opcode;
          }
        ~read_address:pc
    in
    let advance = inputs.step &: running &: ~:(inputs.load) in
    (* A jump to [program_size] halts; otherwise the final sequential step halts. *)
    let last = pc ==: inputs.program_size -:. 1 in
    let continue =
      mux2 inputs.jump (inputs.jump_address <: inputs.program_size) ~:last
    in
    let next_pc = mux2 inputs.jump inputs.jump_address (pc +:. 1) in
    pc
    <== reg spec
          ~enable:(inputs.start |: (advance &: continue))
          (mux2 inputs.start (zero program_bits) next_pc);
    running
    <== reg spec ~enable:(inputs.start |: advance)
          (mux2 inputs.start (inputs.program_size <>:. 0) continue);
    { Output.pc; opcode; halted = ~:running }
end
