open Hardcaml
open Signal

module Make (C : Config.S) = struct
  let program_bits = C.program_bits
  let capacity = Config.entries program_bits - 1

  module Input = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      load_finished : 'a;
      program_halted : 'a;
      pc : 'a; [@bits program_bits]
      opcode : 'a; [@bits 8]
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      match_address : 'a; [@bits program_bits]
      start : 'a;
      scan : 'a;
      executing : 'a;
      invalid_program : 'a;
    }
    [@@deriving hardcaml]
  end

  let create (inputs : _ Input.t) =
    let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
    (* Scan after loading, then execute only if the brackets balance. *)
    let phase = wire 2 in
    let loading = phase ==:. 0 in
    let scanning = phase ==:. 1 in
    let executing = phase ==:. 2 in
    let invalid_program = phase ==:. 3 in
    let load_complete =
      loading &: inputs.load_finished
      &: ~:(reg spec inputs.load_finished)
      &: ~:(inputs.reset)
    in
    let scan = scanning &: ~:(inputs.program_halted) &: ~:(inputs.reset) in
    let scan_complete = scanning &: inputs.program_halted &: ~:(inputs.reset) in
    let stack_depth = wire program_bits in
    let opening = Instruction.matches inputs.opcode Instruction.Loop_start in
    let closing = Instruction.matches inputs.opcode Instruction.Loop_end in
    let push = scan &: opening in
    let pop = scan &: closing &: (stack_depth <>:. 0) in
    let unmatched_close = scan &: closing &: (stack_depth ==:. 0) in
    let matching_open =
      memory capacity
        ~write_port:
          {
            Write_port.write_clock = inputs.clock;
            write_enable = push;
            write_address = stack_depth;
            write_data = inputs.pc;
          }
        ~read_address:
          (mux2 (stack_depth ==:. 0) (zero program_bits) (stack_depth -:. 1))
    in
    stack_depth
    <== reg spec
          (stack_depth +: uresize push program_bits -: uresize pop program_bits);
    (* Each closing bracket records both directions of the jump. *)
    let match_address =
      (multiport_memory capacity
         ~write_ports:
           [|
             {
               Write_port.write_clock = inputs.clock;
               write_enable = pop;
               write_address = matching_open;
               write_data = inputs.pc;
             };
             {
               Write_port.write_clock = inputs.clock;
               write_enable = pop;
               write_address = inputs.pc;
               write_data = matching_open;
             };
           |]
         ~read_addresses:[| inputs.pc |]).(0)
    in
    let invalid = reg_fb spec ~width:1 ~f:(fun bad -> bad |: unmatched_close) in
    let bad_brackets = invalid |: (stack_depth <>:. 0) in
    let start_execution = scan_complete &: ~:bad_brackets in
    phase
    <== reg spec
          ~enable:(load_complete |: scan_complete)
          (mux2 load_complete (of_int ~width:2 1)
             (mux2 bad_brackets (of_int ~width:2 3) (of_int ~width:2 2)));
    {
      Output.match_address;
      start = load_complete |: start_execution;
      scan;
      executing;
      invalid_program;
    }
end
