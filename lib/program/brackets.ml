open Hardcaml
open Signal

module State = struct
  type t = Waiting_for_program | Scanning | Ready | Invalid_program
  [@@deriving compare, enumerate, sexp_of]
end

module Make (C : Config.S) = struct
  let program_bits = C.program_bits
  let capacity = C.program_capacity

  module Address = struct
    type 'a t = { address : 'a [@bits program_bits] } [@@deriving hardcaml]
  end

  module Stack = Hardcaml_circuits.Stack.Make (struct
    module M = Address

    let capacity = capacity
  end)

  module Input = struct
    type 'a t = {
      load_finished : 'a;
      program_halted : 'a;
      program_ready : 'a;
      pc : 'a; [@bits program_bits]
      opcode : 'a Instruction.Opcode.t;
    }
    [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = {
      match_address : 'a; [@bits program_bits]
      start : 'a;
      scan : 'a;
      ready : 'a;
      invalid_program : 'a;
    }
    [@@deriving hardcaml]
  end

  let jump_table ~clock ~write_enable ~opening ~closing ~address =
    let write_port write_address write_data =
      {
        Write_port.write_clock = clock;
        write_enable;
        write_address;
        write_data;
      }
    in
    (multiport_memory capacity
       ~write_ports:[| write_port opening closing; write_port closing opening |]
       ~read_addresses:[| address |]).(0)

  let create ~clock ~reset (inputs : _ Input.t) =
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let state = Always.State_machine.create (module State) spec in
    let start_scan =
      state.is Waiting_for_program &: inputs.load_finished &: ~:reset
    in
    let scanning = state.is Scanning &: ~:reset in
    let scan = scanning &: inputs.program_ready &: ~:(inputs.program_halted) in
    let scan_complete = scanning &: inputs.program_halted in
    let pop = scan &: Instruction.is inputs.opcode Loop_end in
    let stack =
      Stack.create (Scope.create ())
        {
          Stack.I.clock;
          clear = reset;
          wr_data = { Address.address = inputs.pc };
          push = scan &: Instruction.is inputs.opcode Loop_start;
          pop;
        }
    in
    let previous_pc = reg spec inputs.pc in
    let match_address =
      jump_table ~clock ~write_enable:(stack.q.valid &: ~:reset)
        ~opening:stack.q.value.address ~closing:previous_pc ~address:inputs.pc
    in
    let scan_valid = scan_complete &: stack.empty in
    let scan_invalid =
      pop &: stack.empty |: (scan_complete &: ~:(stack.empty))
    in
    Always.(
      compile
        [
          when_ start_scan [ state.set_next Scanning ];
          when_ scan_valid [ state.set_next Ready ];
          when_ scan_invalid [ state.set_next Invalid_program ];
        ]);
    {
      Output.match_address;
      start = start_scan |: scan_valid;
      scan;
      ready = state.is Ready;
      invalid_program = state.is Invalid_program;
    }
end
