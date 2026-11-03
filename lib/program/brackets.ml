open Hardcaml
open Signal

module Input = struct
  type 'a t = {
    clock : 'a;
    reset : 'a;
    scan : 'a;
    pc : 'a; [@bits 8]
    opcode : 'a; [@bits 8]
  }
  [@@deriving hardcaml]
end

module Output = struct
  type 'a t = { match_address : 'a; [@bits 8] balanced : 'a; invalid : 'a }
  [@@deriving hardcaml]
end

let create (inputs : _ Input.t) =
  let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
  let stack_depth = wire 8 in
  let opening = Instruction.matches inputs.opcode Instruction.Loop_start in
  let closing = Instruction.matches inputs.opcode Instruction.Loop_end in
  let push = inputs.scan &: opening in
  let pop = inputs.scan &: closing &: (stack_depth <>:. 0) in
  let unmatched_close = inputs.scan &: closing &: (stack_depth ==:. 0) in
  let matching_open =
    memory Program.capacity
      ~write_port:
        {
          Write_port.write_clock = inputs.clock;
          write_enable = push;
          write_address = stack_depth;
          write_data = inputs.pc;
        }
      ~read_address:(mux2 (stack_depth ==:. 0) (zero 8) (stack_depth -:. 1))
  in
  stack_depth <== reg spec (stack_depth +: uresize push 8 -: uresize pop 8);
  (* Each closing bracket records both directions of the jump. *)
  let match_address =
    (multiport_memory Program.capacity
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
  { Output.match_address; balanced = stack_depth ==:. 0; invalid }
