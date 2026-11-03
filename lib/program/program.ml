open Hardcaml
open Signal

let capacity = 255

(* Load one opcode per clock edge. [program_size] is the number of loaded opcodes,
   from 0 to [capacity]. [start] begins a run; [step] advances the counter. *)
module Input = struct
  type 'a t = {
    clock : 'a;
    reset : 'a;
    load : 'a;
    load_address : 'a; [@bits 8]
    load_opcode : 'a; [@bits 8]
    program_size : 'a; [@bits 8]
    start : 'a;
    step : 'a;
  }
  [@@deriving hardcaml]
end

module Output = struct
  type 'a t = { pc : 'a; [@bits 8] opcode : 'a; [@bits 8] halted : 'a }
  [@@deriving hardcaml]
end

let create (inputs : _ Input.t) =
  let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
  (* These registers reset to zero, so the program starts halted. *)
  let pc = wire 8 in
  let running = wire 1 in
  (* The opcode at [pc] is available without advancing the program. *)
  let opcode =
    memory capacity
      ~write_port:
        {
          Write_port.write_clock = inputs.clock;
          (* The 8-bit address 255 lies outside the 255-opcode store. *)
          write_enable = inputs.load &: (inputs.load_address <:. capacity);
          write_address = inputs.load_address;
          write_data = inputs.load_opcode;
        }
      ~read_address:pc
  in
  let advance = inputs.step &: running &: ~:(inputs.load) in
  (* The final step clears [running] and leaves [pc] on the last opcode. *)
  let last = pc ==: inputs.program_size -:. 1 in
  pc
  <== reg spec
        ~enable:(inputs.start |: (advance &: ~:last))
        (mux2 inputs.start (zero 8) (pc +:. 1));
  running
  <== reg spec ~enable:(inputs.start |: advance)
        (mux2 inputs.start (inputs.program_size <>:. 0) ~:last);
  { Output.pc; opcode; halted = ~:running }
