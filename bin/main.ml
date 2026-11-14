open Hardcaml

module Config = Bf_caml.Config.Make (struct
  let max_program_size = 256
  let tape_length = 16
end)

module Cpu = Bf_caml.Cpu.Make (Config)
module Program = Bf_caml.Program.Make (Config)
module Instruction = Bf_caml.Instruction
module Simulator = Cyclesim.With_interface (Cpu.Input) (Cpu.Output)

let run_sim (sim : Simulator.t) source =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.reset := Bits.vdd;
  inputs.new_program := Bits.gnd;
  inputs.byte_valid := Bits.gnd;
  inputs.byte := Bits.of_char '\000';
  inputs.finish := Bits.gnd;
  inputs.step := Bits.gnd;
  inputs.input_byte := Bits.of_int ~width:8 0;
  inputs.input_ready := Bits.gnd;
  inputs.output_ready := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.reset := Bits.gnd;

  let read_input_byte () =
    Bits.of_char (Option.value (In_channel.input_char stdin) ~default:'\000')
  in
  let opcode_count =
    String.fold_left
      (fun count character ->
        if Option.is_some (Instruction.of_char character) then count + 1
        else count)
      0 source
  in
  if opcode_count > Program.capacity then
    Printf.eprintf "error: program exceeds %d instructions\n%!" Program.capacity
  else (
    inputs.new_program := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.new_program := Bits.gnd;
    inputs.byte_valid := Bits.vdd;
    String.iter
      (fun character ->
        inputs.byte := Bits.of_char character;
        Cyclesim.cycle sim)
      source;
    inputs.byte_valid := Bits.gnd;
    inputs.finish := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.finish := Bits.gnd;
    Cyclesim.cycle sim;
    inputs.step := Bits.vdd;
    while not (Bits.to_bool !(outputs.halted)) do
      let needs_input = Bits.to_bool !(outputs.input_needed) in
      if needs_input then inputs.input_byte := read_input_byte ();
      inputs.input_ready := Bits.of_bool needs_input;
      let output =
        if Bits.to_bool !(outputs.output_valid) then
          Some (Bits.to_char !(outputs.output_byte))
        else None
      in
      Cyclesim.cycle sim;
      Option.iter
        (fun byte ->
          output_char stdout byte;
          flush stdout)
        output
    done;
    inputs.step := Bits.gnd;
    inputs.input_ready := Bits.gnd;
    if Bits.to_bool !(outputs.invalid_program) then
      prerr_endline "error: unmatched brackets")

let () =
  match Sys.argv with
  | [| _; path |] ->
      let source =
        try In_channel.with_open_bin path In_channel.input_all
        with Sys_error message ->
          prerr_endline message;
          exit 1
      in
      Out_channel.with_open_bin "cell.vcd" (fun channel ->
          let sim = Vcd.wrap channel (Simulator.create Cpu.create) in
          run_sim sim source)
  | _ ->
      prerr_endline "usage: bf_caml PROGRAM.bf";
      exit 2
