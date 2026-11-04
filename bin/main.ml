open Hardcaml

module Config = Bf_caml.Config.Make (struct
  let max_program_size = 256
  let tape_length = 16
end)

module Cpu = Bf_caml.Cpu.Make (Config)
module Tape = Bf_caml.Tape.Make (Config)
module Program = Bf_caml.Program.Make (Config)
module Instruction = Bf_caml.Instruction
module Simulator = Cyclesim.With_interface (Cpu.Input) (Cpu.Output)

let repl handle =
  let interactive = Unix.isatty (Unix.descr_of_in_channel stdin) in
  let rec loop () =
    let line =
      if interactive then LNoise.linenoise "bf> "
      else In_channel.input_line stdin
    in
    match line with
    | None | Some ":quit" -> ()
    | Some line ->
        if interactive then ignore (LNoise.history_add line);
        print_endline (handle line);
        loop ()
  in
  loop ()

let run_sim (sim : Simulator.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let show () =
    let cells = !(outputs.cells) in
    let values =
      List.init Tape.cell_count (fun index ->
          let low = index * 8 in
          Bits.select cells (low + 7) low |> Bits.to_int |> string_of_int)
    in
    Printf.sprintf "pc = %d\npointer = %d\ncells = [%s]"
      (Bits.to_int !(outputs.pc))
      (Bits.to_int !(outputs.pointer))
      (String.concat "," values)
  in

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
  Printf.printf "%s\n%!" (show ());

  let read_input_byte () =
    let byte =
      if Unix.isatty (Unix.descr_of_in_channel stdin) then
        match LNoise.linenoise "input> " with
        | Some line when line <> "" -> Some line.[0]
        | _ -> None
      else In_channel.input_char stdin
    in
    Bits.of_char (Option.value byte ~default:'\000')
  in
  repl (fun line ->
      let opcode_count =
        String.fold_left
          (fun count character ->
            if Option.is_some (Instruction.of_char character) then count + 1
            else count)
          0 line
      in
      if opcode_count > Program.capacity then
        Printf.sprintf "error: program exceeds %d instructions" Program.capacity
      else (
        inputs.new_program := Bits.vdd;
        Cyclesim.cycle sim;
        inputs.new_program := Bits.gnd;
        inputs.byte_valid := Bits.vdd;
        String.iter
          (fun character ->
            inputs.byte := Bits.of_char character;
            Cyclesim.cycle sim)
          line;
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
            (fun byte -> Printf.printf "output = %S\n%!" (String.make 1 byte))
            output
        done;
        inputs.step := Bits.gnd;
        inputs.input_ready := Bits.gnd;
        (* Capture the final state in the VCD after the last instruction. *)
        Cyclesim.cycle sim;
        if Bits.to_bool !(outputs.invalid_program) then
          "error: unmatched brackets"
        else show ()))

let run sim path =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> run_sim (Vcd.wrap channel sim));
  Printf.printf "Wrote waveform to %s\n" path

let () =
  match Sys.argv with
  | [| _; path |] -> run (Simulator.create Cpu.create) path
  | _ ->
      prerr_endline "usage: bf_caml FILE";
      exit 2
