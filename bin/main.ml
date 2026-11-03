open Hardcaml
module Tape = Bf_caml.Tape
module Instruction = Bf_caml.Instruction
module Simulator = Cyclesim.With_interface (Tape.Input) (Tape.Output)

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
    Printf.sprintf "pointer = %d\ncells = [%s]"
      (Bits.to_int !(outputs.pointer))
      (String.concat "," values)
  in

  inputs.reset := Bits.vdd;
  inputs.enable := Bits.gnd;
  inputs.opcode := Instruction.to_bits Instruction.Increment;
  inputs.pointer_load := Bits.gnd;
  inputs.pointer_next := Bits.of_int ~width:4 0;
  Cyclesim.cycle sim;
  inputs.reset := Bits.gnd;
  Printf.printf "%s\n%!" (show ());

  let execute opcode =
    inputs.opcode := Instruction.to_bits opcode;
    Cyclesim.cycle sim
  in
  repl (fun line ->
      inputs.enable := Bits.vdd;
      String.iter
        (fun character -> Option.iter execute (Instruction.of_char character))
        line;
      inputs.enable := Bits.gnd;
      (* VCD samples before each edge, so this cycle captures the last result. *)
      Cyclesim.cycle sim;
      show ())

let run sim path =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> run_sim (Vcd.wrap channel sim));
  Printf.printf "Wrote waveform to %s\n" path

let () =
  match Sys.argv with
  | [| _; path |] -> run (Simulator.create Tape.create) path
  | _ ->
      prerr_endline "usage: bf_caml FILE";
      exit 2
