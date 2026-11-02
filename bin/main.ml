open Hardcaml
module Cell = Bf_caml.Cell
module Instruction = Bf_caml.Instruction
module Simulator = Cyclesim.With_interface (Cell.Input) (Cell.Output)

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
  let cell () = Bits.to_int !(outputs.cell) in

  inputs.reset := Bits.vdd;
  inputs.enable := Bits.gnd;
  inputs.opcode := Instruction.to_bits Instruction.Increment;
  Cyclesim.cycle sim;
  inputs.reset := Bits.gnd;
  Printf.printf "cell=%d\n%!" (cell ());

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
      Printf.sprintf "cell=%d" (cell ()))

let run sim path =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> run_sim (Vcd.wrap channel sim));
  Printf.printf "Wrote waveform to %s\n" path

let () =
  match Sys.argv with
  | [| _; path |] -> run (Simulator.create Cell.create) path
  | _ ->
      prerr_endline "usage: bf_caml FILE";
      exit 2
