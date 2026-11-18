open Hardcaml

module Stats = struct
  type t = { load_cycles : int; execute_cycles : int; seconds : float }

  let to_string { load_cycles; execute_cycles; seconds } =
    Printf.sprintf "cycles: %d load + %d execute = %d total\ntime: %.3fs"
      load_cycles execute_cycles
      (load_cycles + execute_cycles)
      seconds
end

type error =
  | Program_too_large of int
  | Unmatched_brackets
  | Interrupted of Stats.t
  | End_of_input of Stats.t

exception End_of_input_reached

module Make (C : Config.S) (Memory : Memory.S) (Source : Source.S) = struct
  module Cpu = Cpu.Make_with_source (C) (Memory) (Source)
  module Sim = Cyclesim.With_interface (Cpu.Input) (Cpu.Output)

  let parse source =
    String.to_seq source |> Seq.filter_map Instruction.of_char |> List.of_seq

  let load (sim : Sim.t) ~cycles program =
    let i = Cyclesim.inputs sim in
    let cycle () =
      incr cycles;
      Cyclesim.cycle sim
    in
    Cpu.Input.iter i ~f:(fun port -> port := Bits.zero (Bits.width !port));
    i.output_ready := Bits.vdd;
    Helpers.pulse ~cycle i.reset;
    Helpers.pulse ~cycle i.new_program;
    let o = Cyclesim.outputs sim ~clock_edge:Before in
    Source.load i.source o.source ~cycle program

  let step (sim : Sim.t) ~read ~write =
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim ~clock_edge:Before in
    Cyclesim.cycle_before_clock_edge sim;
    i.input_ready := !(o.input_needed);
    if Bits.to_bool !(o.input_needed) then
      i.input_byte :=
        Bits.of_char
          (match read () with
          | Some byte -> byte
          | None -> raise End_of_input_reached);
    let output = Bits.to_bool !(o.output_valid) in
    let byte = Bits.to_char !(o.output_byte) in
    Cyclesim.cycle sim;
    if output then write byte

  let execute (sim : Sim.t) ~read ~write ~load_cycles ~execute_cycles program =
    let o = Cyclesim.outputs sim in
    load sim ~cycles:load_cycles program;
    (Cyclesim.inputs sim).step := Bits.vdd;
    while not (Bits.to_bool !(o.halted)) do
      step sim ~read ~write;
      incr execute_cycles
    done;
    if Bits.to_bool !(o.invalid_program) then Error Unmatched_brackets
    else Ok ()

  let run ?vcd_path ~read ~write source =
    let program = parse source in
    if List.length program > C.program_capacity then
      Error (Program_too_large C.program_capacity)
    else
      let sim = Sim.create (Cpu.create program) in
      let start = Unix.gettimeofday () in
      let load_cycles = ref 0 in
      let execute_cycles = ref 0 in
      let stats () =
        {
          Stats.load_cycles = !load_cycles;
          execute_cycles = !execute_cycles;
          seconds = Unix.gettimeofday () -. start;
        }
      in
      let execute sim =
        execute sim ~read ~write ~load_cycles ~execute_cycles program
      in
      match
        match vcd_path with
        | None -> execute sim
        | Some path ->
            Out_channel.with_open_bin path (fun ch -> execute (Vcd.wrap ch sim))
      with
      | result -> Result.map stats result
      | exception Sys.Break -> Error (Interrupted (stats ()))
      | exception End_of_input_reached -> Error (End_of_input (stats ()))
end
