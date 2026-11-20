open Hardcaml

module Stats = struct
  type t = {
    load_cycles : int;
    clear_cycles : int;
    execute_cycles : int;
    seconds : float;
  }

  let to_string { load_cycles; clear_cycles; execute_cycles; seconds } =
    Printf.sprintf
      "cycles: %d load + %d clear + %d execute = %d total\ntime: %.3fs"
      load_cycles clear_cycles execute_cycles
      (load_cycles + clear_cycles + execute_cycles)
      seconds
end

type error =
  | Program_too_large of int
  | Unmatched_brackets
  | Interrupted of Stats.t
  | Cycle_limit of Stats.t
  | End_of_input of Stats.t

exception Cycle_limit_reached
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
    i.output.ready := Bits.vdd;
    Helpers.pulse ~cycle i.reset;
    Helpers.pulse ~cycle i.new_program;
    let o = Cyclesim.outputs sim ~clock_edge:Before in
    Source.load i.source o.source ~cycle program

  let next_byte ~read =
    match read () with
    | Some byte -> Bits.of_char byte
    | None -> raise End_of_input_reached

  let step (sim : Sim.t) ~read ~write =
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim ~clock_edge:Before in
    let supplied = Stream_sim.Byte.offered i.input in
    Cyclesim.cycle sim;
    Stream_sim.Byte.withdraw i.input;
    if Bits.to_bool !(o.input.ready) && not supplied then
      Stream_sim.Byte.offer i.input (next_byte ~read);
    if Stream_sim.Byte.transferred o.output i.output then
      write (Bits.to_char !(o.output.data))

  let check_limit ~max_cycles cycles =
    if Option.exists (fun limit -> cycles >= limit) max_cycles then
      raise Cycle_limit_reached

  let execute (sim : Sim.t) ~max_cycles ~read ~write ~load_cycles ~clear_cycles
      ~execute_cycles program =
    let o = Cyclesim.outputs sim in
    load sim ~cycles:load_cycles program;
    (Cyclesim.inputs sim).step := Bits.vdd;
    while not (Bits.to_bool !(o.halted)) do
      check_limit ~max_cycles !execute_cycles;
      let counter =
        if Bits.to_bool !(o.clearing) then clear_cycles else execute_cycles
      in
      step sim ~read ~write;
      incr counter
    done;
    if Bits.to_bool !(o.program_too_large) then
      Error (Program_too_large C.program_capacity)
    else if Bits.to_bool !(o.invalid_program) then Error Unmatched_brackets
    else Ok ()

  let run ?vcd_path ?max_cycles ~read ~write source =
    let program = parse source in
    let sim = Sim.create (Cpu.create program) in
    let start = Unix.gettimeofday () in
    let load_cycles = ref 0 in
    let clear_cycles = ref 0 in
    let execute_cycles = ref 0 in
    let stats () =
      {
        Stats.load_cycles = !load_cycles;
        clear_cycles = !clear_cycles;
        execute_cycles = !execute_cycles;
        seconds = Unix.gettimeofday () -. start;
      }
    in
    let execute sim =
      execute sim ~max_cycles ~read ~write ~load_cycles ~clear_cycles
        ~execute_cycles program
    in
    match
      match vcd_path with
      | None -> execute sim
      | Some path ->
          Out_channel.with_open_bin path (fun ch -> execute (Vcd.wrap ch sim))
    with
    | result -> Result.map stats result
    | exception Sys.Break -> Error (Interrupted (stats ()))
    | exception Cycle_limit_reached -> Error (Cycle_limit (stats ()))
    | exception End_of_input_reached -> Error (End_of_input (stats ()))
end
