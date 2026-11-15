open Hardcaml

type error = Program_too_large of int | Unmatched_brackets

module Make (C : Config.S) (Memory : Memory.S) = struct
  module Cpu = Cpu.Make_with_memory (C) (Memory)
  module Program = Program.Make (C)
  module Sim = Cyclesim.With_interface (Cpu.Input) (Cpu.Output)

  let parse source =
    String.to_seq source |> Seq.filter_map Instruction.of_char |> List.of_seq

  let pulse sim port =
    port := Bits.vdd;
    Cyclesim.cycle sim;
    port := Bits.gnd

  let load (sim : Sim.t) program =
    let i = Cyclesim.inputs sim in
    Cpu.Input.iter i ~f:(fun port -> port := Bits.zero (Bits.width !port));
    i.output_ready := Bits.vdd;
    pulse sim i.reset;
    pulse sim i.new_program;
    i.byte_valid := Bits.vdd;
    List.iter
      (fun op ->
        i.byte := Instruction.to_bits op;
        Cyclesim.cycle sim)
      program;
    i.byte_valid := Bits.gnd;
    pulse sim i.finish

  let step (sim : Sim.t) ~read ~write =
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim ~clock_edge:Before in
    Cyclesim.cycle_before_clock_edge sim;
    i.input_ready := !(o.input_needed);
    if Bits.to_bool !(o.input_needed) then
      i.input_byte := Bits.of_char (Option.value (read ()) ~default:'\000');
    let output = Bits.to_bool !(o.output_valid) in
    let byte = Bits.to_char !(o.output_byte) in
    Cyclesim.cycle sim;
    if output then write byte

  let execute (sim : Sim.t) ~read ~write program =
    let o = Cyclesim.outputs sim in
    load sim program;
    (Cyclesim.inputs sim).step := Bits.vdd;
    while not (Bits.to_bool !(o.halted)) do
      step sim ~read ~write
    done;
    if Bits.to_bool !(o.invalid_program) then Error Unmatched_brackets
    else Ok ()

  let run ?vcd_path ?(rom = false) ~read ~write source =
    let program = parse source in
    if List.length program > Program.capacity then
      Error (Program_too_large Program.capacity)
    else
      let sim =
        Sim.create (Cpu.create ?rom:(if rom then Some program else None))
      in
      let program = if rom then [] else program in
      match vcd_path with
      | None -> execute sim ~read ~write program
      | Some path ->
          Out_channel.with_open_bin path (fun ch ->
              execute (Vcd.wrap ch sim) ~read ~write program)
end
