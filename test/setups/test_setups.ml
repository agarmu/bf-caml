open Hardcaml
open Bf_caml

module Four_lanes = struct
  let lanes = 4
end

module Banked = Banked_memory.Make (Ram_memory) (Four_lanes)
module Banked_rom = Banked_memory.Make_read_only (Rom) (Four_lanes)

type sim = Sim : (_, _) Cyclesim.t -> sim
type machine = { sim : sim; reload : (string -> unit) option }

module Make (C : Config.S) = struct
  let tapes =
    [ ("ram", (module Ram_memory : Memory.S)); ("banked", (module Banked)) ]

  let programs =
    [
      ("ram", (module Source.Loaded (C) (Ram_memory) : Source.S), true);
      ("banked ram", (module Source.Loaded (C) (Banked)), true);
      ("rom", (module Source.Rom (C) (Rom)), false);
      ("banked rom", (module Source.Rom (C) (Banked_rom)), false);
    ]

  let setups =
    List.concat_map
      (fun (tape, (module Tape : Memory.S)) ->
        List.map
          (fun (program, (module Program : Source.S), reloadable) ->
            let module S = Simulation.Make (C) (Tape) (Program) in
            let start source =
              let sim = S.Sim.create (S.Cpu.create (S.parse source)) in
              let load source = S.load sim ~cycles:(ref 0) (S.parse source) in
              load source;
              {
                sim = Sim sim;
                reload = (if reloadable then Some load else None);
              }
            in
            (Printf.sprintf "%s tape, %s program" tape program, S.run, start))
          programs)
      tapes

  let all = List.map (fun (name, run, _) -> (name, run)) setups
  let machines = List.map (fun (name, _, start) -> (name, start)) setups
end
