open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 64
  let tape_length = 16
end)

module Four_lanes = struct
  let lanes = 4
end

module Banked = Banked_memory.Make (Ram_memory) (Four_lanes)
module Banked_rom = Banked_memory.Make_read_only (Rom) (Four_lanes)

module Simulation =
  Simulation.Make (Config) (Ram_memory) (Source.Loaded (Config) (Ram_memory))

let tapes =
  [ ("ram", (module Ram_memory : Memory.S)); ("banked", (module Banked)) ]

let programs =
  [
    ("ram", (module Source.Loaded (Config) (Ram_memory) : Source.S));
    ("banked ram", (module Source.Loaded (Config) (Banked)));
    ("rom", (module Source.Rom (Config) (Rom)));
    ("banked rom", (module Source.Rom (Config) (Banked_rom)));
  ]

let setups =
  List.concat_map
    (fun (tape, (module Tape : Memory.S)) ->
      List.map
        (fun (program, (module Program : Source.S)) ->
          let module S = Bf_caml.Simulation.Make (Config) (Tape) (Program) in
          (Printf.sprintf "%s tape, %s program" tape program, S.run))
        programs)
    tapes
