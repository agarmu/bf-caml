open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 64
  let tape_length = 16
end)

module Banked =
  Banked_memory.Make
    (Ram_memory)
    (struct
      let lanes = 4
    end)

module Loaded (M : Memory.S) =
  Simulation.Make (Config) (M) (Source.Loaded (Config) (M))

module Rom (M : Memory.S) = Simulation.Make (Config) (M) (Source.Rom (Config))
module Simulation = Loaded (Ram_memory)

let memories =
  [ ("ram", (module Ram_memory : Memory.S)); ("banked", (module Banked)) ]

let setups =
  List.concat_map
    (fun (name, (module M : Memory.S)) ->
      let module L = Loaded (M) in
      let module R = Rom (M) in
      [
        (name ^ " tape, loaded program", L.run);
        (name ^ " tape, rom program", R.run);
      ])
    memories
