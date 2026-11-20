open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 64
  let tape_length = 16
end)

module Simulation =
  Simulation.Make (Config) (Ram_memory) (Source.Loaded (Config) (Ram_memory))

let setups =
  let module Setups = Test_setups.Make (Config) in
  Setups.all
