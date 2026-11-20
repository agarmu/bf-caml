open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 4096
  let tape_length = 1024
end)

module Sim =
  Simulation.Make (Config) (Ram_memory) (Source.Loaded (Config) (Ram_memory))

let run source =
  match Sim.run ~read:(fun () -> None) ~write:print_char source with
  | Ok _ -> ()
  | Error _ -> print_string "error"

let example name =
  In_channel.with_open_bin ("../examples/" ^ name ^ ".bf") In_channel.input_all

let%expect_test "hello world" =
  run (example "hello-world");
  [%expect {| Hello World! |}]
