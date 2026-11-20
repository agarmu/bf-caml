open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 64
  let tape_length = 1024
end)

module Sim =
  Simulation.Make (Config) (Ram_memory) (Source.Loaded (Config) (Ram_memory))

let show = function
  | Ok { Simulation.Stats.load_cycles; clear_cycles; execute_cycles; _ } ->
      Printf.printf "ok: load=%d clear=%d execute=%d\n" load_cycles clear_cycles
        execute_cycles
  | Error (Simulation.Cycle_limit { execute_cycles; _ }) ->
      Printf.printf "cycle limit after %d execute cycles\n" execute_cycles
  | Error (End_of_input { execute_cycles; _ }) ->
      Printf.printf "end of input after %d execute cycles\n" execute_cycles
  | Error (Program_too_large capacity) ->
      Printf.printf "program too large (capacity %d)\n" capacity
  | Error Unmatched_brackets -> print_endline "unmatched brackets"
  | Error (Interrupted _) -> print_endline "interrupted"

let run ?max_cycles source =
  show (Sim.run ?max_cycles ~read:(fun () -> None) ~write:ignore source)

let%expect_test "clear cycles are counted apart from execute cycles" =
  run "+++";
  [%expect {| ok: load=6 clear=1019 execute=11 |}]

let%expect_test "cycle limit counts execute cycles only" =
  run ~max_cycles:5 "+[]";
  run ~max_cycles:1_000 "+++";
  [%expect
    {|
    cycle limit after 5 execute cycles
    ok: load=6 clear=1019 execute=11
    |}]

let%expect_test "errors" =
  run (String.make 64 '+');
  run "+[";
  run ",";
  [%expect
    {|
    program too large (capacity 63)
    unmatched brackets
    end of input after 2 execute cycles
    |}]
