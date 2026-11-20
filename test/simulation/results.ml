open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 64
  let tape_length = 1024
end)

module Setups = Test_setups.Make (Config)

let show = function
  | Ok { Simulation.Stats.load_cycles; clear_cycles; execute_cycles; _ } ->
      Printf.sprintf "ok: load=%d clear=%d execute=%d" load_cycles clear_cycles
        execute_cycles
  | Error (Simulation.Cycle_limit { execute_cycles; _ }) ->
      Printf.sprintf "cycle limit after %d execute cycles" execute_cycles
  | Error (End_of_input { execute_cycles; _ }) ->
      Printf.sprintf "end of input after %d execute cycles" execute_cycles
  | Error (Program_too_large capacity) ->
      Printf.sprintf "program too large (capacity %d)" capacity
  | Error Unmatched_brackets -> "unmatched brackets"
  | Error (Interrupted _) -> "interrupted"

let run ?max_cycles source =
  let results =
    List.map
      (fun (name, run) ->
        ( name,
          show
            (run ?vcd_path:None ?max_cycles
               ~read:(fun () -> None)
               ~write:ignore source) ))
      Setups.all
  in
  match results with
  | (_, first) :: rest when List.for_all (fun (_, r) -> r = first) rest ->
      print_endline first
  | _ -> List.iter (fun (name, r) -> Printf.printf "%-32s %s\n" name r) results

let%expect_test "clear cycles are counted apart from execute cycles" =
  run "+++";
  [%expect
    {|
    ram tape, ram program            ok: load=6 clear=1019 execute=11
    ram tape, banked ram program     ok: load=6 clear=1019 execute=9
    ram tape, rom program            ok: load=2 clear=1023 execute=11
    ram tape, banked rom program     ok: load=2 clear=1023 execute=9
    banked tape, ram program         ok: load=6 clear=251 execute=11
    banked tape, banked ram program  ok: load=6 clear=251 execute=9
    banked tape, rom program         ok: load=2 clear=255 execute=11
    banked tape, banked rom program  ok: load=2 clear=255 execute=9
    |}]

let%expect_test "cycle limit counts execute cycles only" =
  run ~max_cycles:5 "+[]";
  run ~max_cycles:1_000 "+++";
  [%expect
    {|
    cycle limit after 5 execute cycles
    ram tape, ram program            ok: load=6 clear=1019 execute=11
    ram tape, banked ram program     ok: load=6 clear=1019 execute=9
    ram tape, rom program            ok: load=2 clear=1023 execute=11
    ram tape, banked rom program     ok: load=2 clear=1023 execute=9
    banked tape, ram program         ok: load=6 clear=251 execute=11
    banked tape, banked ram program  ok: load=6 clear=251 execute=9
    banked tape, rom program         ok: load=2 clear=255 execute=11
    banked tape, banked rom program  ok: load=2 clear=255 execute=9
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
