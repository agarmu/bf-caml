open Hardcaml
open Setup

let load source =
  let sim = Simulation.Sim.create (Simulation.Cpu.create []) in
  Simulation.load sim ~cycles:(ref 0) (Simulation.parse source);
  sim

let cycles sim n =
  for _ = 1 to n do
    Cyclesim.cycle sim
  done

let run_until_halted (sim : Simulation.Sim.t) =
  let i = Cyclesim.inputs sim in
  let before = Cyclesim.outputs ~clock_edge:Before sim in
  let after = Cyclesim.outputs sim in
  let output = Buffer.create 8 in
  i.step := Bits.vdd;
  i.output_ready := Bits.vdd;
  let rec go limit =
    if limit > 0 && not (Bits.to_bool !(after.halted)) then (
      Cyclesim.cycle sim;
      if Bits.to_bool !(before.output_valid) then
        Buffer.add_char output (Bits.to_char !(before.output_byte));
      go (limit - 1))
  in
  go 10_000;
  Buffer.contents output

let%expect_test "output waits for output_ready" =
  let sim = load "+.+." in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  i.step := Bits.vdd;
  i.output_ready := Bits.gnd;
  cycles sim 200;
  Printf.printf "blocked: halted=%b output_valid=%b byte=%d\n"
    (Bits.to_bool !(o.halted))
    (Bits.to_bool !(o.output_valid))
    (Bits.to_int !(o.output_byte));
  Printf.printf "released: %S\n" (run_until_halted sim);
  [%expect
    {|
    blocked: halted=false output_valid=true byte=1
    released: "\001\002"
    |}]

let%expect_test "input waits for input_ready" =
  let sim = load ",." in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  i.step := Bits.vdd;
  cycles sim 200;
  Printf.printf "blocked: halted=%b input_needed=%b\n"
    (Bits.to_bool !(o.halted))
    (Bits.to_bool !(o.input_needed));
  i.input_byte := Bits.of_char 'x';
  i.input_ready := Bits.vdd;
  Printf.printf "released: %S\n" (run_until_halted sim);
  [%expect
    {|
    blocked: halted=false input_needed=true
    released: "x"
    |}]

let%expect_test "step low freezes the program" =
  let sim = load "+>+>+" in
  let o = Cyclesim.outputs sim in
  cycles sim 200;
  Printf.printf "pc=%d pointer=%d halted=%b\n" (Bits.to_int !(o.pc))
    (Bits.to_int !(o.pointer)) (Bits.to_bool !(o.halted));
  [%expect {| pc=0 pointer=0 halted=false |}]

let%expect_test "reloading after a run starts clean" =
  let sim = Simulation.Sim.create (Simulation.Cpu.create []) in
  let o = Cyclesim.outputs sim in
  List.iter
    (fun source ->
      Simulation.load sim ~cycles:(ref 0) (Simulation.parse source);
      let output = run_until_halted sim in
      Printf.printf "%-24s %S invalid=%b\n" source output
        (Bits.to_bool !(o.invalid_program)))
    [ "+++++++[>+++++++<-]>."; "["; ">.<."; "++++++++[>++++++<-]>." ];
  [%expect
    {|
    +++++++[>+++++++<-]>.    "1" invalid=false
    [                        "" invalid=true
    >.<.                     "\000\000" invalid=false
    ++++++++[>++++++<-]>.    "0" invalid=false
    |}]
