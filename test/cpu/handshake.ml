open Hardcaml
open Setup

let set sim name value =
  let port = Cyclesim.in_port sim name in
  port := Bits.of_int ~width:(Bits.width !port) value

let get ?clock_edge sim name =
  Bits.to_int !(Cyclesim.out_port ?clock_edge sim name)

let high ?clock_edge sim name = get ?clock_edge sim name = 1

let cycles sim n =
  for _ = 1 to n do
    Cyclesim.cycle sim
  done

let run_until_halted sim =
  let output = Buffer.create 8 in
  set sim "step" 1;
  set sim "output_ready" 1;
  let rec go limit =
    if limit > 0 && not (high sim "halted") then (
      Cyclesim.cycle sim;
      if high ~clock_edge:Before sim "output_valid" then
        Buffer.add_char output
          (Char.chr (get ~clock_edge:Before sim "output_byte"));
      go (limit - 1))
  in
  go 10_000;
  Buffer.contents output

let report results =
  match results with
  | (_, first) :: rest when List.for_all (fun (_, r) -> r = first) rest ->
      print_string first
  | _ -> List.iter (fun (name, r) -> Printf.printf "%s:\n%s" name r) results

let everywhere ?(reloadable = false) f =
  List.filter_map
    (fun (name, start) ->
      match start "" with
      | { Test_setups.reload = None; _ } when reloadable -> None
      | _ -> Some (name, f start))
    machines
  |> report

let%expect_test "output waits for output_ready" =
  everywhere (fun start ->
      match start "+.+." with
      | { Test_setups.sim = Sim sim; _ } ->
          set sim "step" 1;
          set sim "output_ready" 0;
          cycles sim 200;
          let blocked =
            Printf.sprintf "blocked: halted=%b output_valid=%b byte=%d\n"
              (high sim "halted") (high sim "output_valid")
              (get sim "output_byte")
          in
          blocked ^ Printf.sprintf "released: %S\n" (run_until_halted sim));
  [%expect
    {|
    blocked: halted=false output_valid=true byte=1
    released: "\001\002"
    |}]

let%expect_test "input waits for input_ready" =
  everywhere (fun start ->
      match start ",." with
      | { Test_setups.sim = Sim sim; _ } ->
          set sim "step" 1;
          cycles sim 200;
          let blocked =
            Printf.sprintf "blocked: halted=%b input_needed=%b\n"
              (high sim "halted") (high sim "input_needed")
          in
          set sim "input_byte" (Char.code 'x');
          set sim "input_ready" 1;
          blocked ^ Printf.sprintf "released: %S\n" (run_until_halted sim));
  [%expect
    {|
    blocked: halted=false input_needed=true
    released: "x"
    |}]

let%expect_test "step low freezes the program" =
  everywhere (fun start ->
      match start "+>+>+" with
      | { Test_setups.sim = Sim sim; _ } ->
          cycles sim 200;
          Printf.sprintf "pc=%d pointer=%d halted=%b\n" (get sim "pc")
            (get sim "pointer") (high sim "halted"));
  [%expect {| pc=0 pointer=0 halted=false |}]

let%expect_test "reloading after a run starts clean" =
  everywhere ~reloadable:true (fun start ->
      match start "" with
      | { Test_setups.sim = Sim sim; reload = Some reload } ->
          [ "+++++++[>+++++++<-]>."; "["; ">.<."; "++++++++[>++++++<-]>." ]
          |> List.map (fun source ->
              reload source;
              let output = run_until_halted sim in
              Printf.sprintf "%-24s %S invalid=%b\n" source output
                (high sim "invalid_program"))
          |> String.concat ""
      | { reload = None; _ } -> assert false);
  [%expect
    {|
    +++++++[>+++++++<-]>.    "1" invalid=false
    [                        "" invalid=true
    >.<.                     "\000\000" invalid=false
    ++++++++[>++++++<-]>.    "0" invalid=false
    |}]
