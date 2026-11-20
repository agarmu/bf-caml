open Bf_caml

let write byte =
  output_char stdout byte;
  flush stdout

let read =
  let ended = ref false in
  fun () ->
    match In_channel.input_char stdin with
    | Some byte -> Some byte
    | None when !ended -> None
    | None ->
        ended := true;
        Some '\004'

let fail message =
  prerr_endline message;
  exit 1

let stop message stats code =
  prerr_endline (message ^ "\n" ^ stats);
  exit code

let too_large capacity =
  fail (Printf.sprintf "error: program exceeds %d instructions" capacity)

let simulate_direct target ?vcd_path source =
  let format = Simulation.Stats.to_string in
  match Machine.Direct.simulate target ?vcd_path ~read ~write source with
  | Ok stats -> prerr_endline (format stats)
  | Error (Program_too_large capacity) -> too_large capacity
  | Error Unmatched_brackets -> fail "error: unmatched brackets"
  | Error (Interrupted stats) -> stop "interrupted" (format stats) 130
  | Error (Cycle_limit stats) -> stop "cycle limit reached" (format stats) 1
  | Error (End_of_input stats) -> stop "end of input" (format stats) 1

let simulate_uart target ?vcd_path source =
  let format = Uart_simulation.format_stats in
  match Uart_simulation.run target ?vcd_path ~read ~write source with
  | Ok stats -> prerr_endline (format stats)
  | Error (Program_too_large capacity) -> too_large capacity
  | Error (Client_error error) ->
      fail ("error: " ^ Uart_client.error_message error)
  | Error (Halted_with_error Invalid_brackets) ->
      fail "error: unmatched brackets"
  | Error (Halted_with_error Program_too_large) ->
      too_large Machine.Uart.program_capacity
  | Error (Halted_with_error (Unknown_halt_reason reason)) ->
      fail (Printf.sprintf "error: UART halted with reason %d" reason)
  | Error (Halted_with_error Completed) ->
      fail "error: unexpected completion event"
  | Error (Interrupted stats) -> stop "interrupted" (format stats) 130
  | Error (End_of_input stats) -> stop "end of input" (format stats) 1

let read_source path = In_channel.with_open_bin path In_channel.input_all

let dump_rtl target ~direct ~clocks_per_bit ~output program =
  let open Hardcaml in
  let capacity, build =
    if direct then
      (Machine.Direct.program_capacity, Machine.Direct.circuit target)
    else
      ( Machine.Uart.program_capacity,
        Machine.Uart.circuit target ~clocks_per_bit )
  in
  if List.length program > capacity then too_large capacity;
  let circuit = build program in
  if output = "-" then Rtl.print Verilog circuit
  else Rtl.output ~output_mode:(To_file output) Verilog circuit

let run memory banking direct clocks_per_bit rtl vcd_path path =
  let target = { Machine.memory; banking } in
  let dump output program =
    dump_rtl target ~direct ~clocks_per_bit ~output program;
    Ok ()
  in
  try
    match (rtl, path, memory) with
    | None, Some path, _ ->
        let simulate = if direct then simulate_direct else simulate_uart in
        simulate target ?vcd_path (read_source path);
        Ok ()
    | None, None, _ -> Error "a program file is required"
    | Some output, _, Machine.Ram | Some output, None, Machine.Rom ->
        dump output []
    | Some output, Some path, Machine.Rom ->
        dump output (Instruction.parse (read_source path))
  with Sys_error message -> fail ("error: " ^ message)

open Cmdliner

let memory =
  let values = Arg.enum [ ("ram", Machine.Ram); ("rom", Machine.Rom) ] in
  Arg.(
    value & opt values Machine.Ram
    & info [ "memory" ] ~docv:"MEMORY"
        ~doc:"Use loadable RAM or embed the program in ROM.")

let banking =
  let values =
    Arg.enum
      [
        ("none", Machine.Unbanked);
        ("tape", Machine.Tape);
        ("program", Machine.Program);
        ("all", Machine.All);
      ]
  in
  Arg.(
    value
    & opt values Machine.Unbanked
    & info [ "bank" ] ~docv:"BANKING" ~doc:"Choose which memories to bank.")

let direct =
  Arg.(
    value & flag
    & info [ "direct" ]
        ~doc:"Use the raw CPU interface instead of UART command and control.")

let clocks_per_bit =
  let parser value =
    match Arg.Conv.parser Arg.int value with
    | Ok clocks when clocks >= 4 -> Ok clocks
    | Ok _ -> Error "UART requires at least four clocks per bit"
    | Error _ as error -> error
  in
  let converter = Arg.Conv.of_conv ~parser Arg.int in
  Arg.(
    value
    & opt converter Machine.Uart.default_clocks_per_bit
    & info [ "clocks-per-bit" ] ~docv:"N"
        ~doc:"Number of clocks per UART bit in generated RTL.")

let rtl =
  Arg.(
    value
    & opt (some string) None
    & info [ "dump-rtl" ] ~docv:"FILE"
        ~doc:
          "Emit Verilog to $(docv), or to standard output when $(docv) is \
           $(b,-).")

let vcd =
  Arg.(
    value
    & opt (some string) None
    & info [ "dump-vcd" ] ~docv:"FILE"
        ~doc:"Write simulation waveforms to $(docv).")

let program =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"PROGRAM.bf"
        ~doc:
          "Brainfuck source file. Required for simulation and optional when \
           dumping RTL with ROM.")

let command =
  let term =
    Term.(
      const run $ memory $ banking $ direct $ clocks_per_bit $ rtl $ vcd
      $ program)
  in
  Cmd.v
    (Cmd.info "bf_caml" ~doc:"Simulate Brainfuck hardware or generate Verilog.")
    (Term.cli_parse_result' term)

let () =
  Sys.catch_break true;
  exit (Cmd.eval command)
