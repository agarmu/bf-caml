open Hardcaml

type stats = { setup_cycles : int; run_cycles : int; seconds : float }

type error =
  | Client_error of Uart_client.error
  | Halted_with_error of Uart_client.halt_reason
  | Program_too_large of int
  | End_of_input of stats
  | Interrupted of stats

let format_stats { setup_cycles; run_cycles; seconds } =
  Printf.sprintf "cycles: %d UART setup + %d run = %d total\ntime: %.3fs"
    setup_cycles run_cycles
    (setup_cycles + run_cycles)
    seconds

let with_simulator ?vcd_path circuit use =
  let sim = Cyclesim.create circuit in
  match vcd_path with
  | None -> use sim
  | Some path ->
      Out_channel.with_open_bin path (fun channel -> use (Vcd.wrap channel sim))

exception Receive_overrun

let transport sim ~cycle_count =
  let received = Queue.create () in
  let rx_valid = Cyclesim.in_port sim "rx_valid" in
  let rx_data = Cyclesim.in_port sim "rx_data" in
  let output = Cyclesim.out_port ~clock_edge:Before sim in
  let overrun = output "rx_overrun" in
  let tx_valid = output "tx_valid" in
  let tx_data = output "tx_data" in
  let cycle () =
    incr cycle_count;
    Cyclesim.cycle sim;
    if Bits.to_bool !overrun then raise Receive_overrun;
    if Bits.to_bool !tx_valid then Queue.add (Bits.to_char !tx_data) received
  in
  let transfer action =
    try Ok (action ())
    with Receive_overrun -> Error "UART receive FIFO overrun"
  in
  let reset = Cyclesim.in_port sim "reset" in
  reset := Bits.vdd;
  cycle ();
  reset := Bits.gnd;
  {
    Uart_client.write =
      (fun bytes ->
        transfer (fun () ->
            rx_valid := Bits.vdd;
            Fun.protect
              ~finally:(fun () -> rx_valid := Bits.gnd)
              (fun () ->
                String.iter
                  (fun byte ->
                    rx_data := Bits.of_char byte;
                    cycle ())
                  bytes)));
    read_byte =
      (fun () ->
        transfer (fun () ->
            while Queue.is_empty received do
              cycle ()
            done;
            Some (Queue.take received)));
  }

let ( let* ) = Result.bind
let client result = Result.map_error (fun error -> Client_error error) result

let run target ?vcd_path ~read ~write source =
  let program = Instruction.parse source in
  let capacity = Machine.Uart.program_capacity in
  if List.length program > capacity then Error (Program_too_large capacity)
  else
    let upload = target.Machine.memory = Ram in
    let circuit =
      Machine.Uart.byte_circuit target (if upload then [] else program)
    in
    with_simulator ?vcd_path circuit (fun sim ->
        let cycle_count = ref 0 in
        let setup_cycles = ref None in
        let start = Unix.gettimeofday () in
        let stats () =
          let setup_cycles = Option.value !setup_cycles ~default:!cycle_count in
          {
            setup_cycles;
            run_cycles = !cycle_count - setup_cycles;
            seconds = Unix.gettimeofday () -. start;
          }
        in
        let execute () =
          let connection = Uart_client.create (transport sim ~cycle_count) in
          let* () =
            if upload then
              program |> List.to_seq
              |> Seq.map Instruction.to_char
              |> String.of_seq
              |> Uart_client.upload connection
              |> client
            else Ok ()
          in
          setup_cycles := Some !cycle_count;
          let* _ = client (Uart_client.run connection) in
          let rec pump () =
            let* event = client (Uart_client.next_event connection) in
            match event with
            | Output byte ->
                write byte;
                pump ()
            | Need_input -> (
                match read () with
                | None -> Error (End_of_input (stats ()))
                | Some byte ->
                    let* () = client (Uart_client.send_input connection byte) in
                    pump ())
            | Step_done -> pump ()
            | Halted Completed -> Ok (stats ())
            | Halted reason -> Error (Halted_with_error reason)
          in
          pump ()
        in
        try execute () with Sys.Break -> Error (Interrupted (stats ())))
