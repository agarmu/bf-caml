open Hardcaml

type memory = Ram | Rom
type banking = Unbanked | Tape | Program | All
type t = { memory : memory; banking : banking }

module Eight_lanes = struct
  let lanes = 8
end

module Banked_ram = Banked_memory.Make (Ram_memory) (Eight_lanes)
module Banked_rom = Banked_memory.Make_read_only (Rom) (Eight_lanes)

let ram banked : (module Memory.S) =
  if banked then (module Banked_ram) else (module Ram_memory)

let rom banked : (module Memory.Read_only) =
  if banked then (module Banked_rom) else (module Rom)

let tape_memory { banking; _ } =
  ram (match banking with Tape | All -> true | Program | Unbanked -> false)

let bank_program { banking; _ } =
  match banking with Program | All -> true | Tape | Unbanked -> false

module Direct = struct
  module Config = Config.Make (struct
    let max_program_size = 32768
    let tape_length = 32768
  end)

  let program_capacity = Config.program_capacity

  let source target : (module Source.S) =
    let banked = bank_program target in
    match target.memory with
    | Ram ->
        let module Memory = (val ram banked) in
        (module Source.Loaded (Config) (Memory))
    | Rom ->
        let module Memory = (val rom banked) in
        (module Source.Rom (Config) (Memory))

  let circuit target program =
    let module Tape = (val tape_memory target) in
    let module Source = (val source target) in
    let module Cpu = Cpu.Make_with_source (Config) (Tape) (Source) in
    let module Top = Circuit.With_interface (Cpu.Input) (Cpu.Output) in
    Top.create_exn ~name:"bf_caml" (Cpu.create program)

  let simulate target =
    let module Tape = (val tape_memory target) in
    let module Source = (val source target) in
    let module Sim = Simulation.Make (Config) (Tape) (Source) in
    Sim.run
end

module Uart = struct
  module Config = Config.Make (struct
    let max_program_size = 1 lsl Uart_message.address_bits
    let tape_length = max_program_size
  end)

  let program_capacity = Config.program_capacity
  let default_clocks_per_bit = 16
  let rx_capacity = 8

  let controller target : (module Uart_control.S) =
    let module Tape = (val tape_memory target) in
    let banked = bank_program target in
    match target.memory with
    | Ram ->
        let module Program = (val ram banked) in
        (module Uart_control.Make_with_memories (Config) (Tape) (Program))
    | Rom ->
        let module Program = (val rom banked) in
        (module Uart_control.Make_with_rom (Config) (Tape) (Program))

  let create target program ~clock ~reset ~rx ~tx =
    let module Controller = (val controller target) in
    Controller.create ~program ~clock ~reset ~rx_capacity ~rx ~tx

  let circuit target ~clocks_per_bit program =
    let open Signal in
    let clock = input "clock" 1 in
    let reset = input "reset" 1 in
    let rx = Uart_rx.create ~clocks_per_bit ~clock ~reset ~rx:(input "rx" 1) in
    let tx_ready = wire 1 in
    let control =
      create target program ~clock ~reset ~rx:rx.byte ~tx:{ ready = tx_ready }
    in
    let sink, tx = Uart_tx.create ~clocks_per_bit ~clock ~reset control.tx in
    tx_ready <== sink.ready;
    Circuit.create_exn ~name:"bf_caml"
      [
        output "tx" tx;
        output "rx_framing_error" rx.framing_error;
        output "rx_overrun" control.rx_overrun;
      ]

  let byte_circuit target program =
    let open Signal in
    let clock = input "clock" 1 in
    let reset = input "reset" 1 in
    let rx =
      {
        Stream.Byte.Source.valid = input "rx_valid" 1;
        data = input "rx_data" 8;
      }
    in
    let control = create target program ~clock ~reset ~rx ~tx:{ ready = vdd } in
    Circuit.create_exn ~name:"bf_caml_uart"
      [
        output "tx_valid" control.tx.valid;
        output "tx_data" control.tx.data;
        output "rx_overrun" control.rx_overrun;
      ]
end
