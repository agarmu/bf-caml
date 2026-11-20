open Hardcaml
open Bf_caml
open Serial_sim

let packet kind id offset byte =
  [
    0x80 lor kind;
    id;
    offset land 0x7f;
    (offset lsr 7) land 0x7f;
    byte land 0x7f;
    ((offset lsr 14) lsl 1) lor (byte lsr 7);
  ]

type host = { send : int list -> unit; expect : int list -> unit }

let connect create =
  let clocks_per_bit = 10 in
  let rx =
    Uart_rx.create ~clocks_per_bit ~clock ~reset ~rx:(Signal.input "rx" 1)
  in
  let tx_ready = Signal.wire 1 in
  let output_bytes, overrun =
    create rx.byte { Stream.Byte.Sink.ready = tx_ready }
  in
  let tx_sink, tx = Uart_tx.create ~clocks_per_bit ~clock ~reset output_bytes in
  Signal.assign tx_ready tx_sink.ready;
  let reply = Uart_rx.create ~clocks_per_bit ~clock ~reset ~rx:tx in
  let sim =
    simulate
      [
        output "received" reply.byte.valid;
        output "data" reply.byte.data;
        output "error"
          Signal.(rx.framing_error |: overrun |: reply.framing_error);
      ]
  in
  let received = Queue.create () in
  let cycle bit =
    set sim "rx" bit;
    Cyclesim.cycle sim;
    assert (get sim "error" = 0);
    if get sim "received" = 1 then Queue.add (get sim "data") received
  in
  let expect expected =
    let timeout =
      2 * List.length expected * Uart_frame.width * clocks_per_bit
    in
    for _ = 1 to timeout do
      if Queue.length received < List.length expected then cycle 1
    done;
    let actual =
      List.init
        (min (List.length expected) (Queue.length received))
        (fun _ -> Queue.take received)
    in
    if actual <> expected then
      failwith
        (Printf.sprintf "expected [%s], got [%s]"
           (String.concat ", " (List.map string_of_int expected))
           (String.concat ", " (List.map string_of_int actual)))
  in
  set sim "reset" 1;
  cycle 1;
  set sim "reset" 0;
  { send = List.iter (send ~clocks_per_bit cycle); expect }
