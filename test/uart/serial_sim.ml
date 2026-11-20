open Hardcaml

let clock = Signal.input "clock" 1
let reset = Signal.input "reset" 1
let output = Signal.output

let simulate outputs =
  Cyclesim.create (Circuit.create_exn ~name:"serial" outputs)

let set sim name value =
  let port = Cyclesim.in_port sim name in
  port := Bits.of_int ~width:(Bits.width !port) value

let get sim name = Bits.to_int !(Cyclesim.out_port ~clock_edge:Before sim name)

let frame_bits byte =
  (0 :: List.init 8 (fun bit -> (byte lsr bit) land 1)) @ [ 1 ]

let send ~clocks_per_bit cycle byte =
  List.iter
    (fun bit ->
      for _ = 1 to clocks_per_bit do
        cycle bit
      done)
    (frame_bits byte)

let transmitter ~clocks_per_bit =
  Bf_caml.Uart_tx.create ~clocks_per_bit ~clock ~reset
    { valid = Signal.input "valid" 1; data = Signal.input "data" 8 }

let offer sim ~cycle byte =
  set sim "valid" 1;
  set sim "data" byte;
  cycle ();
  assert (get sim "ready" = 1);
  set sim "valid" 0
