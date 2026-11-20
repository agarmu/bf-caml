open Hardcaml
open Bf_caml
open Serial_sim

let%expect_test "receiver rejects a false start and reports a bad stop" =
  let clocks_per_bit = 10 in
  let rx =
    Uart_rx.create ~clocks_per_bit ~clock ~reset ~rx:(Signal.input "rx" 1)
  in
  let sim =
    simulate
      [
        output "valid" rx.byte.valid;
        output "data" rx.byte.data;
        output "framing_error" rx.framing_error;
      ]
  in
  let received = ref [] in
  let errors = ref 0 in
  let cycle bit =
    set sim "rx" bit;
    Cyclesim.cycle sim;
    if get sim "valid" = 1 then received := get sim "data" :: !received;
    errors := !errors + get sim "framing_error"
  in
  let hold bit cycles =
    for _ = 1 to cycles do
      cycle bit
    done
  in
  set sim "reset" 1;
  cycle 1;
  set sim "reset" 0;
  cycle 0;
  (* A low pulse shorter than half a bit. *)
  hold 1 (2 * clocks_per_bit);
  hold 0 (25 * clocks_per_bit);
  (* A break spans multiple frames. *)
  hold 1 (2 * clocks_per_bit);
  send ~clocks_per_bit cycle 0x81;
  hold 1 clocks_per_bit;
  assert (!errors = 1 && !received = [ 0x81 ])
