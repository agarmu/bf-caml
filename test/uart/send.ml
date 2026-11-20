open Serial_sim

let check_send clocks_per_bit =
  let sink, tx = transmitter ~clocks_per_bit in
  let sim = simulate [ output "ready" sink.ready; output "tx" tx ] in
  let cycle () = Hardcaml.Cyclesim.cycle sim in
  set sim "reset" 1;
  cycle ();
  assert (get sim "tx" = 1 && get sim "ready" = 0);
  set sim "reset" 0;
  List.iter
    (fun byte ->
      offer sim ~cycle byte;
      assert (get sim "tx" = 1);
      send ~clocks_per_bit
        (fun bit ->
          cycle ();
          assert (get sim "tx" = bit && get sim "ready" = 0))
        byte)
    [ 0x00; 0x81; 0x55; 0xff ];
  cycle ();
  assert (get sim "tx" = 1 && get sim "ready" = 1)

let%expect_test "TX drives 8N1 frames and holds ready low through the stop bit"
    =
  List.iter check_send [ 4; 9; 10 ]
