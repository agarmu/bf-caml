open Bf_caml
open Uart_client

let packet kind id offset byte =
  [
    0x80 lor kind;
    id;
    offset land 0x7f;
    (offset lsr 7) land 0x7f;
    byte land 0x7f;
    ((offset lsr 14) lsl 1) lor (byte lsr 7);
  ]
  |> List.to_seq |> Seq.map Char.chr |> String.of_seq

let session exchanges =
  let pending = Queue.create () in
  let exchanges = Queue.of_seq (List.to_seq exchanges) in
  let write request =
    let expected, response = Queue.take exchanges in
    assert (request = expected);
    String.iter (fun byte -> Queue.add byte pending) response;
    Ok ()
  in
  let connection =
    create { write; read_byte = (fun () -> Ok (Queue.take_opt pending)) }
  in
  (connection, fun () -> assert (Queue.is_empty exchanges))

let%expect_test "replies preserve wide fields and queue interleaved events" =
  let client, done_sending =
    session
      [
        ( packet 10 1 16385 0,
          "\001\130\002" ^ packet 18 0 0 0x81 ^ packet 23 1 16385 0xff );
        (packet 9 2 0 2, packet 22 2 0x3fff 0xff);
        (packet 9 3 0 3, packet 22 3 0x3ff 0);
      ]
  in
  assert (read_tape client 16385 = Ok '\255');
  assert (next_event client = Ok (Output '\129'));
  assert (instruction_count client = Ok Int32.minus_one);
  assert (next_event client = Error Transport_closed);
  done_sending ()

let%expect_test "an incorrect upload offset stops the upload" =
  let client, done_sending =
    session
      [
        (packet 1 1 0 0, packet 16 1 0 0);
        (packet 2 2 0 (Char.code '+'), packet 16 2 0 0);
      ]
  in
  assert (
    upload client "+."
    = Error (Invalid_reply "incorrect upload acknowledgment offset"));
  done_sending ()

let%expect_test "a reply for a different request is rejected" =
  let client, done_sending = session [ (packet 6 1 0 0, packet 16 2 0 0) ] in
  assert (pause client = Error (Invalid_reply "request ID mismatch"));
  done_sending ()
