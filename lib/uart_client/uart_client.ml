open Hardcaml
module Kind = Uart_message.Kind
module Wire_error = Uart_message.Error
module Codec = Uart_message.Bits_codec

(* Write sends the entire string. Read waits for a byte and returns None on
   closure. A session sends one request at a time and queues incoming events. *)
type transport = {
  write : string -> (unit, string) result;
  read_byte : unit -> (char option, string) result;
}

type error =
  | Transport_error of string
  | Transport_closed
  | Invalid_reply of string
  | Rejected of { command : string; code : int; expected_offset : int }
  | Argument_out_of_range of string

type halt_reason =
  | Completed
  | Invalid_brackets
  | Program_too_large
  | Unknown_halt_reason of int

type event = Output of char | Need_input | Step_done | Halted of halt_reason

type state = {
  pc : int;
  reset : bool;
  loading : bool;
  program_ready : bool;
  running : bool;
  halted : bool;
  clearing : bool;
  invalid_program : bool;
  program_too_large : bool;
}

type t = { transport : transport; events : event Queue.t; mutable id : int }

let create transport = { transport; events = Queue.create (); id = 0 }
let ( let* ) = Result.bind
let max_offset = (1 lsl Uart_message.offset_bits) - 1

let error_message = function
  | Transport_error message -> message
  | Transport_closed -> "UART transport closed"
  | Invalid_reply message -> "invalid UART reply: " ^ message
  | Argument_out_of_range argument -> argument ^ " is out of range"
  | Rejected { command; code; expected_offset } ->
      let reason =
        match code with
        | n when n = Wire_error.unrecognized_command -> "unrecognized command"
        | n when n = Wire_error.command_not_allowed_in_current_state ->
            "command is unavailable in the current state"
        | n when n = Wire_error.upload_offset_mismatch ->
            Printf.sprintf "expected upload offset %d" expected_offset
        | n when n = Wire_error.wrote_to_readonly ->
            "program storage is read-only"
        | n when n = Wire_error.program_input_fifo_full ->
            "program input FIFO is full"
        | n when n = Wire_error.tape_address_out_of_range ->
            "tape address is out of range"
        | n when n = Wire_error.unsupported_state_selector ->
            "unsupported state selector"
        | n when n = Wire_error.malformed_command -> "malformed command"
        | n when n = Wire_error.upload_requires_reset ->
            "reset before uploading"
        | n when n = Wire_error.upload_not_started -> "upload has not started"
        | n when n = Wire_error.upload_limit_reached -> "upload limit reached"
        | n when n = Wire_error.program_execution_inactive ->
            "program execution is inactive"
        | n when n = Wire_error.tape_inspection_requires_pause ->
            "pause execution before inspecting the tape"
        | n when n = Wire_error.program_unavailable -> "no program is loaded"
        | n when n = Wire_error.program_already_halted ->
            "program already halted"
        | n -> Printf.sprintf "device error %d" n
      in
      command ^ ": " ^ reason

let transport result =
  Result.map_error (fun reason -> Transport_error reason) result

let read_packet t =
  let rec collect symbols remaining =
    let* byte = transport (t.transport.read_byte ()) in
    match byte with
    | None -> Error Transport_closed
    | Some byte ->
        let value = Char.code byte in
        let symbol = Bits.of_char byte in
        if value land 0x80 <> 0 then
          collect [ symbol ] (Uart_message.wire_bytes - 1)
        else if remaining = 0 then collect [] 0
        else if remaining > 1 then collect (symbol :: symbols) (remaining - 1)
        else if value > 3 then Error (Invalid_reply "invalid final byte")
        else
          let bits = Bits.concat_msb (List.rev (symbol :: symbols)) in
          let message = Codec.decode bits in
          Ok
            {
              Uart_message.kind = Bits.to_int message.kind;
              id = Bits.to_int message.id;
              offset = Bits.to_int message.offset;
              byte = Bits.to_int message.byte;
            }
  in
  collect [] 0

let decode_event (message : int Uart_message.message) =
  let byte = message.byte in
  if message.kind = Kind.output then Ok (Output (Char.chr byte))
  else if message.kind = Kind.need_input then Ok Need_input
  else if message.kind = Kind.step_done then Ok Step_done
  else if message.kind = Kind.halted then
    let open Uart_message.Halt_reason in
    Ok
      (Halted
         (if byte = program_completed then Completed
          else if byte = invalid_bracket_structure then Invalid_brackets
          else if byte = program_too_large then Program_too_large
          else Unknown_halt_reason byte))
  else Error (Invalid_reply "unknown event kind")

let next_event t =
  match Queue.take_opt t.events with
  | Some event -> Ok event
  | None ->
      let* message = read_packet t in
      if message.id = 0 then decode_event message
      else Error (Invalid_reply "unexpected command reply")

let request t ~name ~kind ?(offset = 0) ?(byte = 0) ~expect () =
  if offset < 0 || offset > max_offset then
    Error (Argument_out_of_range "UART offset")
  else (
    t.id <- (t.id mod 127) + 1;
    let packet =
      Codec.encode
        {
          kind = Bits.of_int ~width:7 kind;
          id = Bits.of_int ~width:7 t.id;
          offset = Bits.of_int ~width:Uart_message.offset_bits offset;
          byte = Bits.of_int ~width:8 byte;
        }
      |> Bits.split_msb ~part_width:8
      |> List.map Bits.to_char |> List.to_seq |> String.of_seq
    in
    let* () = transport (t.transport.write packet) in
    let rec response () =
      let* message = read_packet t in
      if message.id = 0 then (
        let* event = decode_event message in
        Queue.add event t.events;
        response ())
      else if message.id <> t.id then
        Error (Invalid_reply "request ID mismatch")
      else if message.kind = Kind.error then
        Error
          (Rejected
             {
               command = name;
               code = message.byte;
               expected_offset = message.offset;
             })
      else if message.kind = expect then Ok message
      else Error (Invalid_reply "unexpected reply kind")
    in
    response ())

let ack t ~name ~kind ?offset ?byte () =
  request t ~name ~kind ?offset ?byte ~expect:Kind.ack ()

let accepted result = Result.map (fun _ -> ()) result

let reset t =
  let* () = accepted (ack t ~name:"reset" ~kind:Kind.reset ()) in
  Queue.clear t.events;
  Ok ()

(* The device must be in its reset state before uploading. *)
let upload t source =
  let size = String.length source in
  if size > max_offset then Error (Argument_out_of_range "upload size")
  else
    let expect_offset expected (reply : int Uart_message.message) =
      if reply.offset = expected then Ok ()
      else Error (Invalid_reply "incorrect upload acknowledgment offset")
    in
    let* reply = ack t ~name:"begin upload" ~kind:Kind.begin_load () in
    let* () = expect_offset 0 reply in
    let rec send offset =
      if offset = size then
        let* reply =
          ack t ~name:"finish upload" ~kind:Kind.end_load ~offset ()
        in
        expect_offset offset reply
      else
        let* reply =
          ack t ~name:"upload byte" ~kind:Kind.load_byte ~offset
            ~byte:(Char.code source.[offset])
            ()
        in
        let* () = expect_offset (offset + 1) reply in
        send (offset + 1)
    in
    send 0

let run t =
  match ack t ~name:"run" ~kind:Kind.run () with
  | Ok _ -> Ok `Started
  | Error (Rejected { code; _ }) when code = Wire_error.program_already_halted
    ->
      Ok `Already_halted
  | Error reason -> Error reason

let pause t = accepted (ack t ~name:"pause" ~kind:Kind.pause ())

let step t count =
  if count < 1 || count > max_offset then
    Error (Argument_out_of_range "instruction count")
  else accepted (ack t ~name:"step" ~kind:Kind.step ~offset:count ())

let send_input t byte =
  accepted (ack t ~name:"send input" ~kind:Kind.input ~byte:(Char.code byte) ())

let query t byte =
  request t ~name:"get state" ~kind:Kind.get_state ~byte ~expect:Kind.state ()

let state t =
  let* reply = query t 0 in
  let flag bit = reply.byte land (1 lsl bit) <> 0 in
  Ok
    {
      pc = reply.offset;
      reset = flag 0;
      loading = flag 1;
      program_ready = flag 2;
      running = flag 3;
      halted = flag 4;
      clearing = flag 5;
      invalid_program = flag 6;
      program_too_large = flag 7;
    }

(* Pause execution before reading both halves of the counter. *)
let instruction_count t =
  let* low = query t 2 in
  let* high = query t 3 in
  let open Int32 in
  let offset_bits = Uart_message.address_bits in
  if
    low.offset >= 1 lsl offset_bits
    || high.offset >= 1 lsl (32 - offset_bits - 8)
    || high.byte <> 0
  then Error (Invalid_reply "invalid instruction count")
  else
    Ok
      (logor (of_int low.offset)
         (logor
            (shift_left (of_int low.byte) offset_bits)
            (shift_left (of_int high.offset) (offset_bits + 8))))

let current_cell t =
  let* reply = query t 1 in
  Ok (reply.offset, Char.chr reply.byte)

let read_tape t offset =
  let* reply =
    request t ~name:"inspect tape" ~kind:Kind.read_tape ~offset
      ~expect:Kind.tape ()
  in
  if reply.offset = offset then Ok (Char.chr reply.byte)
  else Error (Invalid_reply "incorrect tape address")
