open Hardcaml
open Signal
open Always

module Kind = struct
  let begin_load = 1
  let load_byte = 2
  let end_load = 3
  let input = 4
  let reset = 5
  let pause = 6
  let step = 7
  let run = 8
  let get_state = 9
  let read_tape = 10
  let ack = 16
  let error = 17
  let output = 18
  let halted = 19
  let need_input = 20
  let step_done = 21
  let state = 22
  let tape = 23
end

module Error = struct
  let unrecognized_command = 1
  let command_not_allowed_in_current_state = 2
  let upload_offset_mismatch = 3
  let wrote_to_readonly = 4
  let program_input_fifo_full = 5
  let tape_address_out_of_range = 6
  let unsupported_state_selector = 7
  let malformed_command = 8
  let upload_requires_reset = 9
  let upload_not_started = 10
  let upload_limit_reached = 11
  let program_execution_inactive = 12
  let tape_inspection_requires_pause = 13
  let program_unavailable = 14
  let program_already_halted = 15
end

module Halt_reason = struct
  let program_completed = 0
  let invalid_bracket_structure = 1
  let program_too_large = 2
end

let address_bits = 14
let offset_bits = address_bits + 1
let wire_bytes = 6
let wire_bits = 8 * wire_bytes

type 'a message = { kind : 'a; id : 'a; offset : 'a; byte : 'a }
type t = Signal.t message

module Codec (Word : Hardcaml.Comb.S) = struct
  open Word

  (* Only the first octet has bit 7 set, so it also resynchronizes the reader.
     The final octet carries the high bits of the offset and payload. *)
  let encode message =
    let symbol data = uresize data 8 in
    concat_msb
      [
        vdd @: message.kind;
        symbol message.id;
        symbol (select message.offset 6 0);
        symbol (select message.offset 13 7);
        symbol (select message.byte 6 0);
        symbol (bit message.offset address_bits @: bit message.byte 7);
      ]

  let decode packet =
    let symbol index = select packet ((8 * index) + 6) (8 * index) in
    {
      kind = symbol 5;
      id = symbol 4;
      offset = bit (symbol 0) 1 @: symbol 2 @: symbol 3;
      byte = bit (symbol 0) 0 @: symbol 1;
    }
end

module Signal_codec = Codec (Signal)
module Bits_codec = Codec (Bits)

let receive spec (bytes : _ Stream.Byte.Source.t)
    (consumer : _ Stream.Byte.Sink.t) =
  let packet = Variable.reg spec ~width:wire_bits in
  let remaining =
    Variable.reg spec ~width:(Bits.num_bits_to_represent wire_bytes)
  in
  let valid = Variable.reg spec ~width:1 in
  let malformed = Variable.reg spec ~width:1 in
  let ready = ~:(valid.value) |: consumer.ready in
  compile
    [
      when_ consumer.ready [ valid <--. 0 ];
      when_ (ready &: bytes.valid)
        [
          if_ (bit bytes.data 7)
            [
              packet <-- uresize bytes.data wire_bits;
              remaining <--. wire_bytes - 1;
            ]
            [
              when_ (remaining.value <>:. 0)
                [
                  packet <-- drop_top packet.value 8 @: bytes.data;
                  remaining <-- remaining.value -:. 1;
                  when_ (remaining.value ==:. 1)
                    [ valid <--. 1; malformed <-- (bytes.data >:. 3) ];
                ];
            ];
        ];
    ];
  (ready, valid.value, malformed.value, Signal_codec.decode packet.value)

let transmit spec ~valid message (sink : _ Stream.Byte.Sink.t) =
  let packet = Variable.reg spec ~width:wire_bits in
  let remaining =
    Variable.reg spec ~width:(Bits.num_bits_to_represent wire_bytes)
  in
  let busy = remaining.value <>:. 0 in
  compile
    [
      when_ (~:busy &: valid)
        [ packet <-- Signal_codec.encode message; remaining <--. wire_bytes ];
      when_ (busy &: sink.ready)
        [ packet <-- sll packet.value 8; remaining <-- remaining.value -:. 1 ];
    ];
  (~:busy, { Stream.Byte.Source.valid = busy; data = sel_top packet.value 8 })
