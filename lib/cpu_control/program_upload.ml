open Hardcaml
open Signal
open Always
open Cpu_command_result

module Phase = struct
  type t = Reset | Loading | Ready [@@deriving compare, enumerate, sexp_of]
end

type t = {
  reset_state : Signal.t;
  loading : Signal.t;
  ready : Signal.t;
  requested : Signal.t;
  outcome : Signal.t Result_code.t;
  next_offset : Signal.t;
  load_byte : Signal.t;
  finish : Signal.t;
}

let create spec ~writable ~reset ~begin_load ~write ~end_load ~requested_offset
    =
  let phase = State_machine.create (module Phase) spec in
  let offset_bits = width requested_offset in
  let offset = Variable.reg spec ~width:offset_bits in
  let matching_offset = requested_offset ==: offset.value in
  let offset_exhausted = offset.value ==: ones offset_bits in
  let uploading = write |: end_load in
  let requested = begin_load |: uploading in
  let outcome =
    choose
      [
        (~:writable, Outcome.Wrote_to_Readonly);
        (begin_load &: ~:(phase.is Reset), Outcome.Upload_requires_reset);
        (uploading &: ~:(phase.is Loading), Outcome.Upload_not_started);
        (uploading &: ~:matching_offset, Outcome.Upload_offset_mismatch);
        (write &: offset_exhausted, Outcome.Upload_limit_reached);
      ]
      ~otherwise:Outcome.Accepted
  in
  let accepted = Result_code.Of_signal.is outcome Outcome.Accepted in
  let load_byte = write &: accepted in
  let finish = end_load &: accepted in
  let next_offset =
    mux2 reset (zero offset_bits) (offset.value +: uresize load_byte offset_bits)
  in
  compile
    [
      when_ reset [ phase.set_next Reset ];
      when_ (begin_load &: accepted) [ phase.set_next Loading ];
      when_ finish [ phase.set_next Ready ];
      when_ (reset |: load_byte) [ offset <-- next_offset ];
    ];
  {
    reset_state = phase.is Reset;
    loading = phase.is Loading;
    ready = phase.is Ready |: (~:writable &: phase.is Reset);
    requested;
    outcome;
    next_offset;
    load_byte;
    finish;
  }
