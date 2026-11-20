open Hardcaml
open Signal
open Always

module Notification = struct
  type t = Halted | Step_done | Need_input | Output
  [@@deriving compare, enumerate, sexp_of]
end

module Kind = Enum.Make_binary (Notification)

module Stop_reason = struct
  type t = Program_completed | Invalid_bracket_structure | Program_too_large
  [@@deriving compare, enumerate, sexp_of]
end

module Stop = Enum.Make_binary (Stop_reason)

type event = {
  valid : Signal.t;
  kind : Signal.t Kind.t;
  byte : Signal.t;
  stop_reason : Signal.t Stop.t;
}

type t = {
  event : event;
  output_ready : Signal.t;
  step_done_reported : Signal.t;
}

let create spec ~clear ~enabled ~ready ~halted ~invalid_program
    ~program_too_large ~step_done ~waiting_for_input
    ~(output : _ Stream.Byte.Source.t) =
  let halt_reported = Variable.reg spec ~width:1 in
  let input_requested = Variable.reg spec ~width:1 in
  let report_halt = halted &: ~:(halt_reported.value) in
  let report_input = waiting_for_input &: ~:(input_requested.value) in
  let send = enabled &: ready in
  let step_done_reported = send &: step_done in
  let control_event = report_halt |: step_done |: report_input in
  compile
    [
      when_ (send &: report_halt &: ~:step_done) [ halt_reported <--. 1 ];
      when_ clear [ halt_reported <--. 0 ];
      if_ waiting_for_input
        [
          when_
            (send &: report_input &: ~:(report_halt |: step_done))
            [ input_requested <--. 1 ];
        ]
        [ input_requested <--. 0 ];
    ];
  let encode kind = Kind.to_raw (Kind.Of_signal.of_enum kind) in
  let kind =
    mux2 step_done
      (encode Notification.Step_done)
      (mux2 report_halt
         (encode Notification.Halted)
         (mux2 report_input
            (encode Notification.Need_input)
            (encode Notification.Output)))
  in
  let stop reason = Stop.to_raw (Stop.Of_signal.of_enum reason) in
  let stop_reason =
    mux2 program_too_large
      (stop Stop_reason.Program_too_large)
      (mux2 invalid_program
         (stop Stop_reason.Invalid_bracket_structure)
         (stop Stop_reason.Program_completed))
  in
  {
    event =
      {
        valid = enabled &: (control_event |: output.valid);
        kind = Kind.Of_signal.of_raw kind;
        byte = mux2 control_event (zero 8) output.data;
        stop_reason = Stop.Of_signal.of_raw stop_reason;
      };
    output_ready = send &: ~:control_event;
    step_done_reported;
  }
