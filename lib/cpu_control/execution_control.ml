open Hardcaml
open Signal
open Always
open Cpu_command_result

type t = {
  running : Signal.t;
  enable : Signal.t;
  requested : Signal.t;
  outcome : Signal.t Result_code.t;
  step_done : Signal.t;
}

let create spec ~available ~reset ~pause ~run ~step ~count ~retired ~halted
    ~reported =
  let requested = run |: step in
  let outcome =
    choose
      [
        (~:available, Outcome.Program_unavailable);
        (halted, Outcome.Program_already_halted);
      ]
      ~otherwise:Outcome.Accepted
  in
  let executable = Result_code.Of_signal.is outcome Outcome.Accepted in
  let run = run &: executable in
  let step = step &: executable in
  let continuous = Variable.reg spec ~width:1 in
  let remaining = Variable.reg spec ~width:(width count) in
  let done_pending = Variable.reg spec ~width:1 in
  let stepping = remaining.value <>:. 0 in
  let final_step = remaining.value ==:. 1 in
  (* Sample halt after retirement so completing the program reports Halted. *)
  let budget_finished = reg spec (retired &: final_step) in
  let completed = budget_finished &: ~:halted in
  let step_done = done_pending.value |: completed in
  let stop = reset |: pause in
  compile
    [
      when_ (reported |: reset) [ done_pending <--. 0 ];
      when_ (completed &: ~:(reported |: reset)) [ done_pending <--. 1 ];
      if_ (stop |: halted)
        [ continuous <--. 0; remaining <--. 0 ]
        [
          when_ (retired &: stepping) [ remaining <-- remaining.value -:. 1 ];
          when_ run [ continuous <--. 1; remaining <--. 0 ];
          when_ step
            [
              continuous <--. 0;
              remaining <-- mux2 (count ==:. 0) (one (width count)) count;
            ];
        ];
    ];
  let running = executable &: (continuous.value |: stepping) in
  {
    running;
    (* Leave room for the next completion before retiring its final instruction. *)
    enable = running &: ~:(stop |: run |: step |: (final_step &: step_done));
    requested;
    outcome;
    step_done;
  }
