open Hardcaml
open Signal

module Outcome = struct
  type t =
    | Accepted
    | Unrecognized_command
    | Command_not_allowed_in_current_state
    | Upload_offset_mismatch
    | Wrote_to_Readonly
    | Program_input_fifo_full
    | Tape_address_out_of_range
    | Upload_requires_reset
    | Upload_not_started
    | Upload_limit_reached
    | Program_execution_inactive
    | Tape_inspection_requires_pause
    | Program_unavailable
    | Program_already_halted
  [@@deriving compare, enumerate, sexp_of]
end

module Result_code = Enum.Make_binary (Outcome)

let encode outcome = Result_code.to_raw (Result_code.Of_signal.of_enum outcome)
let of_raw = Result_code.Of_signal.of_raw

let choose cases ~otherwise =
  let result =
    priority_select
      (List.map
         (fun (valid, outcome) -> { With_valid.valid; value = encode outcome })
         cases)
  in
  of_raw (mux2 result.valid result.value (encode otherwise))
