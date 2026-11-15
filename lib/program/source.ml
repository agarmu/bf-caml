open Hardcaml
open Signal
open Helpers

type t = {
  load : Signal.t;
  program_size : Signal.t;
  finished : Signal.t;
  read : Memory.read;
}

module type S = sig
  module Input : Interface.S

  val create :
    Instruction.t list ->
    clock:Signal.t ->
    reset:Signal.t ->
    Signal.t Input.t ->
    t

  val load :
    Bits.t ref Input.t -> cycle:(unit -> unit) -> Instruction.t list -> unit
end

module Loaded (C : Config.S) (Memory : Memory.S) : S = struct
  module Loader = Loader.Make (C)
  module Input = Loader.Input

  let create _ ~clock ~reset inputs =
    let loader = Loader.create ~clock ~reset inputs in
    let read ~address =
      Memory.create ~address_bits:C.program_bits ~clear:gnd ~clock ~reset
        ~address:(mux2 loader.load loader.program_size address)
        ~write_enable:loader.load ~write_data:inputs.byte
    in
    {
      load = loader.load;
      program_size = loader.program_size;
      finished = loader.finished;
      read;
    }

  let load (inputs : _ Input.t) ~cycle program =
    let send op =
      inputs.byte := Instruction.to_bits op;
      cycle ()
    in
    holding inputs.byte_valid (fun () -> List.iter send program);
    pulse ~cycle inputs.finish
end

module Rom (C : Config.S) : S = struct
  module Input = Interface.Empty

  let create program ~clock:_ ~reset:_ _ =
    let size = List.length program in
    if size > C.program_capacity then invalid_arg "program too large for ROM";
    {
      load = gnd;
      program_size = of_int ~width:C.program_bits size;
      finished = vdd;
      read = Rom.create ~contents:(List.map Instruction.to_signal program);
    }

  let load _ ~cycle:_ _ = ()
end
