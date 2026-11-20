open Hardcaml
open Signal
open Helpers

type t = {
  load : Signal.t;
  program_size : Signal.t;
  finished : Signal.t;
  too_large : Signal.t;
  read : Memory.read;
}

module type S = sig
  type nonrec t = t

  module Input : Interface.S
  module Output : Interface.S

  val create :
    Instruction.t list ->
    clock:Signal.t ->
    reset:Signal.t ->
    Signal.t Input.t ->
    t * Signal.t Output.t

  val load :
    Bits.t ref Input.t ->
    Bits.t ref Output.t ->
    cycle:(unit -> unit) ->
    Instruction.t list ->
    unit
end

module Loaded (C : Config.S) (Memory : Memory.S) : S = struct
  type nonrec t = t

  module Loader = Loader.Make (C)
  module Input = Loader.Input

  module Output = struct
    type 'a t = { byte_ready : 'a } [@@deriving hardcaml]
  end

  let create _ ~clock ~reset inputs =
    let loader = Loader.create ~clock ~reset inputs in
    let read ~address =
      Memory.create ~address_bits:C.program_bits ~clear:gnd ~clock ~reset
        ~address:(mux2 loader.load loader.program_size address)
        ~write_enable:loader.load ~write_data:loader.opcode
    in
    ( {
        load = loader.load;
        program_size = loader.program_size;
        finished = loader.finished;
        too_large = loader.too_large;
        read;
      },
      { Output.byte_ready = loader.byte_ready } )

  let load (inputs : _ Input.t) (outputs : _ Output.t) ~cycle program =
    let send op =
      inputs.byte := Bits.of_char (Instruction.to_char op);
      cycle ();
      if not (Bits.to_bool !(outputs.byte_ready)) then
        failwith "loader not ready for byte"
    in
    holding inputs.byte_valid (fun () -> List.iter send program);
    pulse ~cycle inputs.finish
end

module Rom (C : Config.S) : S = struct
  type nonrec t = t

  module Input = Interface.Empty
  module Output = Interface.Empty

  let create program ~clock ~reset _ =
    let too_large = List.length program > C.program_capacity in
    let program = if too_large then [] else program in
    ( {
        load = gnd;
        program_size = of_int ~width:C.program_bits (List.length program);
        finished = of_bool (not too_large);
        too_large = of_bool too_large;
        read =
          Rom.create ~clock ~reset ~data_bits:Instruction.bits
            ~contents:(List.map Instruction.encode program);
      },
      Output.None )

  let load _ _ ~cycle:_ _ = ()
end
