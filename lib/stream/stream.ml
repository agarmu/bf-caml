open Hardcaml

module type Data = sig
  val bits : int
end

module Make (D : Data) = struct
  module Source = struct
    type 'a t = { valid : 'a; data : 'a [@bits D.bits] } [@@deriving hardcaml]
  end

  module Sink = struct
    type 'a t = { ready : 'a } [@@deriving hardcaml]
  end

  let transfer (source : Signal.t Source.t) (sink : Signal.t Sink.t) =
    Signal.(source.valid &: sink.ready)

  let fifo ~capacity ~clock ~clear (source : Signal.t Source.t)
      (sink : Signal.t Sink.t) =
    if capacity < 2 then invalid_arg "fifo capacity must be at least 2";
    let fifo =
      Fifo.create ~showahead:true () ~capacity:(capacity - 1) ~clock ~clear
        ~wr:source.valid ~d:source.data ~rd:sink.ready
    in
    ( { Sink.ready = Signal.( ~: ) fifo.full },
      { Source.valid = Signal.( ~: ) fifo.empty; data = fifo.q } )
end

module Byte = Make (struct
  let bits = 8
end)
