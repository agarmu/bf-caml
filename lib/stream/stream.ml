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
end

module Byte = Make (struct
  let bits = 8
end)
