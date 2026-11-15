(* infix for function composition *)
let ( %> ) = Fun.compose

let holding port f =
  port := Hardcaml.Bits.vdd;
  f ();
  port := Hardcaml.Bits.gnd

let pulse ~cycle port = holding port cycle
