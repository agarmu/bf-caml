open Hardcaml
open Bf_caml

let decoder () =
  let decoded = Instruction.decode (Signal.input "byte" 8) in
  let sim =
    Cyclesim.create
      (Circuit.create_exn ~name:"decode"
         [
           Signal.output "valid" decoded.valid;
           Signal.output "value" decoded.value;
         ])
  in
  let byte = Cyclesim.in_port sim "byte" in
  let valid = Cyclesim.out_port sim "valid" in
  let value = Cyclesim.out_port sim "value" in
  let of_raw raw =
    List.find
      (fun op ->
        Bits.equal
          (Instruction.Opcode.to_raw (Instruction.Opcode.Of_bits.of_enum op))
          raw)
      Instruction.all
  in
  fun c ->
    byte := Bits.of_char c;
    Cyclesim.cycle sim;
    if Bits.to_bool !valid then Some (of_raw !value) else None

let%expect_test "decode matches of_char for every byte" =
  let decode = decoder () in
  String.init 256 Char.chr
  |> String.iter (fun c ->
      match (decode c, Instruction.of_char c) with
      | Some op, Some op' when op = op' -> print_char c
      | None, None -> ()
      | _ -> Printf.printf "\nmismatch at %C\n" c);
  [%expect {| +,-.<>[] |}]
