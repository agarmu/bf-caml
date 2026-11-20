open Hardcaml
open Bf_caml

module Config = Config.Make (struct
  let max_program_size = 16
  let tape_length = 16
end)

module Loader = Loader.Make (Config)

let create () =
  let loader =
    Loader.create ~clock:(Signal.input "clock" 1)
      ~reset:(Signal.input "reset" 1)
      (Loader.Input.map2 Loader.Input.port_names Loader.Input.port_widths
         ~f:Signal.input)
  in
  Cyclesim.create
    (Circuit.create_exn ~name:"loader"
       (Loader.Output.to_list
          (Loader.Output.map2 Loader.Output.port_names loader ~f:Signal.output)))

let set sim name value =
  let port = Cyclesim.in_port sim name in
  port := Bits.of_int ~width:(Bits.width !port) value

let get ?clock_edge sim name =
  Bits.to_int !(Cyclesim.out_port ?clock_edge sim name)

let pulse sim name =
  set sim name 1;
  Cyclesim.cycle sim;
  set sim name 0

let opcode_char raw =
  List.find
    (fun op ->
      Bits.to_int
        (Instruction.Opcode.to_raw (Instruction.Opcode.Of_bits.of_enum op))
      = raw)
    Instruction.all
  |> Instruction.to_char

let send sim byte =
  set sim "byte_valid" 1;
  set sim "byte_data" (Char.code byte);
  Cyclesim.cycle sim;
  set sim "byte_valid" 0;
  if get ~clock_edge:Before sim "load" = 1 then
    Some (opcode_char (get ~clock_edge:Before sim "opcode"))
  else None

let send_all sim bytes =
  String.to_seq bytes |> Seq.filter_map (send sim) |> String.of_seq

let status sim =
  Printf.printf "size=%d finished=%d too_large=%d byte_ready=%d\n"
    (get sim "program_size") (get sim "finished") (get sim "too_large")
    (get sim "byte_ready")

let fresh () =
  let sim = create () in
  pulse sim "reset";
  sim

let%expect_test "loads only instruction bytes, in order" =
  let sim = fresh () in
  print_endline (send_all sim "a+b-c[d]e.,<> \n!");
  pulse sim "finish";
  status sim;
  [%expect
    {|
    +-[].,<>
    size=8 finished=1 too_large=0 byte_ready=0
    |}]

let%expect_test "a full program can still finish" =
  let sim = fresh () in
  Printf.printf "loaded %d\n"
    (String.length (send_all sim (String.make 15 '+')));
  pulse sim "finish";
  status sim;
  [%expect
    {|
    loaded 15
    size=15 finished=1 too_large=0 byte_ready=0
    |}]

let%expect_test "an instruction past capacity marks the program too large" =
  let sim = fresh () in
  Printf.printf "loaded %d of 20\n"
    (String.length (send_all sim (String.make 20 '+')));
  status sim;
  pulse sim "finish";
  status sim;
  [%expect
    {|
    loaded 15 of 20
    size=15 finished=0 too_large=1 byte_ready=1
    size=15 finished=0 too_large=1 byte_ready=1
    |}]

let%expect_test "nothing loads after finishing" =
  let sim = fresh () in
  ignore (send_all sim "+-");
  pulse sim "finish";
  Printf.printf "%S\n" (send_all sim "<>");
  status sim;
  [%expect {|
    ""
    size=2 finished=1 too_large=0 byte_ready=0
    |}]

let%expect_test "reset starts a fresh program" =
  let sim = fresh () in
  ignore (send_all sim "+-+");
  pulse sim "finish";
  pulse sim "reset";
  print_endline (send_all sim "[]");
  status sim;
  [%expect {|
    []
    size=2 finished=0 too_large=0 byte_ready=1
    |}]
