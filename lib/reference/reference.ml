type outcome = Halted | End_of_input | Unmatched_brackets | Program_too_large
[@@deriving sexp_of, equal]

let jump_table program =
  let table = Array.make (Array.length program) 0 in
  let rec go pc open_loops =
    if pc = Array.length program then
      if open_loops = [] then Some table else None
    else
      match ((program.(pc) : Instruction.t), open_loops) with
      | Loop_start, _ -> go (pc + 1) (pc :: open_loops)
      | Loop_end, [] -> None
      | Loop_end, start :: rest ->
          table.(start) <- pc;
          table.(pc) <- start;
          go (pc + 1) rest
      | _ -> go (pc + 1) open_loops
  in
  go 0 []

module Make (C : Config.S) = struct
  let tape_length = Config.entries C.pointer_bits

  let execute ~max_steps ~read ~write program jumps =
    let tape = Array.make tape_length 0 in
    let pointer = ref 0 in
    let cell () = tape.(!pointer) in
    let set value = tape.(!pointer) <- value land 0xff in
    let move delta = pointer := (!pointer + delta) land (tape_length - 1) in
    let rec step pc steps =
      let continue pc = step pc (steps + 1) in
      let next = pc + 1 in
      if pc = Array.length program then Some Halted
      else if steps = max_steps then None
      else
        match (program.(pc) : Instruction.t) with
        | Move_right ->
            move 1;
            continue next
        | Move_left ->
            move (-1);
            continue next
        | Increment ->
            set (cell () + 1);
            continue next
        | Decrement ->
            set (cell () - 1);
            continue next
        | Output ->
            write (Char.chr (cell ()));
            continue next
        | Input -> (
            match read () with
            | None -> Some End_of_input
            | Some byte ->
                set (Char.code byte);
                continue next)
        | Loop_start -> continue (if cell () = 0 then jumps.(pc) + 1 else next)
        | Loop_end -> continue (if cell () <> 0 then jumps.(pc) + 1 else next)
    in
    step 0 0

  let run ~max_steps ~read ~write program =
    let program = Array.of_list program in
    if Array.length program > C.program_capacity then Some Program_too_large
    else
      match jump_table program with
      | None -> Some Unmatched_brackets
      | Some jumps -> execute ~max_steps ~read ~write program jumps
end
