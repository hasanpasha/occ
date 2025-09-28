type state = { map : (string, int) Hashtbl.t; counter : int ref }

let rec eliminate (air : Ir.t) : Ir.t = List.map transform_subroutine air

and transform_subroutine { name; instructions } =
  let state = { map = Hashtbl.create 32; counter = ref 0 } in
  let instructions =
    List.map (fun instr -> transform_instruction instr state) instructions
  in
  let allocated_stack_size = (abs !(state.counter) + 15) land lnot 15 in
  let instructions = Ir.AllocateStack allocated_stack_size :: instructions in
  { name; instructions }

and transform_instruction instr state =
  match instr with
  | Mov { src; dst } ->
      Mov
        { src = transform_operand src state; dst = transform_operand dst state }
  | Unary { operator; operand } ->
      Unary { operator; operand = transform_operand operand state }
  | Binary { operator; src1; src2 } ->
      Binary
        {
          operator;
          src1 = transform_operand src1 state;
          src2 = transform_operand src2 state;
        }
  | Cmp { src1; src2 } ->
      Cmp
        {
          src1 = transform_operand src1 state;
          src2 = transform_operand src2 state;
        }
  | Idiv operand -> Idiv (transform_operand operand state)
  | SetCC { cond; dst } -> SetCC { cond; dst = transform_operand dst state }
  | Push operand -> Push (transform_operand operand state)
  | _ -> instr

and transform_operand operand state =
  match operand with
  | Pseudo name ->
      let index =
        match Hashtbl.find_opt state.map name with
        | Some v -> v
        | None ->
            state.counter := !(state.counter) - 4;
            Hashtbl.add state.map name !(state.counter);
            !(state.counter)
      in
      Stack index
  | _ -> operand
