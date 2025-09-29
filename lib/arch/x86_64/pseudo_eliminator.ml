type state = {
  symbols : Validators.Type_checker.t;
  map : (string, int) Hashtbl.t;
  counter : int ref;
}

let rec eliminate (air : Ir.t) (symbols : Validators.Type_checker.t) : Ir.t =
  let state = { symbols; map = Hashtbl.create 32; counter = ref 0 } in
  List.map (fun top -> transform_top top state) air

and transform_top top state =
  match top with
  | Subroutine { name; global; instructions } ->
      let instructions =
        List.map (fun instr -> transform_instruction instr state) instructions
      in
      let allocated_stack_size = (abs !(state.counter) + 15) land lnot 15 in
      let instructions =
        Ir.AllocateStack allocated_stack_size :: instructions
      in
      Ir.Subroutine { name; global; instructions }
  | StaticVariable _ -> top

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
  | Pseudo name -> (
      match
        (Hashtbl.find_opt state.map name, Hashtbl.find_opt state.symbols name)
      with
      | Some v, _ -> Ir.Stack v
      | _, Some { typ = Int; attr = Static _ } -> Ir.Data name
      | _, _ ->
          state.counter := !(state.counter) - 4;
          Hashtbl.add state.map name !(state.counter);
          Ir.Stack !(state.counter))
  | _ -> operand
