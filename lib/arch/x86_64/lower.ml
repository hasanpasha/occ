let rec lower (tir : Tacky_ir.t) : Ir.t =
  List.map lower_function tir |> Pseudo_eliminator.eliminate |> Fixup.fix

and call_regs : Ir.regbase list = [ DI; SI; DX; CX; R8; R9 ]

and lower_function { name; params; instructions } : Ir.subroutine =
  let instructions = List.concat (List.map lower_instruction instructions) in

  let params_instrs =
    List.mapi
      (fun i param ->
        let src =
          match i < List.length call_regs with
          | true -> Ir.Reg { base = List.nth call_regs i; part = DW }
          | false -> Ir.Stack ((i - List.length call_regs + 2) * 8)
        in
        Ir.Mov { src; dst = Ir.Pseudo param })
      params
  in

  let instructions = params_instrs @ instructions in

  { name; instructions }

and lower_instruction instr =
  let open Ir in
  match instr with
  | Return value ->
      let operand = lower_value value in
      [ Mov { src = operand; dst = Reg { base = AX; part = DW } }; Ret ]
  | Unary { operator = Not; src; dst } ->
      let src' = lower_value src in
      let dst' = lower_value dst in
      [
        Cmp { src1 = Imm 0l; src2 = src' };
        Mov { src = Imm 0l; dst = dst' };
        SetCC { cond = E; dst = dst' };
      ]
  | Unary { operator; src; dst } ->
      let operator =
        match operator with
        | Negate -> Neg
        | Complement -> Not
        | _ -> failwith "unreachable"
      in
      let src' = lower_value src in
      let dst' = lower_value dst in
      [ Mov { src = src'; dst = dst' }; Unary { operator; operand = dst' } ]
  | Binary
      {
        operator =
          ( Add | Subtract | Multiply | BinaryAnd | BinaryOr | ExclusiveOr
          | ShiftLeft | ShiftRight ) as operator;
        src1;
        src2;
        dst;
      } ->
      let operator =
        match operator with
        | Add -> Add
        | Subtract -> Sub
        | Multiply -> Imul
        | BinaryAnd -> And
        | BinaryOr -> Or
        | ExclusiveOr -> Xor
        | ShiftLeft -> Sal
        | ShiftRight -> Sar
        | _ -> failwith "unreachable"
      in
      let src1' = lower_value src1 in
      let src2' = lower_value src2 in
      let dst' = lower_value dst in

      [
        Mov { src = src1'; dst = dst' };
        Binary { operator; src1 = src2'; src2 = dst' };
      ]
  | Binary { operator = (Divide | Remainder) as operator; src1; src2; dst } ->
      let src1' = lower_value src1 in
      let src2' = lower_value src2 in
      let dst' = lower_value dst in

      let base = match operator with Divide -> AX | _ -> DX in

      [
        Mov { src = src1'; dst = Reg { base = AX; part = DW } };
        Cdq;
        Idiv src2';
        Mov { src = Reg { base; part = DW }; dst = dst' };
      ]
  | Binary
      {
        operator =
          (Equals | NotEquals | Less | LessEquals | Greater | GreaterEquals) as
          operator;
        src1;
        src2;
        dst;
      } ->
      let cond =
        match operator with
        | Equals -> E
        | NotEquals -> NE
        | Less -> L
        | LessEquals -> LE
        | Greater -> G
        | GreaterEquals -> GE
        | _ -> failwith "unreachable"
      in
      let src1' = lower_value src1 in
      let src2' = lower_value src2 in
      let dst' = lower_value dst in

      [
        Cmp { src1 = src2'; src2 = src1' };
        Mov { src = Imm 0l; dst = dst' };
        SetCC { cond; dst = dst' };
      ]
  | Copy { src; dst } ->
      let src' = lower_value src in
      let dst' = lower_value dst in
      [ Mov { src = src'; dst = dst' } ]
  | Jump target -> [ Jmp target ]
  | Label label -> [ Label label ]
  | JumpIfZero { cond; target } ->
      let cond' = lower_value cond in
      [ Cmp { src1 = cond'; src2 = Imm 0l }; JmpCC { cond = E; target } ]
  | JumpIfNotZero { cond; target } ->
      let cond' = lower_value cond in
      [ Cmp { src1 = cond'; src2 = Imm 0l }; JmpCC { cond = NE; target } ]
  | FunCall { fun_name; args; dst } ->
      let regs_args = List.take (List.length call_regs) args in
      let stack_args = List.drop (List.length call_regs) args in
      let stack_padding = if List.length stack_args mod 2 != 0 then 8 else 0 in
      let bytes_to_remove = (8 * List.length stack_args) + stack_padding in
      let dst = lower_value dst in

      let instrs =
        (if stack_padding != 0 then [ AllocateStack stack_padding ] else [])
        @ List.mapi
            (fun i arg ->
              let arg = lower_value arg in
              Mov
                {
                  src = arg;
                  dst = Reg { base = List.nth call_regs i; part = DW };
                })
            regs_args
        @ List.map
            (fun arg ->
              let arg = lower_value arg in
              Push arg)
            (List.rev stack_args)
        @ [ Call fun_name ]
        @ (if bytes_to_remove != 0 then [ DeallocateStack bytes_to_remove ]
           else [])
        @ [ Mov { src = Reg { base = AX; part = DW }; dst } ]
      in

      instrs

and lower_value = function
  | Constant value -> Imm (Int32.of_int value)
  | Tacky_ir.Variable name -> Pseudo name
