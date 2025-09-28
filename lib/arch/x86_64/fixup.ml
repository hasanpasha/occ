let rec fix (air : Ir.t) : Ir.t = List.map fix_subroutine air

and fix_subroutine { name; instructions } =
  let instructions = List.concat (List.map fix_instruction instructions) in
  { name; instructions }

and fix_instruction instr =
  let open Ir in
  match instr with
  | Idiv operand ->
      let r10d = Reg { base = R10; part = DW } in
      [ Mov { src = operand; dst = r10d }; Idiv r10d ]
  | Mov { src = Stack _ as src; dst = Stack _ as dst } ->
      let r10d = Reg { base = R10; part = DW } in
      [ Mov { src; dst = r10d }; Mov { src = r10d; dst } ]
  | Binary
      {
        operator = (Add | Sub) as operator;
        src1 = Stack _ as src1;
        src2 = Stack _ as src2;
      }
  | Binary
      {
        operator = (Imul | And | Or | Xor) as operator;
        src1;
        src2 = Stack _ as src2;
      } ->
      let r10d = Reg { base = R10; part = DW } in
      [
        Mov { src = src2; dst = r10d };
        Binary { operator; src1; src2 = r10d };
        Mov { src = r10d; dst = src2 };
      ]
  | Binary
      {
        operator = (Shl | Sal | Shr | Sar) as operator;
        src1 = (Reg _ | Stack _) as src1;
        src2;
      }
    when src2 != Reg { base = CX; part = LB } ->
      [
        Mov { src = src1; dst = Reg { base = CX; part = DW } };
        Binary { operator; src1 = Reg { base = CX; part = LB }; src2 };
      ]
  | Cmp { src1 = Stack _ as src1; src2 = Stack _ as src2 } ->
      let r10d = Reg { base = R10; part = DW } in
      [ Mov { src = src1; dst = r10d }; Cmp { src1 = r10d; src2 } ]
  | Cmp { src1; src2 = Imm _ as src2 } ->
      let r10d = Reg { base = R10; part = DW } in
      [ Mov { src = src2; dst = r10d }; Cmp { src1; src2 = r10d } ]
  | Push (Stack index) ->
      [
        Mov { src = Stack index; dst = Reg { base = AX; part = DW } };
        Push (Reg { base = AX; part = QW });
      ]
  | _ -> [ instr ]
