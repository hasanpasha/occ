open Tacky_ir

let rec lower (program : Ast.t) : t =
  let counter = ref 0 in
  List.map (fun decl -> lower_top_declaration decl counter) program
  |> List.filter_map (fun x -> x)

and lower_top_declaration decl counter : function' option =
  match decl with
  | Ast.Function { name; params; body = Some body } ->
      let instructions = lower_block body counter @ [ Return (Constant 0) ] in
      Some { name; params; instructions }
  | Ast.Function _ -> None
  | Ast.Variable _ ->
      failwith "top level variable declaration are not supported yet"

and lower_inner_declaration decl counter =
  match decl with
  | Ast.Variable decl -> lower_variable_declaration decl counter
  | Ast.Function _ -> []

and lower_variable_declaration { name; init } counter =
  match init with
  | Some expr ->
      let value, instrs = lower_expression expr counter in
      instrs @ [ Copy { src = value; dst = Variable name } ]
  | None -> []

and lower_block blk counter =
  List.concat (List.map (fun item -> lower_block_item item counter) blk)

and lower_block_item item counter =
  match item with
  | Ast.Decl decl -> lower_inner_declaration decl counter
  | Ast.Stmt stmt -> lower_statement stmt counter

and lower_statement stmt counter =
  match stmt with
  | Ast.Return expr ->
      let value, instrs = lower_expression expr counter in
      instrs @ [ Return value ]
  | Ast.Expr expr ->
      let _, instrs = lower_expression expr counter in
      instrs
  | Ast.Null -> []
  | Ast.If { cond; then'; else' } -> (
      let cond_value, cond_instrs = lower_expression cond counter in
      let then_instrs = lower_statement then' counter in
      let end_lbl = make_unique "end" counter in
      match else' with
      | Some else' ->
          let else_lbl = make_unique "else" counter in
          let else_instrs = lower_statement else' counter in
          cond_instrs
          @ [ JumpIfZero { cond = cond_value; target = else_lbl } ]
          @ then_instrs
          @ [ Jump end_lbl; Label else_lbl ]
          @ else_instrs @ [ Label end_lbl ]
      | None ->
          cond_instrs
          @ [ JumpIfZero { cond = cond_value; target = end_lbl } ]
          @ then_instrs @ [ Label end_lbl ])
  | Ast.Compound blk -> lower_block blk counter
  | Ast.Goto target -> [ Jump target ]
  | Ast.LabeledStmt { stmt; label } ->
      let instrs = lower_statement stmt counter in
      Label label :: instrs
  | Ast.While { cond; body; label } ->
      let cond_value, cond_instrs = lower_expression cond counter in
      let break_label = make_break_label label in
      let continue_label = make_continue_label label in
      [ Label continue_label ] @ cond_instrs
      @ [ JumpIfZero { cond = cond_value; target = break_label } ]
      @ lower_statement body counter
      @ [ Jump continue_label; Label break_label ]
  | Ast.DoWhile { body; cond; label } ->
      let cond_value, cond_instrs = lower_expression cond counter in
      let start_label = Printf.sprintf "start.%s" label in
      let break_label = make_break_label label in
      let continue_label = make_continue_label label in
      [ Label start_label ]
      @ lower_statement body counter
      @ [ Label continue_label ] @ cond_instrs
      @ [
          JumpIfNotZero { cond = cond_value; target = start_label };
          Label break_label;
        ]
  | Ast.For { init; cond; post; body; label } ->
      let start_label = Printf.sprintf "start.%s" label in
      let break_label = make_break_label label in
      let continue_label = make_continue_label label in

      (match init with
      | Ast.VarDecl decl -> lower_variable_declaration decl counter
      | Ast.Expr (Some expr) ->
          let _, instrs = lower_expression expr counter in
          instrs
      | Ast.Expr None -> [])
      @ [ Label start_label ]
      @ (match cond with
        | Some expr ->
            let value, instrs = lower_expression expr counter in
            instrs @ [ JumpIfZero { cond = value; target = break_label } ]
        | None -> [])
      @ lower_statement body counter
      @ [ Label continue_label ]
      @ (match post with
        | Some expr ->
            let _, instrs = lower_expression expr counter in
            instrs
        | None -> [])
      @ [ Jump start_label; Label break_label ]
  | Ast.Break lbl -> [ Jump (make_break_label lbl) ]
  | Ast.Continue lbl -> [ Jump (make_continue_label lbl) ]
  | Ast.Switch { expr; stmt; label; cases; default } ->
      let break_label = make_break_label label in
      let value, instrs = lower_expression expr counter in
      instrs
      @ List.concat
          (List.map
             (fun (expr, label) ->
               let constant_value, _ = lower_expression expr counter in
               let cond = make_temp_var counter in
               [
                 Binary
                   {
                     operator = Equals;
                     src1 = value;
                     src2 = constant_value;
                     dst = cond;
                   };
                 JumpIfNotZero { cond; target = label };
               ])
             cases)
      @ (match default with
        | Some label -> [ Jump label ]
        | None -> [ Jump break_label ])
      @ lower_statement stmt counter
      @ [ Label break_label ]
  | Ast.Case { expr = _; stmt; label } | Ast.Default { stmt; label } -> (
      [ Label label ]
      @ match stmt with Some stmt -> lower_statement stmt counter | None -> [])

and lower_expression (expr : Ast.expression) (counter : int ref) :
    value * instruction list =
  match expr with
  | Ast.IntLit value -> (Constant value, [])
  | Ast.LeftUnary { operator = Plus; rhs } -> lower_expression rhs counter
  | Ast.LeftUnary { operator = (PlusPlus | MinusMinus) as operator; rhs } ->
      let operator =
        match operator with
        | PlusPlus -> Add
        | MinusMinus -> Subtract
        | _ -> failwith "unreachable"
      in
      let src_value, src_instrs = lower_expression rhs counter in
      let dst = make_temp_var counter in

      ( dst,
        src_instrs
        @ [
            Binary
              { operator; src1 = src_value; src2 = Constant 1; dst = src_value };
            Copy { src = src_value; dst };
          ] )
  | Ast.LeftUnary { operator; rhs } ->
      let operator =
        match operator with
        | Tilde -> Complement
        | Minus -> Negate
        | Excl -> Not
        | _ -> failwith "unreachable"
      in
      let src, src_instrs = lower_expression rhs counter in
      let dst = make_temp_var counter in

      (dst, src_instrs @ [ Unary { operator; src; dst } ])
  | Ast.RightUnary { operator = (PlusPlus | MinusMinus) as operator; lhs } ->
      let operator =
        match operator with
        | PlusPlus -> Add
        | MinusMinus -> Subtract
        | _ -> failwith "unreachable"
      in
      let src, src_instrs = lower_expression lhs counter in
      let dst = make_temp_var counter in

      ( dst,
        src_instrs
        @ [
            Copy { src; dst };
            Binary { operator; src1 = src; src2 = Constant 1; dst = src };
          ] )
  | Ast.RightUnary _ -> failwith "unhandled suffix unary"
  | Ast.Binary { operator = AmpAmp; lhs; rhs } ->
      let dst = make_temp_var counter in
      let false_label = make_unique "false" counter in
      let end_label = make_unique "end" counter in
      let src1, src1_instrs = lower_expression lhs counter in
      let src2, src2_instrs = lower_expression rhs counter in
      ( dst,
        src1_instrs
        @ [ JumpIfZero { cond = src1; target = false_label } ]
        @ src2_instrs
        @ [
            JumpIfZero { cond = src2; target = false_label };
            Copy { src = Constant 1; dst };
            Jump end_label;
            Label false_label;
            Copy { src = Constant 0; dst };
            Label end_label;
          ] )
  | Ast.Binary { operator = VerbarVarbar; lhs; rhs } ->
      let dst = make_temp_var counter in
      let true_label = make_unique "true" counter in
      let end_label = make_unique "end" counter in
      let src1, src1_instrs = lower_expression lhs counter in
      let src2, src2_instrs = lower_expression rhs counter in
      ( dst,
        src1_instrs
        @ [ JumpIfNotZero { cond = src1; target = true_label } ]
        @ src2_instrs
        @ [
            JumpIfNotZero { cond = src2; target = true_label };
            Copy { src = Constant 0; dst };
            Jump end_label;
            Label true_label;
            Copy { src = Constant 1; dst };
            Label end_label;
          ] )
  | Ast.Binary { operator; lhs; rhs } ->
      let operator =
        match operator with
        | Plus -> Add
        | Minus -> Subtract
        | Astrsk -> Multiply
        | Slash -> Divide
        | Percnt -> Remainder
        | Amp -> BinaryAnd
        | Verbar -> BinaryOr
        | Hat -> ExclusiveOr
        | LtLt -> ShiftLeft
        | GtGt -> ShiftRight
        | Lt -> Less
        | Gt -> Greater
        | LtEqual -> LessEquals
        | GtEqual -> GreaterEquals
        | EqualEqual -> Equals
        | ExclEqual -> NotEquals
        | _ -> failwith "unreachable"
      in
      let dst = make_temp_var counter in
      let src1, src1_instrs = lower_expression lhs counter in
      let src2, src2_instrs = lower_expression rhs counter in

      (dst, src1_instrs @ src2_instrs @ [ Binary { operator; src1; src2; dst } ])
  | Ast.Var name -> (Variable name, [])
  | Ast.Assignment { operator = Equal; lhs; rhs } ->
      let src, src_instrs = lower_expression rhs counter in
      let dst, _ = lower_expression lhs counter in
      (dst, src_instrs @ [ Copy { src; dst } ])
  | Ast.Assignment { operator; lhs; rhs } ->
      let operator =
        match operator with
        | PlusEqual -> Add
        | MinusEqual -> Subtract
        | AstrskEqual -> Multiply
        | SlashEqual -> Divide
        | PercntEqual -> Remainder
        | AmpEqual -> BinaryAnd
        | VerbarEqual -> BinaryOr
        | HatEqual -> ExclusiveOr
        | LtLtEqual -> ShiftLeft
        | GtGtEqual -> ShiftRight
        | _ -> failwith "unreachable"
      in
      let src, src_instrs = lower_expression rhs counter in
      let dst, _ = lower_expression lhs counter in
      (dst, src_instrs @ [ Binary { operator; src1 = dst; src2 = src; dst } ])
  | Ast.Conditional { cond; true'; false' } ->
      let dst = make_temp_var counter in
      let else_label = make_unique "else" counter in
      let end_label = make_unique "end" counter in

      let cond, cond_instrs = lower_expression cond counter in
      let true', true_instrs = lower_expression true' counter in
      let false', false_instrs = lower_expression false' counter in

      ( dst,
        cond_instrs
        @ [ JumpIfZero { cond; target = else_label } ]
        @ true_instrs
        @ [ Copy { src = true'; dst }; Jump end_label; Label else_label ]
        @ false_instrs
        @ [ Copy { src = false'; dst }; Label end_label ] )
  | Ast.FunctionCall { name; args } ->
      let dst = make_temp_var counter in
      let args' = List.map (fun arg -> lower_expression arg counter) args in
      let args, args_instrs =
        let vs, instrss = List.split args' in
        (vs, List.concat instrss)
      in
      (dst, args_instrs @ [ FunCall { fun_name = name; args; dst } ])

and make_unique prefix counter =
  let name = Printf.sprintf "%s.%d" prefix !counter in
  counter := !counter + 1;
  name

and make_temp_var counter =
  let name = Printf.sprintf "temp.var.%d" !counter in
  counter := !counter + 1;
  Variable name

and make_break_label seed = Printf.sprintf "break.%s" seed
and make_continue_label seed = Printf.sprintf "continue.%s" seed
