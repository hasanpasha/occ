type t =
{
  lexer: Lexer.t;
  cur: Token.t option;
  peek: Token.t option;
}
[@@deriving show]

type precedence =
| Nothing
| Assignment
| Ternary
| Lor
| Land
| Bor
| Bxor
| Band
| Equality
| Comparsion
| Shift
| Term
| Factor
| LeftUnary
| RightUnary
| Primary
[@@deriving show, eq, enum]

let prec_succ prec = precedence_of_enum (precedence_to_enum prec) |> Option.get

let advance parser =
  let { lexer; cur = _; peek } = parser in
  let cur = peek in
  let lexer, peek = Lexer.next_token lexer in
  { lexer; cur; peek }

let init (lexer: Lexer.t) : t =
  let parser = { lexer = lexer; cur = None; peek = None } in
  let parser = advance parser in
  let parser = advance parser in
  parser

let rec program parser : Ast.program =
  let rec program' parser decls =
    match parser.cur with
    | Some _ -> 
      let parser, decl = declaration parser in
      program' parser (decl :: decls)
    | None -> parser, decls
  in
  let _, decls = program' parser [] in
  decls

and declaration parser =
  let parser, type' = expect parser Token.Int in
  match parser.peek with
  | Some Token.LParen -> function' parser type'
  | _ -> variable parser type'

and function' parser type' =
  let _ = type' in
  let parser, name = expect_identifier parser in
  let parser = consume parser Token.LParen in
  let parser = consume parser Token.Void in
  let parser = consume parser Token.RParen in

  let parser, body = block parser in 

  parser, Ast.Function { name; body }

and variable parser type' =
  let _ = type' in
  let parser, name = expect_identifier parser in
  let parser, init = match parser.cur with
  | Some Token.Equal -> 
    let parser = consume parser Token.Equal in
    let parser, expr = expression parser in
    parser, Some expr
  | Some _ -> parser, None
  | None -> failwith "unexpected end of token stream"
  in
  let parser = consume parser Token.Semi in
  parser, Ast.Variable { name; init }

and block parser =
  let parser = consume parser Token.LCub in
  let rec block' parser items =
    match parser.cur with
    | Some Token.RCub -> parser, items
    | Some _ -> 
      let parser, item = block_item parser in
      block' parser (item :: items)
    | None -> failwith "unexpected end of token stream"
  in
  let parser, items = block' parser [] in
  let parser = consume parser Token.RCub in
  parser, List.rev items

and block_item parser =
  match parser.cur with
  | Some Token.Int -> 
    let parser, decl = declaration parser in
    parser, (Ast.Decl decl)
  | Some _ -> 
    let parser, stmt = statement parser in
    parser, (Ast.Stmt stmt)
  | None -> failwith "unexpected end of token stream"

and statement parser =
  let open Token in
  match parser.cur with
  | Some Return -> return_stmt parser
  | Some Semi -> null_stmt parser
  | Some If -> if_stmt parser
  | Some Goto -> goto_stmt parser
  | Some Ident _ when parser.peek = Some Token.Colon -> labeled_stmt_stmt parser
  | Some LCub -> compound_stmt parser
  | Some Break -> break_stmt parser
  | Some Continue -> continue_stmt parser
  | Some While -> while_stmt parser
  | Some Do -> do_while_stmt parser
  | Some For -> for_stmt parser
  | Some Switch -> switch_stmt parser
  | Some Case -> case_stmt parser
  | Some Default -> default_stmt parser
  | Some _ -> expr_stmt parser
  | None -> failwith "unexpected end of token stream" 

and return_stmt parser =
  let parser = consume parser Token.Return in
  let parser, expr = expression parser in
  let parser = consume parser Token.Semi in
  parser, Ast.Return expr

and expr_stmt parser =
  let parser, expr = expression parser in
  let parser = consume parser Token.Semi in
  parser, Ast.Expr expr

and null_stmt parser = 
  let parser = consume parser Token.Semi in
  parser, Ast.Null

and if_stmt parser = 
  let parser = consume parser Token.If in
  let parser = consume parser Token.LParen in
  let parser, cond = expression parser in
  let parser = consume parser Token.RParen in
  let parser, then' = statement parser in
  let parser, else' = match parser.cur with
  | Some Token.Else ->
    let parser = consume parser Token.Else in 
    let parser, stmt = statement parser in
    parser, Some stmt
  | _ -> parser, None
  in
  parser, Ast.If { cond; then'; else' }

and goto_stmt parser =
  let parser = consume parser Token.Goto in
  let parser, target = expect_identifier parser in
  let parser = consume parser Token.Semi in
  parser, Ast.Goto target

and labeled_stmt_stmt parser =
  let parser, label = expect_identifier parser in
  let parser = consume parser Token.Colon in
  let parser, stmt = statement parser in
  parser, Ast.LabeledStmt { label; stmt }

and compound_stmt parser =
  let parser, blk = block parser in
  parser, Ast.Compound blk

and break_stmt parser = 
  let parser = consume parser Token.Break in
  let parser = consume parser Token.Semi in
  parser, Ast.Break

and continue_stmt parser = 
  let parser = consume parser Token.Continue in
  let parser = consume parser Token.Semi in
  parser, Ast.Break

and while_stmt parser =
  let parser = consume parser Token.While in
  
  let parser = consume parser Token.LParen in
  let parser, cond = expression parser in
  let parser = consume parser Token.RParen in
  
  let parser, body = statement parser in
  parser, Ast.While { cond; body }

and do_while_stmt parser =
  let parser = consume parser Token.Do in
  
  let parser, body = statement parser in

  let parser = consume parser Token.While in
  let parser = consume parser Token.LParen in
  let parser, cond = expression parser in
  let parser = consume parser Token.RParen in
  let parser = consume parser Token.Semi in 
  
  parser, Ast.DoWhile { body; cond }

and for_stmt parser =
  let parser = consume parser Token.For in
  let parser = consume parser Token.LParen in
  let parser, init = match parser.cur with
  | Some Token.Semi -> 
    let parser = consume parser Token.Semi in
    parser, Ast.Expr None
  | Some Token.Int -> 
    let parser, decl = declaration parser in
    parser, Ast.Decl decl
  | _ -> 
    let parser, expr = expression parser in
    let parser = consume parser Token.Semi in
    parser, Ast.Expr (Some expr)
  in
  let parser, cond = match parser.cur with
  | Some Token.Semi ->
    parser, None
  | _ ->
    let parser, expr = expression parser in
    parser, Some expr
  in
  let parser = consume parser Token.Semi in
  let parser, post = match parser.cur with
  | Some Token.RParen ->
    parser, None
  | _ ->
    let parser, expr = expression parser in
    parser, Some expr
  in 
  let parser = consume parser Token.RParen in
  let parser, body = statement parser in
  parser, Ast.For { init; cond; post; body }


and switch_stmt parser =
  let parser = consume parser Token.Switch in
  let parser = consume parser Token.LParen in
  let parser, expr = expression parser in
  let parser = consume parser Token.RParen in
  let parser, stmt = statement parser in
  parser, Ast.Switch { expr; stmt }

and case_stmt parser =
  let parser = consume parser Token.Case in
  let parser, expr = expression parser in
  let parser = consume parser Token.Colon in
  let parser, stmt = match parser.cur with
  | Some Token.Case | Some Token.Default | Some Token.RParen -> parser, None
  | _ -> 
    let parser, stmt = statement parser in
    parser, Some stmt
  in
  parser, Ast.Case { expr; stmt }
  

and default_stmt parser =
  let parser = consume parser Token.Default in
  let parser = consume parser Token.Colon in
  let parser, stmt = match parser.cur with
  | Some Token.Case | Some Token.Default | Some Token.RParen -> parser, None
  | _ -> 
    let parser, stmt = statement parser in
    parser, Some stmt
  in
  parser, Ast.Default stmt 

and expression parser = expression_prec parser Assignment

and get_prefix_fn (cur: Token.t) ?(peek: Token.t option = None) : (t -> t * Ast.expression) = 
  let open Token in
  match cur, peek with
  | LParen, _ -> group_expr
  | Ident _, _ -> variable_expr
  | IntLit _, _ -> int_lit_expr
  | Tilde, _ | Excl, _ | PlusPlus, _ | MinusMinus, _ | Plus, _ | Minus, _  -> left_unary_expr 
  | _, _ -> failwith "can't find prefix function for this situation"

and get_infix_fn (cur: Token.t) : (t -> Ast.expression -> t * Ast.expression) =
let open Token in
match cur with
| PlusPlus | MinusMinus -> right_unary_expr
| Plus | Minus | Astrsk | Slash | Percnt | LtLt | GtGt | Amp | Hat | Verbar | Lt 
| Gt | LtEqual | GtEqual | EqualEqual | ExclEqual | AmpAmp | VerbarVarbar -> binary_expr
| Quest -> conditional_expr
| Equal | PlusEqual | MinusEqual | AstrskEqual | SlashEqual | PercntEqual
| AmpEqual | HatEqual | VerbarEqual | LtLtEqual | GtGtEqual -> assignment_expr
| _ -> failwith "can't find infix function for this situation"


and get_prec token = 
let open Token in
match token with
| PlusPlus | MinusMinus -> RightUnary
| Plus | Minus -> Term
| Astrsk | Slash | Percnt -> Factor
| LtLt | GtGt -> Shift
| Amp -> Band
| Hat -> Bxor
| Verbar -> Bor
| Lt | Gt | LtEqual | GtEqual -> Comparsion
| EqualEqual | ExclEqual -> Equality
| AmpAmp -> Land
| VerbarVarbar -> Lor
| Quest -> Ternary
| Equal | PlusEqual | MinusEqual | AstrskEqual | SlashEqual | PercntEqual
| AmpEqual | HatEqual | VerbarEqual | LtLtEqual | GtGtEqual -> Assignment
| _ -> Nothing

and expression_prec (parser: t) (prec: precedence) : t * Ast.expression =
  let parser, lhs = (get_prefix_fn (parser.cur |> Option.get) ~peek:parser.peek) parser in
  let rec parse_infix parser lhs = 
    let cur_prec = get_prec (parser.cur |> Option.get) in
    match cur_prec >= prec with
    | true -> 
      let parser, rhs = (get_infix_fn (parser.cur |> Option.get)) parser lhs in
      parse_infix parser rhs
    | false -> parser, lhs
  in
  parse_infix parser lhs


and int_lit_expr parser =
  let parser, int = expect_int_lit parser in
  parser, Ast.IntLit int

and group_expr parser =
  let parser = consume parser Token.LParen in
  let parser, expr = expression parser in
  let parser = consume parser Token.RParen in
  parser, expr

and variable_expr parser =
  let parser, name = expect_identifier parser in
  parser, Ast.Var name

and left_unary_expr parser =
  let parser, operator = expect_any parser in
  let parser, rhs = expression_prec parser LeftUnary in
  parser, Ast.LeftUnary { operator; rhs }

and right_unary_expr parser lhs =
  let parser, operator = expect_any parser in
  parser, Ast.RightUnary { operator; lhs }

and binary_expr parser lhs =
  let parser, operator = expect_any parser in
  let parser, rhs = expression_prec parser (prec_succ (get_prec operator)) in 
  parser, Ast.Binary { operator; lhs; rhs }

and assignment_expr parser lhs =
  let parser, operator = expect_any parser in
  let parser, rhs = expression_prec parser (prec_succ (get_prec operator)) in 
  parser, Ast.Assignment { operator; lhs; rhs }

and conditional_expr parser cond =
  let parser = consume parser Token.Quest in
  let parser, true' = expression parser in
  let parser = consume parser Token.Colon in
  let parser, false' = expression_prec parser Ternary in 
  parser, Ast.Conditional { cond; true'; false' }

and expect parser expected_token =
  match parser.cur with
  | Some cur_token when cur_token = expected_token -> advance parser, cur_token
  | Some cur_token -> failwith (Printf.sprintf "unexpected token, expected %s, found: %s" (Token.show expected_token) (Token.show cur_token))
  | None -> failwith "unexpected end of token stream"

and expect_identifier parser =
  match parser.cur with
  | Some Token.Ident name -> 
    let parser = advance parser in
    parser, name
  | Some tok -> failwith (Printf.sprintf "expected identifier, found %s" (Token.show tok))
  | _ -> failwith "expected identifier, found nothing"

and expect_int_lit parser =
  match parser.cur with
  | Some Token.IntLit value -> 
    let parser = advance parser in
    parser, value
  | Some tok -> failwith (Printf.sprintf "expected int_lit, found %s" (Token.show tok)) 
  | _ -> failwith "expected int_lit, found nothing"

and consume parser token =
  let parser, _ = expect parser token in
  parser

and expect_any parser =
  let cur = parser.cur |> Option.get in
  let parser = advance parser in
  parser, cur