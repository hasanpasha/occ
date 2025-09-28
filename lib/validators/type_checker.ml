type typ = Int | Function of { arity : int; defined : bool } [@@deriving eq]
type t = (string, typ) Hashtbl.t

let rec check (ast : Ast.t) : Ast.t =
  let table : t = Hashtbl.create 32 in
  List.map (fun decl -> check_declaration decl table) ast

and check_declaration decl table =
  match decl with
  | Variable vari -> Variable (check_variable_declaration vari table)
  | Function func -> Function (check_function_declaration func table)

and check_variable_declaration { name; init } table =
  Hashtbl.add table name Int;
  let init = Option.map (fun init -> check_expression init table) init in
  { name; init }

and check_function_declaration { name; params; body } table =
  let has_body = Option.is_some body in
  let this_arity = List.length params in

  let already_defined =
    match Hashtbl.find_opt table name with
    | Some (Function { arity; defined = true })
      when arity = this_arity && has_body ->
        failwith "function is defined more than once"
    | Some (Function { arity; defined }) when arity = this_arity -> defined
    | Some _ -> failwith "incompatible function declaration"
    | None -> false
  in

  let fun_t =
    Function { arity = this_arity; defined = already_defined || has_body }
  in

  Hashtbl.add table name fun_t;

  let body, params =
    match body with
    | Some blk ->
        ( Some (check_block blk table),
          List.map
            (fun param ->
              Hashtbl.add table param Int;
              param)
            params )
    | None -> (body, params)
  in

  { name; params; body }

and check_block blk table =
  List.map (fun item -> check_block_item item table) blk

and check_block_item item table =
  match item with
  | Stmt stmt -> Stmt (check_statement stmt table)
  | Decl decl -> Decl (check_declaration decl table)

and check_statement stmt table =
  match stmt with
  | Return expr -> Return (check_expression expr table)
  | Expr expr -> Expr (check_expression expr table)
  | If { cond; then'; else' } ->
      If
        {
          cond = check_expression cond table;
          then' = check_statement then' table;
          else' = Option.map (fun stmt -> check_statement stmt table) else';
        }
  | LabeledStmt { stmt; label } ->
      LabeledStmt { stmt = check_statement stmt table; label }
  | Compound blk -> Compound (check_block blk table)
  | While { cond; body; label } ->
      While
        {
          cond = check_expression cond table;
          body = check_statement body table;
          label;
        }
  | DoWhile { body; cond; label } ->
      DoWhile
        {
          body = check_statement body table;
          cond = check_expression cond table;
          label;
        }
  | For { init; cond; post; body; label } ->
      let init =
        match init with
        | VarDecl decl -> Ast.VarDecl (check_variable_declaration decl table)
        | Expr expr ->
            Expr (Option.map (fun expr -> check_expression expr table) expr)
      in
      For
        {
          init;
          cond = Option.map (fun expr -> check_expression expr table) cond;
          post = Option.map (fun expr -> check_expression expr table) post;
          body = check_statement body table;
          label;
        }
  | Switch { expr; stmt; label; cases; default } ->
      Switch
        {
          expr = check_expression expr table;
          stmt = check_statement stmt table;
          label;
          cases;
          default;
        }
  | Case { expr; stmt; label } ->
      Case
        {
          expr = check_expression expr table;
          stmt = Option.map (fun stmt -> check_statement stmt table) stmt;
          label;
        }
  | Default { stmt; label } ->
      Default
        {
          stmt = Option.map (fun stmt -> check_statement stmt table) stmt;
          label;
        }
  | Null | Goto _ | Break _ | Continue _ -> stmt

and check_expression expr table =
  match expr with
  | IntLit _ -> expr
  | Ast.LeftUnary { operator; rhs } ->
      LeftUnary { operator; rhs = check_expression rhs table }
  | Ast.RightUnary { operator; lhs } ->
      RightUnary { operator; lhs = check_expression lhs table }
  | Ast.Binary { operator; lhs; rhs } ->
      Binary
        {
          operator;
          lhs = check_expression lhs table;
          rhs = check_expression rhs table;
        }
  | Ast.Assignment { operator; lhs; rhs } ->
      Assignment
        {
          operator;
          lhs = check_expression lhs table;
          rhs = check_expression rhs table;
        }
  | Ast.Conditional { cond; true'; false' } ->
      Conditional
        {
          cond = check_expression cond table;
          true' = check_expression true' table;
          false' = check_expression false' table;
        }
  | Ast.Var name -> (
      match Hashtbl.find table name with
      | Int -> Var name
      | _ -> failwith "function name used as variable")
  | Ast.FunctionCall { name; args } ->
      (match Hashtbl.find table name with
      | Int -> failwith "variable used as function name"
      | Function { arity; defined = _ } when arity != List.length args ->
          failwith "function called with the wrong number of arguments"
      | _ -> ());

      let args = List.map (fun arg -> check_expression arg table) args in
      FunctionCall { name; args }
