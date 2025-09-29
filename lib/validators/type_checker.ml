type entry = { typ : typ; attr : attribute }
and typ = Int | Function of { arity : int }

and attribute =
  | Fun of { defined : bool; global : bool }
  | Static of { init : initial_value; global : bool }
  | Local

and initial_value = Tentative | Initial of int | NoInitializer
[@@deriving show]

type t = (string, entry) Hashtbl.t

let rec check (ast : Ast.t) : Ast.t * t =
  let table : t = Hashtbl.create 32 in
  let transformed =
    List.map (fun decl -> check_file_scope_declaration decl table) ast
  in
  (transformed, table)

and check_file_scope_declaration decl table =
  match decl with
  | Function func -> Function (check_function_declaration func table)
  | Variable vari -> Variable (check_file_scope_variable_declaration vari table)

and check_local_declaration (decl : Ast.declaration) table : Ast.declaration =
  match decl with
  | Function func -> Function (check_function_declaration func table)
  | Variable vari -> Variable (check_local_variable_declaration vari table)

and check_function_declaration { name; params; body; storage } table =
  let fun_type = Function { arity = List.length params } in
  let has_body = Option.is_some body in
  let global = storage <> Some Static in

  let already_defined, global =
    match Hashtbl.find_opt table name with
    | Some { typ; attr = _ } when typ <> fun_type ->
        failwith "Incompatible function declaration"
    | Some { typ = _; attr = Fun { defined = true; global = _ } } when has_body
      ->
        failwith "Function is defined more than once"
    | Some { typ = _; attr = Fun { defined = _; global = true } }
      when storage = Some Static ->
        failwith "Static function declaration follows non-static"
    | Some { typ = _; attr = Fun { defined; global } } -> (defined, global)
    | _ -> (false, global)
  in

  let attr = Fun { defined = already_defined || has_body; global } in

  Hashtbl.replace table name { typ = fun_type; attr };

  let body, params =
    match body with
    | Some blk ->
        ( Some (check_block blk table),
          List.map
            (fun param ->
              Hashtbl.replace table param { typ = Int; attr = Local };
              param)
            params )
    | None -> (body, params)
  in

  { name; params; body; storage }

and check_file_scope_variable_declaration { name; init; storage } table =
  let initial =
    match init with
    | Some (Ast.IntLit i) -> Initial i
    | None when storage = Some Extern -> NoInitializer
    | None -> Tentative
    | Some _ -> failwith "Non-constant initializer"
  in
  let global = storage <> Some Static in

  let global =
    match Hashtbl.find_opt table name with
    | Some { typ; attr = _ } when typ <> Int ->
        failwith "function redeclared as variable"
    | Some { typ = _; attr = Static { init = _; global = this_global } }
      when storage = Some Extern ->
        this_global
    | Some { typ = _; attr = Static { init = _; global = this_global } }
      when this_global <> global ->
        failwith [%string "Conflicting variable '%{name}' linkage"]
    | _ -> global
  in

  let initial =
    match (initial, Hashtbl.find_opt table name) with
    | ( Initial _,
        Some { typ = _; attr = Static { init = Initial _; global = _ } } ) ->
        failwith "Conflicting file scope variable definitions"
    | ( _,
        Some { typ = _; attr = Static { init = Initial _ as init; global = _ } }
      ) ->
        init
    | ( (Tentative | NoInitializer),
        Some { typ = _; attr = Static { init = Tentative; global = _ } } ) ->
        Tentative
    | _, _ -> initial
  in

  let attr = Static { init = initial; global } in

  Hashtbl.replace table name { typ = Int; attr };

  { name; init; storage }

and check_local_variable_declaration { name; init; storage } table =
  let init =
    if storage = Some Extern then (
      (if init <> None then
         failwith "Initializer on local extern variable declaration"
       else
         match Hashtbl.find_opt table name with
         | Some { typ; attr = _ } when typ <> Int ->
             failwith "Function redeclared as variable"
         | Some _ -> ()
         | None ->
             Hashtbl.replace table name
               {
                 typ = Int;
                 attr = Static { init = NoInitializer; global = true };
               });
      init)
    else if storage = Some Static then (
      let initial_value =
        match init with
        | Some (IntLit i) -> Initial i
        | None -> Initial 0
        | _ -> failwith "Non-constant initializer on local static variable"
      in
      Hashtbl.replace table name
        { typ = Int; attr = Static { init = initial_value; global = false } };
      init)
    else (
      Hashtbl.replace table name { typ = Int; attr = Local };
      Option.map (fun init -> check_expression init table) init)
  in

  { name; init; storage }

and check_block blk table =
  List.map (fun item -> check_block_item item table) blk

and check_block_item item table =
  match item with
  | Stmt stmt -> Stmt (check_statement stmt table)
  | Decl decl -> Decl (check_local_declaration decl table)

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
        | VarDecl { name = _; init = _; storage = Some _ } ->
            failwith "storage class in `for` init variable declaration"
        | VarDecl decl ->
            Ast.VarDecl (check_local_variable_declaration decl table)
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
      | { typ = Int; attr = _ } -> Var name
      | _ -> failwith "function name used as variable")
  | Ast.FunctionCall { name; args } ->
      (match Hashtbl.find table name with
      | { typ = Int; attr = _ } -> failwith "variable used as function name"
      | { typ = Function { arity }; attr = _ } when arity != List.length args ->
          failwith "function called with the wrong number of arguments"
      | _ -> ());

      let args = List.map (fun arg -> check_expression arg table) args in
      FunctionCall { name; args }
