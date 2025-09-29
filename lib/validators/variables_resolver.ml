let src =
  Logs.Src.create "occ.variables_resolver"
    ~doc:"logs occ's variables resolver events"

module Log = (val Logs.src_log src : Logs.LOG)

module Env = struct
  type value = { unique_name : string; has_linkage : bool }
  type map = (string, value) Hashtbl.t
  type t = { map : map; parent : t option }

  let init = { map = Hashtbl.create 32; parent = None }
  let push env = { map = Hashtbl.create 32; parent = Some env }
  let get env name = Hashtbl.find_opt env.map name

  let rec get_rec env name =
    match get env name with
    | Some unique -> Some unique
    | None -> (
        match env.parent with Some env -> get_rec env name | None -> None)

  let put env name ?(has_linkage : bool = false) (unique_name : string) =
    Hashtbl.add env.map name { unique_name; has_linkage }
end

type t = { env : Env.t; counter : int ref }

let init = { env = Env.init; counter = ref 0 }
let push_scope { env; counter } = { env = Env.push env; counter }

let rec resolve program =
  let state = init in
  resolve_program program state

and resolve_program (program : Ast.t) (state : t) : Ast.t =
  List.map (fun decl -> resolve_file_scope_declaration decl state) program

and resolve_file_scope_declaration decl state =
  match decl with
  | Ast.Function func ->
      Ast.Function (resolve_file_scope_function_declaration func state)
  | Ast.Variable var_decl ->
      Ast.Variable (resolve_file_scope_variable_declaration var_decl state)

and resolve_file_scope_function_declaration { name; params; body; storage }
    state =
  (match Env.get state.env name with
  | Some { unique_name = _; has_linkage = false } ->
      failwith "duplicate function declaration"
  | Some _ -> ()
  | None -> Env.put state.env name name ~has_linkage:true);

  let state = push_scope state in
  let params =
    List.map
      (fun param ->
        match Env.get state.env param with
        | Some _ -> failwith "can't have two parameters with the same name"
        | None ->
            let unique_param_name = make_unique name state.counter in
            Env.put state.env param unique_param_name;
            unique_param_name)
      params
  in
  let body =
    Option.map (fun body -> resolve_block body state ~new_scope:false) body
  in

  { name; params; body; storage }

and resolve_file_scope_variable_declaration decl state =
  Env.put state.env decl.name decl.name ~has_linkage:true;
  decl

and resolve_local_declaration decl state =
  match decl with
  | Ast.Function func ->
      Ast.Function (resolve_local_function_declaration func state)
  | Ast.Variable var_decl ->
      Ast.Variable (resolve_local_variable_declaration var_decl state)

and resolve_local_function_declaration { name; params; body; storage } state =
  if Option.is_some body then
    failwith "local function definition is not allowed";

  if storage = Some Static then
    failwith [%string "invalid storage class 'static' for function '%{name}'"];

  Env.put state.env name name;

  { name; params; body; storage }

and resolve_local_variable_declaration { name; init; storage } state =
  Log.debug (fun m -> m "resolving %s" name);

  (match Env.get state.env name with
  | Some { unique_name = _; has_linkage }
    when not (has_linkage && storage = Some Extern) ->
      failwith "conflicting local declarations"
  | _ -> ());

  match storage with
  | Some Extern ->
      Env.put state.env name name ~has_linkage:true;
      { name; init; storage }
  | _ ->
      let unique = make_unique name state.counter in
      Env.put state.env name unique;
      let init = Option.map (fun expr -> resolve_expression expr state) init in
      { name = unique; init; storage }

and resolve_block ?(new_scope : bool = true) (blk : Ast.block) (state : t) :
    Ast.block =
  let blk_map = if new_scope then push_scope state else state in
  List.map (fun item -> resolve_block_item item blk_map) blk

and resolve_block_item item state =
  match item with
  | Ast.Decl decl -> Ast.Decl (resolve_local_declaration decl state)
  | Ast.Stmt stmt -> Ast.Stmt (resolve_statement stmt state)

and resolve_statement stmt state =
  match stmt with
  | Ast.Return value -> Ast.Return (resolve_expression value state)
  | Ast.Expr value -> Ast.Expr (resolve_expression value state)
  | Ast.Null -> Ast.Null
  | Ast.If { cond; then'; else' } ->
      Ast.If
        {
          cond = resolve_expression cond state;
          then' = resolve_statement then' state;
          else' = Option.map (fun stmt -> resolve_statement stmt state) else';
        }
  | Ast.Goto target -> Ast.Goto target
  | Ast.LabeledStmt { label; stmt } ->
      Ast.LabeledStmt { stmt = resolve_statement stmt state; label }
  | Ast.Compound blk -> Ast.Compound (resolve_block blk state)
  | Ast.While { cond; body; label } ->
      Ast.While
        {
          cond = resolve_expression cond state;
          body = resolve_statement body state;
          label;
        }
  | Ast.DoWhile { body; cond; label } ->
      Ast.DoWhile
        {
          body = resolve_statement body state;
          cond = resolve_expression cond state;
          label;
        }
  | Ast.For { init; cond; post; body; label } ->
      let state = push_scope state in
      let init =
        match init with
        | Ast.VarDecl decl ->
            Ast.VarDecl (resolve_local_variable_declaration decl state)
        | Ast.Expr (Some e) -> Ast.Expr (Some (resolve_expression e state))
        | Ast.Expr None -> Ast.Expr None
      in
      Ast.For
        {
          init;
          cond = Option.map (fun expr -> resolve_expression expr state) cond;
          post = Option.map (fun expr -> resolve_expression expr state) post;
          body = resolve_statement body state;
          label;
        }
  | Ast.Switch { expr; stmt; label; cases; default } ->
      Ast.Switch
        {
          expr = resolve_expression expr state;
          stmt = resolve_statement stmt state;
          label;
          cases;
          default;
        }
  | Ast.Case { expr; stmt; label } ->
      Ast.Case
        {
          expr = resolve_expression expr state;
          stmt = Option.map (fun stmt -> resolve_statement stmt state) stmt;
          label;
        }
  | Ast.Default { stmt; label } ->
      Ast.Default
        {
          stmt = Option.map (fun stmt -> resolve_statement stmt state) stmt;
          label;
        }
  | _ -> stmt

and resolve_expression expr state =
  match expr with
  | Ast.IntLit value -> Ast.IntLit value
  | Ast.LeftUnary
      {
        operator = (Token.PlusPlus | Token.MinusMinus) as operator;
        rhs = Ast.Var _ as rhs;
      } ->
      Ast.LeftUnary { operator; rhs = resolve_expression rhs state }
  | Ast.LeftUnary { operator = Token.PlusPlus | Token.MinusMinus; _ } ->
      failwith "invalid unary lvalue"
  | Ast.LeftUnary { operator; rhs } ->
      Ast.LeftUnary { operator; rhs = resolve_expression rhs state }
  | Ast.RightUnary
      {
        operator = (Token.PlusPlus | Token.MinusMinus) as operator;
        lhs = (Ast.Var _ | Ast.LeftUnary _) as lhs;
      } ->
      Ast.RightUnary { operator; lhs = resolve_expression lhs state }
  | Ast.RightUnary { operator = Token.PlusPlus | Token.MinusMinus; lhs = _ } ->
      failwith "invalid unary lvalue"
  | Ast.RightUnary { operator; lhs } ->
      Ast.RightUnary { operator; lhs = resolve_expression lhs state }
  | Ast.Binary { operator; lhs; rhs } ->
      Ast.Binary
        {
          operator;
          lhs = resolve_expression lhs state;
          rhs = resolve_expression rhs state;
        }
  | Ast.Assignment { operator; lhs = Ast.Var name; rhs } ->
      let unique_name =
        match Env.get_rec state.env name with
        | Some { unique_name; has_linkage = _ } -> unique_name
        | None -> failwith (Printf.sprintf "variable %s is not defined" name)
      in
      Ast.Assignment
        {
          operator;
          lhs = Ast.Var unique_name;
          rhs = resolve_expression rhs state;
        }
  | Ast.Assignment _ -> failwith "invalid lvalue"
  | Ast.Var name -> (
      match Env.get_rec state.env name with
      | Some { unique_name; has_linkage = _ } -> Ast.Var unique_name
      | None ->
          failwith (Printf.sprintf "variable with name %s is not defined" name))
  | Ast.Conditional { cond; true'; false' } ->
      Ast.Conditional
        {
          cond = resolve_expression cond state;
          true' = resolve_expression true' state;
          false' = resolve_expression false' state;
        }
  | Ast.FunctionCall { name; args } -> (
      match Env.get_rec state.env name with
      | Some { unique_name; has_linkage = _ } ->
          let args = List.map (fun arg -> resolve_expression arg state) args in
          Ast.FunctionCall { name = unique_name; args }
      | None -> failwith [%string "undeclared function %{name}"])

and make_unique name counter =
  let name = Printf.sprintf "var.%s.%d" name !counter in
  counter := !counter + 1;
  name
