let src =
  Logs.Src.create "occ.variables_resolver"
    ~doc:"logs occ's variables resolver events"

module Log = (val Logs.src_log src : Logs.LOG)

module Env = struct
  type map = (string, string) Hashtbl.t
  type t = { map : map; parent : t option }

  let init = { map = Hashtbl.create 32; parent = None }
  let push env = { map = Hashtbl.create 32; parent = Some env }
  let get env name = Hashtbl.find_opt env.map name

  let rec get_rec env name =
    match get env name with
    | Some unique -> Some unique
    | None -> (
        match env.parent with Some env -> get_rec env name | None -> None)

  let put env name unique = Hashtbl.add env.map name unique
end

type t = { env : Env.t; counter : int ref }

let init = { env = Env.init; counter = ref 0 }
let copy { env; counter } = { env = Env.push env; counter }

let rec resolve program =
  let state = init in
  resolve_program program state

and resolve_program (program : Ast.t) (state : t) : Ast.t =
  List.map (fun decl -> resolve_declaration decl state) program

and resolve_declaration decl state =
  match decl with
  | Ast.Function { name; body } ->
      Ast.Function { name; body = resolve_block body state }
  | Ast.Variable { name; init } -> (
      Log.debug (fun m -> m "resolving %s" name);
      match Env.get state.env name with
      | Some _ ->
          failwith
            (Printf.sprintf "variable with name %s is already defined" name)
      | None ->
          ();
          let unique_name = make_unique name state.counter in
          Log.debug (fun m ->
              m "putting %s as %s in variables map" name unique_name);
          Env.put state.env name unique_name;
          Ast.Variable
            {
              name = unique_name;
              init = Option.map (fun expr -> resolve_expression expr state) init;
            })

and resolve_block blk state =
  let blk_map = copy state in
  List.map (fun item -> resolve_block_item item blk_map) blk

and resolve_block_item item state =
  match item with
  | Ast.Decl decl -> Ast.Decl (resolve_declaration decl state)
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
      let state = copy state in
      let init =
        match init with
        | Ast.Decl decl -> Ast.Decl (resolve_declaration decl state)
        | Ast.Expr (Some expr) ->
            Ast.Expr (Some (resolve_expression expr state))
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
        | Some v -> v
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
      | Some unique_name -> Ast.Var unique_name
      | None ->
          failwith (Printf.sprintf "variable with name %s is not defined" name))
  | Ast.Conditional { cond; true'; false' } ->
      Ast.Conditional
        {
          cond = resolve_expression cond state;
          true' = resolve_expression true' state;
          false' = resolve_expression false' state;
        }

and make_unique name counter =
  let name = Printf.sprintf "var.%s.%d" name !counter in
  counter := !counter + 1;
  name
