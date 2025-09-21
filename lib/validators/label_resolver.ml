let src =
  Logs.Src.create "occ.label_resolver" ~doc:"logs occ's label resolver events"

module Log = (val Logs.src_log src : Logs.LOG)

type t = { map : (string, string) Hashtbl.t; counter : int ref }

let init = { map = Hashtbl.create 32; counter = ref 0 }
let copy { map; counter } = { map = Hashtbl.copy map; counter }

let rec resolve program =
  let state = init in
  resolve_program program state

and resolve_program (program : Ast.t) (state : t) : Ast.t =
  List.map (fun decl -> resolve_declaration decl state) program

and resolve_declaration decl state =
  match decl with
  | Ast.Function { name; body } ->
      let this_state = copy state in
      let body = gather_block body this_state in
      let body = resolve_block body this_state in
      Ast.Function { name; body }
  | _ -> decl

and gather_block blk state =
  List.map (fun item -> gather_block_item item state) blk

and gather_block_item item state =
  match item with
  | Ast.Decl decl -> Ast.Decl (resolve_declaration decl state)
  | Ast.Stmt stmt -> Ast.Stmt (gather_statement stmt state)

and gather_statement stmt state =
  match stmt with
  | Ast.If { cond; then'; else' } ->
      Ast.If
        {
          cond;
          then' = gather_statement then' state;
          else' = Option.map (fun stmt -> gather_statement stmt state) else';
        }
  | Ast.LabeledStmt { stmt; label } ->
      Log.debug (fun m -> m "resolving label %s" label);
      (match Hashtbl.find_opt state.map label with
      | Some _ -> failwith (Printf.sprintf "label %s is already defined" label)
      | None -> ());
      let unique_name = make_unique label state.counter in
      Log.debug (fun m -> m "putting label %s as %s" label unique_name);
      Hashtbl.add state.map label unique_name;
      Ast.LabeledStmt
        { stmt = gather_statement stmt state; label = unique_name }
  | Ast.Compound blk -> Ast.Compound (gather_block blk state)
  | Ast.While { cond; body; label } ->
      Ast.While { cond; body = gather_statement body state; label }
  | Ast.DoWhile { body; cond; label } ->
      Ast.DoWhile { body = gather_statement body state; cond; label }
  | Ast.For { init; cond; post; body; label } ->
      Ast.For
        {
          init =
            (match init with
            | Ast.Decl decl -> Ast.Decl (resolve_declaration decl state)
            | _ -> init);
          cond;
          post;
          body = gather_statement body state;
          label;
        }
  | Ast.Switch { expr; stmt; label; cases; default } ->
      Ast.Switch
        { expr; stmt = gather_statement stmt state; label; cases; default }
  | Ast.Case { expr; stmt; label } ->
      Ast.Case
        {
          expr;
          stmt = Option.map (fun stmt -> gather_statement stmt state) stmt;
          label;
        }
  | Ast.Default { stmt; label } ->
      Ast.Default
        {
          stmt = Option.map (fun stmt -> gather_statement stmt state) stmt;
          label;
        }
  | _ -> stmt

and resolve_block (blk : Ast.block) state =
  let blk_map = copy state in
  List.map (fun item -> resolve_block_item item blk_map) blk

and resolve_block_item (item : Ast.block_item) state =
  match item with
  | Ast.Decl decl -> Ast.Decl (resolve_declaration decl state)
  | Ast.Stmt stmt -> Ast.Stmt (resolve_statement stmt state)

and resolve_statement stmt state =
  match stmt with
  | Ast.If { cond; then'; else' } ->
      Ast.If
        {
          cond;
          then' = resolve_statement then' state;
          else' = Option.map (fun stmt -> resolve_statement stmt state) else';
        }
  | Ast.Goto target -> (
      match Hashtbl.find_opt state.map target with
      | Some unique_name -> Ast.Goto unique_name
      | None ->
          failwith (Printf.sprintf "label with name %s is not defined" target))
  | Ast.LabeledStmt { label; stmt } ->
      Ast.LabeledStmt { stmt = resolve_statement stmt state; label }
  | Ast.Compound blk -> Ast.Compound (resolve_block blk state)
  | Ast.While { cond; body; label } ->
      Ast.While { cond; body = resolve_statement body state; label }
  | Ast.DoWhile { body; cond; label } ->
      Ast.DoWhile { body = resolve_statement body state; cond; label }
  | Ast.For { init; cond; post; body; label } ->
      Ast.For
        {
          init =
            (match init with
            | Ast.Decl decl -> Ast.Decl (resolve_declaration decl state)
            | _ -> init);
          cond;
          post;
          body = resolve_statement body state;
          label;
        }
  | Ast.Switch { expr; stmt; label; cases; default } ->
      Ast.Switch
        { expr; stmt = resolve_statement stmt state; label; cases; default }
  | Ast.Case { expr; stmt; label } ->
      Ast.Case
        {
          expr;
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

and make_unique name counter =
  let name = Printf.sprintf "lbl.%s.%d" name !counter in
  counter := !counter + 1;
  name
