let src =
  Logs.Src.create "occ.label_resolver" ~doc:"logs occ's label resolver events"

module Log = (val Logs.src_log src : Logs.LOG)

type t = { map : (string, string) Hashtbl.t; counter : int ref }

let init = { map = Hashtbl.create 32; counter = ref 0 }
let copy { map; counter } = { map = Hashtbl.copy map; counter }

let rec resolve program =
  let state = init in
  resolve_program program state

and resolve_program (program : Vir.t) (state : t) : Vir.t =
  List.map (fun decl -> resolve_declaration decl state) program

and resolve_declaration decl state =
  match decl with
  | Vir.Function { name; body } ->
      let this_state = copy state in
      let body = gather_block body this_state in
      let body = resolve_block body this_state in
      Vir.Function { name; body }
  | _ -> decl

and gather_block blk state =
  List.map (fun item -> gather_block_item item state) blk

and gather_block_item item state =
  match item with
  | Vir.Decl decl -> Vir.Decl (resolve_declaration decl state)
  | Vir.Stmt stmt -> Vir.Stmt (gather_statement stmt state)

and gather_statement stmt state =
  match stmt with
  | Vir.If { cond; then'; else' } ->
      Vir.If
        {
          cond;
          then' = gather_statement then' state;
          else' = Option.map (fun stmt -> gather_statement stmt state) else';
        }
  | Vir.LabeledStmt { stmt; label } ->
      Log.debug (fun m -> m "resolving label %s" label);
      (match Hashtbl.find_opt state.map label with
      | Some _ -> failwith (Printf.sprintf "label %s is already defined" label)
      | None -> ());
      let unique_name = make_unique label state.counter in
      Log.debug (fun m -> m "putting label %s as %s" label unique_name);
      Hashtbl.add state.map label unique_name;
      Vir.LabeledStmt
        { stmt = gather_statement stmt state; label = unique_name }
  | Vir.Compound blk -> Vir.Compound (gather_block blk state)
  | Vir.While { cond; body; label } ->
      Vir.While { cond; body = gather_statement body state; label }
  | Vir.DoWhile { body; cond; label } ->
      Vir.DoWhile { body = gather_statement body state; cond; label }
  | Vir.For { init; cond; post; body; label } ->
      Vir.For
        {
          init =
            (match init with
            | Vir.Decl decl -> Vir.Decl (resolve_declaration decl state)
            | _ -> init);
          cond;
          post;
          body = gather_statement body state;
          label;
        }
  | Vir.Switch { expr; stmt; label; cases; default } ->
      Vir.Switch
        { expr; stmt = gather_statement stmt state; label; cases; default }
  | Vir.Case { expr; stmt; label } ->
      Vir.Case
        {
          expr;
          stmt = Option.map (fun stmt -> gather_statement stmt state) stmt;
          label;
        }
  | Vir.Default { stmt; label } ->
      Vir.Default
        {
          stmt = Option.map (fun stmt -> gather_statement stmt state) stmt;
          label;
        }
  | _ -> stmt

and resolve_block (blk : Vir.block) state =
  let blk_map = copy state in
  List.map (fun item -> resolve_block_item item blk_map) blk

and resolve_block_item (item : Vir.block_item) state =
  match item with
  | Vir.Decl decl -> Vir.Decl (resolve_declaration decl state)
  | Vir.Stmt stmt -> Vir.Stmt (resolve_statement stmt state)

and resolve_statement stmt state =
  match stmt with
  | Vir.If { cond; then'; else' } ->
      Vir.If
        {
          cond;
          then' = resolve_statement then' state;
          else' = Option.map (fun stmt -> resolve_statement stmt state) else';
        }
  | Vir.Goto target -> (
      match Hashtbl.find_opt state.map target with
      | Some unique_name -> Vir.Goto unique_name
      | None ->
          failwith (Printf.sprintf "label with name %s is not defined" target))
  | Vir.LabeledStmt { label; stmt } ->
      Vir.LabeledStmt { stmt = resolve_statement stmt state; label }
  | Vir.Compound blk -> Vir.Compound (resolve_block blk state)
  | Vir.While { cond; body; label } ->
      Vir.While { cond; body = resolve_statement body state; label }
  | Vir.DoWhile { body; cond; label } ->
      Vir.DoWhile { body = resolve_statement body state; cond; label }
  | Vir.For { init; cond; post; body; label } ->
      Vir.For
        {
          init =
            (match init with
            | Vir.Decl decl -> Vir.Decl (resolve_declaration decl state)
            | _ -> init);
          cond;
          post;
          body = resolve_statement body state;
          label;
        }
  | Vir.Switch { expr; stmt; label; cases; default } ->
      Vir.Switch
        { expr; stmt = resolve_statement stmt state; label; cases; default }
  | Vir.Case { expr; stmt; label } ->
      Vir.Case
        {
          expr;
          stmt = Option.map (fun stmt -> resolve_statement stmt state) stmt;
          label;
        }
  | Vir.Default { stmt; label } ->
      Vir.Default
        {
          stmt = Option.map (fun stmt -> resolve_statement stmt state) stmt;
          label;
        }
  | _ -> stmt

and make_unique name counter =
  let name = Printf.sprintf "lbl.%s.%d" name !counter in
  counter := !counter + 1;
  name
