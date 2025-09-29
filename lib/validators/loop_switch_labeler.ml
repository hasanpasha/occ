let src =
  Logs.Src.create "occ.loop_switch_labeler"
    ~doc:"logs occ's loop switch labeler events"

module Log = (val Logs.src_log src : Logs.LOG)

type kind = Loop | Switch

type t = {
  loop_label : string option;
  switch_label : string option;
  kind : kind option;
  counter : int ref;
  cases : (Ast.expression * Ast.ident) list option ref;
  default : Ast.ident option ref;
}

let init =
  {
    loop_label = None;
    switch_label = None;
    kind = None;
    counter = ref 0;
    cases = ref None;
    default = ref None;
  }

let rec resolve program =
  let state = init in
  List.map (fun decl -> resolve_declaration decl state) program

and resolve_declaration decl state =
  match decl with
  | Ast.Function { name; params; body; storage } ->
      Ast.Function
        {
          name;
          params;
          body = Option.map (fun body -> resolve_block body state) body;
          storage;
        }
  | _ -> decl

and resolve_block (blk : Ast.block) state =
  List.map (fun item -> resolve_block_item item state) blk

and resolve_block_item (item : Ast.block_item) state =
  match item with
  | Ast.Decl _ -> item
  | Ast.Stmt stmt -> Ast.Stmt (resolve_statement stmt state)

and resolve_statement stmt state =
  let copy_state_with_loop_lbl
      { loop_label = _; switch_label; kind = _; counter; cases; default } lbl
      kind =
    {
      loop_label = Some lbl;
      switch_label;
      kind = Some kind;
      counter;
      cases;
      default;
    }
  in

  match stmt with
  | Ast.If { cond; then'; else' } ->
      Ast.If
        {
          cond;
          then' = resolve_statement then' state;
          else' = Option.map (fun stmt -> resolve_statement stmt state) else';
        }
  | Ast.LabeledStmt { label; stmt } ->
      Ast.LabeledStmt { stmt = resolve_statement stmt state; label }
  | Ast.Compound blk -> Ast.Compound (resolve_block blk state)
  | Ast.While { cond; body; label = _ } ->
      let unique = make_unique "while" state.counter in
      let state' = copy_state_with_loop_lbl state unique Loop in
      Ast.While { cond; body = resolve_statement body state'; label = unique }
  | Ast.DoWhile { body; cond; label = _ } ->
      let unique = make_unique "do_while" state.counter in
      let state' = copy_state_with_loop_lbl state unique Loop in
      Ast.DoWhile { body = resolve_statement body state'; cond; label = unique }
  | Ast.For { init; cond; post; body; label = _ } ->
      let unique = make_unique "for" state.counter in
      let state' = copy_state_with_loop_lbl state unique Loop in
      Ast.For
        {
          init;
          cond;
          post;
          body = resolve_statement body state';
          label = unique;
        }
  | Ast.Break _ -> (
      Log.debug (fun m -> m "resolving break stmt");
      match state.kind with
      | Some Loop -> Ast.Break (Option.get state.loop_label)
      | Some Switch -> Ast.Break (Option.get state.switch_label)
      | _ ->
          failwith "break statement must be inside a loop or switch statement")
  | Ast.Continue _ -> (
      Log.debug (fun m -> m "resolving continue stmt");
      match state.loop_label with
      | Some name -> Ast.Continue name
      | None ->
          failwith
            (Printf.sprintf "continue statement must be inside a loop statement")
      )
  | Ast.Switch { expr; stmt; label = _; cases = _; default = _ } ->
      let unique = make_unique "switch" state.counter in
      let state =
        {
          loop_label = state.loop_label;
          switch_label = Some unique;
          kind = Some Switch;
          counter = state.counter;
          cases = ref (Some []);
          default = ref None;
        }
      in

      let stmt = resolve_statement stmt state in

      let cases = List.rev (!(state.cases) |> Option.get) in

      List.iter
        (fun (expr, label) ->
          Log.debug (fun m -> m "%s: %s" label (Ast.show_expression expr)))
        cases;

      let check_no_duplicate cases =
        let rec aux = function
          | [] | [ _ ] -> ()
          | (x_expr, _) :: xs ->
              if List.exists (fun (y_expr, _) -> x_expr = y_expr) xs then
                failwith
                  ("multiple case with same expr: " ^ Ast.show_expression x_expr)
              else aux xs
        in
        aux cases
      in

      check_no_duplicate cases;

      Ast.Switch
        { expr; stmt; label = unique; cases; default = !(state.default) }
  | Ast.Case { expr = Ast.IntLit value; stmt; label = _ } ->
      let switch_label =
        match state.switch_label with
        | Some label -> label
        | None ->
            failwith "case statement must be inside a switch statement body"
      in
      let label = make_switch_case switch_label state.counter in
      let expr = Ast.IntLit value in

      let current_cases = !(state.cases) |> Option.get in
      let new_cases = (expr, label) :: current_cases in
      state.cases := Some new_cases;

      Ast.Case
        {
          expr;
          stmt = Option.map (fun stmt -> resolve_statement stmt state) stmt;
          label;
        }
  | Ast.Case _ -> failwith "case expr must be a constant"
  | Ast.Default { stmt; label = _ } ->
      let switch_label =
        match state.switch_label with
        | Some label -> label
        | None ->
            failwith "default statement must be inside a switch statement body"
      in
      let label = make_switch_case switch_label state.counter in

      (match !(state.default) with
      | Some _ ->
          failwith
            "switch statement can't have more than two default statements"
      | None -> state.default := Some label);

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

and make_switch_case switch_label counter =
  let name = Printf.sprintf "%s.%d" switch_label !counter in
  counter := !counter + 1;
  name
