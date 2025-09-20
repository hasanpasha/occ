let src = Logs.Src.create "occ.loop_switch_labeler" ~doc:"logs occ's loop switch labeler events"
module Log = (val Logs.src_log src : Logs.LOG)

type t = 
{
  loop_label: string option;
  switch_label: string option;
  counter: int ref;
  cases: (((Vir.expression * Vir.ident) list) option) ref;
  default: ((Vir.ident) option) ref;
}

let init = 
  { 
    loop_label = None; 
    switch_label = None; 
    counter = ref 0;
    cases = ref None;
    default = ref None
  }

let rec resolve program =
  List.map resolve_declaration program

and resolve_declaration decl =
match decl with
| Vir.Function { name; body } ->
  let state = init in
  Vir.Function { name; body = resolve_block body state }
| _ -> decl

and resolve_block (blk: Vir.block) state = 
  List.map (fun item -> resolve_block_item item state) blk 

and resolve_block_item (item: Vir.block_item) state =
match item with
| Vir.Decl _ -> item
| Vir.Stmt stmt -> Vir.Stmt (resolve_statement stmt state)

and resolve_statement stmt state =
let copy_state_with_loop_lbl { loop_label = _; switch_label; counter; cases; default } lbl =  
  { loop_label = Some lbl; switch_label; counter; cases; default }
in

match stmt with
| Vir.If { cond; then'; else' } -> 
  Vir.If { 
    cond; 
    then' = resolve_statement then' state; 
    else' = Option.map (fun stmt -> resolve_statement stmt state) else' 
  }

| Vir.LabeledStmt { label; stmt } -> 
  Vir.LabeledStmt { stmt = resolve_statement stmt state; label }

| Vir.Compound blk -> Vir.Compound (resolve_block blk state)

| Vir.While { cond; body; label = _ } -> 
  let unique = make_unique "while" state.counter in
  let state' = copy_state_with_loop_lbl state unique in
  Vir.While {
    cond;
    body = resolve_statement body state';
    label = unique
  }

| Vir.DoWhile { body; cond; label = _ } -> 
  let unique = make_unique "do_while" state.counter in
  let state' = copy_state_with_loop_lbl state unique in
  Vir.DoWhile {
    body = resolve_statement body state';
    cond; 
    label = unique
  }

| Vir.For { init; cond; post; body; label = _ } -> 
  let unique = make_unique "for" state.counter in
  let state' = copy_state_with_loop_lbl state unique in
  Vir.For {
    init;
    cond; post;
    body = resolve_statement body state';
    label = unique
  }

| Vir.Break _ -> 
  Log.debug (fun m -> m "resolving break stmt");
  (match state.switch_label with 
  | Some name -> Vir.Break name
  | None -> match state.loop_label with
    | Some name -> Vir.Break name
    | None -> failwith (Printf.sprintf "break statement must be inside a loop or switch statement"))

| Vir.Continue _ -> 
  Log.debug (fun m -> m "resolving continue stmt");
  (match state.loop_label with
  | Some name -> Vir.Continue name
  | None -> failwith (Printf.sprintf "continue statement must be inside a loop statement"))

| Vir.Switch { expr; stmt; label = _; cases = _; default = _ } ->
  let unique = make_unique "switch" state.counter in
  let state = { 
    loop_label = state.loop_label; 
    switch_label = Some unique; 
    counter = state.counter;
    cases = ref (Some []);
    default = ref None;
  } in
  
  let stmt = resolve_statement stmt state in

  let cases = List.rev (!(state.cases) |> Option.get) in

  List.iter (fun (expr, label) -> Log.debug (fun m -> m "%s: %s" label (Vir.show_expression expr))) cases;

  let check_no_duplicate cases =
    let rec aux = function
      | [] | [_] -> ()
      | (x_expr, _) :: xs ->
          if List.exists (fun (y_expr, _) -> x_expr = y_expr) xs then
            failwith ("multiple case with same expr: " ^ (Vir.show_expression x_expr))
          else
            aux xs
    in
  aux cases in

  check_no_duplicate cases;

  Vir.Switch { 
    expr; 
    stmt;
    label = unique;
    cases; 
    default = !(state.default)
  }
  
| Vir.Case { expr = Vir.IntLit value; stmt; label = _ } ->
  let switch_label = match state.switch_label with
  | Some label -> label
  | None -> failwith "case statement must be inside a switch statement body"
  in
  let label = make_switch_case switch_label state.counter in
  let expr = Vir.IntLit value in

  let current_cases = !(state.cases) |> Option.get in
  let new_cases = (expr, label) :: current_cases in
  state.cases := Some new_cases;
  
  Vir.Case {
    expr;
    stmt = Option.map (fun stmt -> resolve_statement stmt state) stmt;
    label
  }
| Vir.Case _ -> failwith "case expr must be a constant"

| Vir.Default { stmt; label = _} ->
  let switch_label = match state.switch_label with
  | Some label -> label
  | None -> failwith "default statement must be inside a switch statement body"
  in
  let label = make_switch_case switch_label state.counter in

  (match !(state.default) with 
  | Some _ -> failwith "switch statement can't have more than two default statements"
  | None -> state.default := Some label);

  Vir.Default {
    stmt = Option.map (fun stmt -> resolve_statement stmt state) stmt;
    label
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