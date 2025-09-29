let validate (program : Ast.t) : Ast.t * Validators.Type_checker.t =
  Validators.Variables_resolver.resolve program
  |> Validators.Label_resolver.resolve |> Validators.Loop_switch_labeler.resolve
  |> Validators.Type_checker.check
