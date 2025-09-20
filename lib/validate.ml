let validate (program: Ast.program) : Vir.program =
  Validators.Variables_resolver.resolve program
  |> Validators.Label_resolver.resolve
  |> Validators.Loop_switch_labeler.resolve