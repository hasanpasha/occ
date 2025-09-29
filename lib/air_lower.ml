type arch = X86_64

let lower (tir : Tacky_ir.t) (symbols : Validators.Type_checker.t) (arch : arch)
    : Air.t =
  match arch with X86_64 -> Air.X86_64 (Arch.X86_64.Lower.lower tir symbols)
