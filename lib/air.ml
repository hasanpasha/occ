type t = X86_64 of Arch.X86_64.Ir.t [@@deriving show, eq]

let emit (air : t) : string =
  match air with X86_64 air -> Arch.X86_64.Emitter.emit air
