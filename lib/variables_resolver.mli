type t

val init : t

val resolve : Ast.program -> Vir.program

val resolve_program : Ast.program -> t -> Vir.program
