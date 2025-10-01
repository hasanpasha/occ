type t

val init : Preprocessor.t -> t
val parse : Preprocessor.t -> Ast.t
val program : t -> Ast.t
