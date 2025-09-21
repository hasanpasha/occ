type t

val init : Lexer.t -> t

val parse_from_lexer : Lexer.t -> Ast.t

val program : t -> Ast.t

val show : t -> string
val pp : Format.formatter -> t -> unit
