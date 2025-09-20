type t

val init : Lexer.t -> t

val parse_from_lexer : Lexer.t -> Ast.program

val program : t -> Ast.program

val show : t -> string
val pp : Format.formatter -> t -> unit
