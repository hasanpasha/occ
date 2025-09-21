type t

val init : string -> t
val next_token : t -> t * Token.t option
val lex : t -> Token.t Seq.t
val pp : Format.formatter -> t -> unit
val show : t -> string
