type entry = { typ : typ; attr : attribute }
and typ = Int | Function of { arity : int }

and attribute =
  | Fun of { defined : bool; global : bool }
  | Static of { init : initial_value; global : bool }
  | Local

and initial_value = Tentative | Initial of int | NoInitializer

type t = (string, entry) Hashtbl.t

val check : Ast.t -> Ast.t * t
