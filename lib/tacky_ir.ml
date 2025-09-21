type t = function' list
and function' = { name : string; instructions : instruction list }

and instruction =
  | Return of value
  | Unary of { operator : unop; src : value; dst : value }
  | Binary of { operator : binop; src1 : value; src2 : value; dst : value }
  | Copy of { src : value; dst : value }
  | Jump of string
  | JumpIfZero of { cond : value; target : string }
  | JumpIfNotZero of { cond : value; target : string }
  | Label of string

and value = Constant of int | Variable of string
and unop = Negate | Complement | Not

and binop =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | BinaryAnd
  | BinaryOr
  | ExclusiveOr
  | ShiftLeft
  | ShiftRight
  | Equals
  | NotEquals
  | Less
  | LessEquals
  | Greater
  | GreaterEquals
[@@deriving show, eq]
