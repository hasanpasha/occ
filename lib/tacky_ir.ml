type t = top_level list

and top_level =
  | Function of {
      name : string;
      global : bool;
      params : string list;
      body : instruction list;
    }
  | StaticVariable of { name : string; global : bool; init : int }

and instruction =
  | Return of value
  | Unary of { operator : unop; src : value; dst : value }
  | Binary of { operator : binop; src1 : value; src2 : value; dst : value }
  | Copy of { src : value; dst : value }
  | Jump of string
  | JumpIfZero of { cond : value; target : string }
  | JumpIfNotZero of { cond : value; target : string }
  | Label of string
  | FunCall of { fun_name : string; args : value list; dst : value }

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
