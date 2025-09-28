type t = subroutine list
and subroutine = { name : string; instructions : instruction list }

and instruction =
  | Mov of { src : operand; dst : operand }
  | Unary of { operator : unop; operand : operand }
  | Binary of { operator : binop; src1 : operand; src2 : operand }
  | Cmp of { src1 : operand; src2 : operand }
  | Idiv of operand
  | Cdq
  | Jmp of string
  | JmpCC of { cond : cond_code; target : string }
  | SetCC of { cond : cond_code; dst : operand }
  | Label of string
  | AllocateStack of int
  | DeallocateStack of int
  | Push of operand
  | Call of string
  | Ret

and cond_code = E | NE | G | GE | L | LE
and unop = Neg | Not
and binop = Add | Sub | Imul | And | Or | Xor | Shl | Sal | Shr | Sar

(* operand *)
and operand = Imm of int32 | Reg of register | Pseudo of string | Stack of int
and register = { base : regbase; part : regpart }
and regbase = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11
and regpart = LB | HB | W | DW | QW [@@deriving show, eq]
