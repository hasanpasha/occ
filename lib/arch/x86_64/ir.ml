type t = subroutine list
and subroutine = { name : string; instructions : instruction list }

and instruction =
  | Mov of { src : operand; dst : operand }
  | Unary of { operator : unop; operand : operand }
  | Binary of { operator : binop; src1 : operand; src2 : operand }
  | Cmp of { src1 : operand; src2 : operand }
  | Idiv of operand
  | Cdq
  | AllocateStack of int
  | Ret
  | Jmp of string
  | JmpCC of jump_cc
  | SetCC of set_cc
  | Label of string

and jump_cc = { cond : cond_code; target : string }
and set_cc = { cond : cond_code; dst : operand }
and cond_code = E | NE | G | GE | L | LE
and unop = Neg | Not
and binop = Add | Sub | Imul | And | Or | Xor | Shl | Sal | Shr | Sar

(* operand *)
and operand = Imm of int32 | Reg of register | Pseudo of string | Stack of int
and register = { base : regbase; part : regpart }
and regbase = AX | DX | R10 | R11 | CX
and regpart = LB | HB | W | DW | QW [@@deriving show, eq]
