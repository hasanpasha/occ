open Ir

let rec emit (air : Ir.t) : string =
  ".section .text"
  ^ String.concat "\n" (List.map subroutine air)
  ^ ".section .note.GNU-stack,\"\",@progbits\n"

and subroutine sub =
  let body = String.concat "\n" (List.map instruction sub.instructions) in
  {%string|
.global %{sub.name}
%{sub.name}:
pushq %rbp
movq %rsp, %rbp
%{body}
|}

and instruction = function
  | Ret -> "movq %rbp, %rsp\npopq %rbp\nret"
  | Mov { src; dst } -> [%string "movl %{operand src}, %{operand dst}"]
  | Unary { operator; operand = oper } ->
      let operator' = match operator with Neg -> "neg" | Not -> "not" in
      [%string "%{operator'}l %{operand oper}"]
  | Binary { operator; src1; src2 } ->
      let operator' =
        match operator with
        | Add -> "add"
        | Sub -> "sub"
        | Imul -> "imul"
        | And -> "and"
        | Or -> "or"
        | Xor -> "xor"
        | Shl -> "shl"
        | Sal -> "sal"
        | Shr -> "shr"
        | Sar -> "sar"
      in
      [%string "%{operator'}l %{operand src1}, %{operand src2}"]
  | Cmp { src1; src2 } -> [%string "cmpl %{operand src1}, %{operand src2}"]
  | Idiv oper -> [%string "idivl %{operand oper}"]
  | Cdq -> "cdq"
  | AllocateStack size -> [%string "subq $%{Int.to_string size}, %rsp"]
  | Jmp target -> [%string "jmp %{label target}"]
  | Label lbl -> [%string "%{label lbl}:"]
  | JmpCC { cond; target } ->
      [%string "j%{string_of_cond_code cond} %{label target}"]
  | SetCC { cond; dst } ->
      [%string "set%{string_of_cond_code cond} %{operand dst}"]

and string_of_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

and label lbl = ".L" ^ lbl

and operand = function
  | Imm value -> [%string "$%{Int32.to_string value}"]
  | Stack index -> [%string "%{Int.to_string index}(%rbp)"]
  | Reg { base; part } ->
      let name =
        match (base, part) with
        | AX, LB -> "al"
        | AX, HB -> "ah"
        | AX, W -> "ax"
        | AX, DW -> "eax"
        | AX, QW -> "rax"
        | DX, LB -> "dl"
        | DX, HB -> "dh"
        | DX, W -> "dx"
        | DX, DW -> "edx"
        | DX, QW -> "rdx"
        | R10, LB -> "r10b"
        | R10, HB -> failwith "r10 doesn't have hight byte part"
        | R10, W -> "r10w"
        | R10, DW -> "r10d"
        | R10, QW -> "r10"
        | R11, LB -> "r11b"
        | R11, HB -> failwith "r11 doesn't have hight byte part"
        | R11, W -> "r11w"
        | R11, DW -> "r11d"
        | R11, QW -> "r11"
        | CX, LB -> "cl"
        | CX, HB -> "ch"
        | CX, W -> "cx"
        | CX, DW -> "ecx"
        | CX, QW -> "rcx"
      in
      "%" ^ name
  | Pseudo _ -> failwith "final air should't containt pseudo operand"
