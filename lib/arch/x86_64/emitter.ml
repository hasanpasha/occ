open Ir

let rec emit (air : Ir.t) : string =
  let subroutines =
    List.filter_map
      (fun top ->
        match top with Subroutine subroutine -> Some subroutine | _ -> None)
      air
  in

  let variables =
    List.filter_map
      (fun top ->
        match top with StaticVariable variable -> Some variable | _ -> None)
      air
  in

  ".section .data\n"
  ^ String.concat "\n" (List.map variable variables)
  ^ ".section .text\n"
  ^ String.concat "\n" (List.map subroutine subroutines)
  ^ "\n.section .note.GNU-stack,\"\",@progbits\n"

and variable { name; global; init } =
  (if global then [%string ".global %{name}\n"] else "")
  ^ {%string|.align 4
%{name}:
.long %{string_of_int init}
|}

and subroutine { name; global; instructions } =
  let body = String.concat "\n" (List.map instruction instructions) in
  (if global then [%string ".global %{name}\n"] else "")
  ^ {%string|%{name}:
pushq %rbp
movq %rsp, %rbp
%{body}|}

and instruction = function
  | Ret -> {%string|movq %rbp, %rsp
popq %rbp 
ret|}
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
  | Jmp target -> [%string "jmp %{fmt_label target}"]
  | Label lbl -> [%string "%{fmt_label lbl}:"]
  | JmpCC { cond; target } ->
      [%string "j%{string_of_cond_code cond} %{fmt_label target}"]
  | SetCC { cond; dst } ->
      [%string "set%{string_of_cond_code cond} %{operand dst}"]
  | DeallocateStack size -> [%string "addq $%{Int.to_string size}, %rsp"]
  | Push op -> [%string "pushq %{operand op}"]
  | Call name -> [%string "call %{name}"]
(* | _ -> "" *)

and string_of_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

and fmt_label lbl = ".L" ^ lbl

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
        | DI, LB -> "dil"
        | DI, HB -> failwith "di doesn't have high byte part"
        | DI, W -> "di"
        | DI, DW -> "edi"
        | DI, QW -> "rdi"
        | SI, LB -> "sil"
        | SI, HB -> failwith "si doesn't have hight byte part"
        | SI, W -> "si"
        | SI, DW -> "esi"
        | SI, QW -> "rsi"
        | DX, LB -> "dl"
        | DX, HB -> "dh"
        | DX, W -> "dx"
        | DX, DW -> "edx"
        | DX, QW -> "rdx"
        | R8, LB -> "r8b"
        | R8, HB -> failwith "r8 doesn't have hight byte part"
        | R8, W -> "r8w"
        | R8, DW -> "r8d"
        | R8, QW -> "r8"
        | R9, LB -> "r9b"
        | R9, HB -> failwith "r9 doesn't have hight byte part"
        | R9, W -> "r9w"
        | R9, DW -> "r9d"
        | R9, QW -> "r9"
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
  | Data name -> [%string "%{name}(%rip)"]
