.section .text
.global main
main:
pushq %rbp
movq %rsp, %rbp
subq $8, %rsp
movl $3, %r10d
cmpl $1, %r10d
movl $0, -4(%rbp)
sete -4(%rbp)
cmpl $2, -4(%rbp)
movl $0, -8(%rbp)
setne -8(%rbp)
movl -8(%rbp), %eax
movq %rbp, %rsp
popq %rbp
ret
movl $0, %eax
movq %rbp, %rsp
popq %rbp
ret
.section .note.GNU-stack,"",@progbits
