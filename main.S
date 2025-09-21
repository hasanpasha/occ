.section .text
.global main
main:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
movl $0, %r10d
cmpl $1, %r10d
je .Lfalse.1
movl $0, %r10d
cmpl $2, %r10d
je .Lfalse.1
movl $1, -4(%rbp)
jmp .Lend.2
.Lfalse.1:
movl $0, -4(%rbp)
.Lend.2:
movl $0, %r10d
cmpl -4(%rbp), %r10d
je .Lend.3
movl $2, %eax
movq %rbp, %rsp
popq %rbp
ret
.Lend.3:
movl $0, %r10d
cmpl $1, %r10d
je .Lelse.5
movl $2, -8(%rbp)
jmp .Lend.6
.Lelse.5:
movl $3, -8(%rbp)
.Lend.6:
movl -8(%rbp), %r10d
movl %r10d, -12(%rbp)
movl $2, -16(%rbp)
negl -16(%rbp)
movl -16(%rbp), %eax
movq %rbp, %rsp
popq %rbp
ret
movq %rbp, %rsp
popq %rbp
ret
.section .note.GNU-stack,"",@progbits
