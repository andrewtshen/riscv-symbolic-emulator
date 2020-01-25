.section .stack,"ax",%progbits

.global _stack
.type _stack, %function

_stack:
    la sp, estack
    addi sp, sp, -32
    sd s0, 24(sp)
    addi s0, sp,32
    li a5, 80
    sw a5, -20(s0)
    li a5, 0
    mv a0, a5
    ld s0, 24(sp)
    addi sp, sp, 32
    mret 
_fallthrough:
    j _fallthrough
.size _stack, .-_stack

.type estack, %object
.align 8
estack:
    .fill 64
.size estack, .-estack
