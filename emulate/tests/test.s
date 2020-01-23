.section .sub,"ax",%progbits

.global _sub
.type _sub, %function

_sub:
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
.size _sub, .-_sub

.type estack, %object
.align 16
estack:
    .fill 256
.size estack, .-estack