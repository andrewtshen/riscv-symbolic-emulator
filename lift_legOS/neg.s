.section .startup,"ax",%progbits

.global _startup
.type _startup, %function
_startup:
    la sp, _estack
    call _neg
_junk:
    j _junk
.size _startup, .-_startup

.type _neg, %function
_neg:
    li a5, 10
    sw a5, -20(s0)
    lw a5, -20(s0)
    blez a5, 1007c
    li a5, 1
    j 10094
    lw a5, -20(s0)
    a5, -1
    sw a5,-24(s0)
    j 10094
    sw zero,-24(s0)
    ret
.size _init, .-_init

; ^ about to run this neg function i got from riscv compiled down from
; neg/riscv/test.c/s