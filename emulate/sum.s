.section .sum,"ax",%progbits

.global _sum
.type _sum, %function
_sum:
    li a5, 2
    li a6, 3
    auipc a4, 4
    bltu a5, a6, _add
    li a7, 8
    mret
_add:
    li a7, 2
    add a6, a5, a7 # adding registers together
    mret
_fallthrough:
    j _fallthrough
.size _sum, .-_sum
