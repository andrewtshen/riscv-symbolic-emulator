.section .sum,"ax",%progbits

.global _sum
.type _sum, %function
_sum:
    li a5, -2
    li a6, 3
    blt a5, a6, _add
    li a6, 3
    mret
_add:
    add a6, a5, a7 # adding registers together
    # li a6, 3 # this causes a sat model to be produced
    uret
_fallthrough:
    j _fallthrough
.size _sum, .-_sum
