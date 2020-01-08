.section .sum,"ax",%progbits

.global _sum
.type _sum, %function
_sum:
    # li a5, 3
    # li a7, 2

    add a6, a5, a7 # adding registers together
    mret
_fallthrough:
    j _fallthrough
.size _sum, .-_sum
