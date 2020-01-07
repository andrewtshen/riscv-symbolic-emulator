.section .sum,"ax",%progbits

.global _sum
.type _sum, %function
_sum:
    li a5, 3
    li a7, 5

    add a6, a5, a7 # adding registers together
    mret
_loop:
    j _loop
.size _sum, .-_sum
