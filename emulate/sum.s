.section .sum,"ax",%progbits

.global _sum
.type _sum, %function
_sum:
    M = (0x3 << 20) >> 20
    K = ((0x3-M) >> 12) << 12
    LUI a5, K
    ADDI a5, a5, M

    M = (0x5 << 20) >> 20
    K = ((0x5-M) >> 12) << 12
    LUI a7, K
    ADDI a7, a7, M
    add a6, a5, a7 # adding registers together
    mret
_loop:
    j _loop
.size _sum, .-_sum
