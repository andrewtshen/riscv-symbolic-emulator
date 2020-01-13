.section .sum,"ax",%progbits

.global _sum
.type _sum, %function
_sum:
    lui a3, 0x80000
    auipc a4, 4
    lw a2, 0x8(a3)


    li a5, -2
    li a6, 3
    blt a5, a6, _add

    li a6, 3
    mret
_add:
    # li a7, 8
    add a6, a5, a7 # adding registers together
    mret
_fallthrough:
    j _fallthrough
.size _sum, .-_sum
