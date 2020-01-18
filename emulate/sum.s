.section .sum,"ax",%progbits

.global _sum
.type _sum, %function
_sum:
    lui a1, 0x80000
    # lui a2, 0x10038
    sd a2, 0x200(a1)
    ld a3, 0x200(a1)

    li a4, 1

    li a5, -2
    li a6, 3
    blt a5, a6, _add

    li a6, 3
    mret
_add:
    add a6, a5, a7 # adding registers together
    mret
_fallthrough:
    j _fallthrough
.size _sum, .-_sum
