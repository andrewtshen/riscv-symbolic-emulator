.section ._sw_lw,"ax",%progbits

.global _sw_lw
.type _sw_lw, %function
_sw_lw:
    lui a1, 0x80000
    # lui a2, 0x10038
    sw a2, 0x200(a1)
    lw a3, 0x200(a1)

    mret
_fallthrough:
    j _fallthrough
.size _sw_lw, .-_sw_lw
