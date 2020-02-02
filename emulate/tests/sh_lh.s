.section ._sh_lh,"ax",%progbits

.global _sh_lh
.type _sh_lh, %function
_sh_lh:
    lui a1, 0x80000
    # lui a2, 0x10038
    sh a2, 0x200(a1)
    lh a3, 0x200(a1)
    uret
_fallthrough:
    j _fallthrough
.size _sh_lh, .-_sh_lh
