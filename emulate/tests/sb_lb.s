.section ._sb_lb,"ax",%progbits

.global _sb_lb
.type _sb_lb, %function
_sb_lb:
    lui a1, 0x80000
    # lui a2, 0x10038
    sb a2, 0x200(a1)
    lb a3, 0x200(a1)

    mret
_fallthrough:
    j _fallthrough
.size _sb_lb, .-_sb_lb
