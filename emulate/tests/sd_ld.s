.section ._sd_ld,"ax",%progbits

.global _sd_ld
.type _sd_ld, %function
_sd_ld:
    lui a1, 0x80000
    # lui a2, 0x10038
    sd a2, 0x200(a1)
    ld a3, 0x200(a1)
    uret
_fallthrough:
    j _fallthrough
.size _sd_ld, .-_sd_ld
