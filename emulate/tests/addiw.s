.section .addiw,"ax",%progbits

.global _addiw
.type _addiw, %function
_addiw:
    li a0, 0x7fffffff
    addiw a1, a0, 0x000
    addiw a2, a0, 0x7ff
    li a0, 0x0
    addiw a3, a0, -1
    li a0, 0xffffffffffffffff
    addiw a4, a0, 0x001
    addiw a5, a0, -1
    uret
_fallthrough:
    j _fallthrough
.size _addiw, .-_addiw
