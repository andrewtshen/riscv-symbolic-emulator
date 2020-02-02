.section .srliw,"ax",%progbits

.global _srliw
.type _srliw, %function
_srliw:
    li a0, 0xffffffffffffffff
    srliw a1, a0, 0
    srliw a2, a0, 1
    srliw a3, a0, 7
    srliw a4, a0, 14
    srliw a5, a0, 31
    uret
_fallthrough:
    j _fallthrough
.size _srliw, .-_srliw
