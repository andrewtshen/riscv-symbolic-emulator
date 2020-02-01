.section .startup,"ax",%progbits

.global _startup
.type _startup, %function
_startup:
    la sp, estack
    call main
_fallthrough:
    j _fallthrough
.size _startup, .-_startup

.type estack, %object
.align 16
estack:
    .fill 4096
.size estack, .-estack
