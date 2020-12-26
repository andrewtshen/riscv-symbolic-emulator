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
.align 8
estack:
    .fill 1024
.size estack, .-estack
