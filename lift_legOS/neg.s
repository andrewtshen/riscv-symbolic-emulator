.section .startup,"ax",%progbits

.global _startup
.type _startup, %function
_startup:
    la sp, _estack
_junk:
    j _junk
.size _startup, .-_startup
