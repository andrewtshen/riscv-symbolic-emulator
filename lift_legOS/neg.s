.section .startup,"ax",%progbits

.global _startup
.type _startup, %function
_startup:
    la sp, _estack
    call _neg
_junk:
    j _junk
.size _startup, .-_startup

.type _neg, %function
_neg:
    li a5, 5
    li a6, 10
    mret
.size _neg, .-_neg
