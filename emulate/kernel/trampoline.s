.section .trampoline, "ax", %progbits

.section .init_text,"ax",%progbits
.global _trampoline
.type _trampoline, %function
_trampoline:
    la sp, ustack
    call main
_fallthrough:
    j _fallthrough
.size _trampoline, .-_trampoline

.type ustack, %object
.align 16
ustack:
    .fill 4096
.size ustack, .-ustack
