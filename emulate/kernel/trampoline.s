.section .trampoline, "ax", %progbits

.section .init_text,"ax",%progbits
.global _trampoline
.type _trampoline, %function
_trampoline:
    la sp, ustack
    call main
    # for now use uret to finish execution
    uret
_fallthrough:
    j _fallthrough
.size _trampoline, .-_trampoline

.type ustack, %object
.align 8
ustack:
    .fill 1024
.size ustack, .-ustack
