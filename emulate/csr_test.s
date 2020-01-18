.section ._csr_test,"ax",%progbits

.global _csr_test
.type _csr_test, %function
_csr_test:
    lui a1, 0x23

    csrrw a4, mstatus, a4
    mret

_fallthrough:
    j _fallthrough
.size _csr_test, .-_csr_test