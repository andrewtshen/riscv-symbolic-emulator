.section .addw,"ax", %progbits

.global _addw
.type _addw, %function

_addw:
    # li a5, 231
    addw a6, a5, a3
    mret
.size _addw, .-_addw
