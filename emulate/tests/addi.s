.section .addi,"ax", %progbits

.global _addi
.type _addi, %function

_addi:
    # li a5, 231
    addi a6, a5, 32
    uret
.size _addi, .-_addi
