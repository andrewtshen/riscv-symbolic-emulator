.section .dret,"ax", %progbits

.global _dret
.type _dret, %function

_dret:
    # testing instrs with missing instr.rkt implementations
    dret # this causes it to go to reset vector so mret doesn't return
    mret
.size _dret, .-_dret
