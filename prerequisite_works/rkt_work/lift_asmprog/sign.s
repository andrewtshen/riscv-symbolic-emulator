.section .startup,"ax",%progbits

.global _startup
.type _startup, %function
_startup:
    la sp, _estack
    call _sign
_junk:
    j _junk
.size _startup, .-_startup

.type _sign, %function
_sign:
    # li a2, 0
    # li a7, 2
    add a7, a5, a6 # adding registers together!
    # mret
    
    blt a2, x0, _neg
    blt x0, a2, _pos
    ble a2, x0, _zero
 _neg:
    li a3, -1
    mret
 _pos:
    li a3, 1
    mret
 _zero:
    li a3, 0
    mret
.size _sign, .-_sign
