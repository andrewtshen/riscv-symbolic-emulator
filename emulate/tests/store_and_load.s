.section ._store_and_load,"ax",%progbits

.global _store_and_load
.type _store_and_load, %function
_store_and_load:
    lui a1, 0x80000
    # lui a2, 0x10038
    sd a2, 0x200(a1)
    ld a3, 0x200(a1)

    mret
_fallthrough:
    j _fallthrough
.size _store_and_load, .-_store_and_load
