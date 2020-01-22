_stack_size = 0x4096;
/* Used to use from https://github.com/sifive/freedom-e-sdk/blob/v201908-branch/bsp/qemu-sifive-e31/metal.default.lds */
/* Now uses virt memory mapping */
MEMORY
{
    ram (wxa!ri)   : ORIGIN = 0x80000000, LENGTH = 0x01000000
}

SECTIONS
{
    .text : {
        . = ALIGN(4);
        _stext = .;
        *(.startup)
        *(.kernelvec)
        *(.text .text.*)
        . = ALIGN(4);
        _etext = .;
    } > ram

    .rodata : {
        . = ALIGN(4);
        _srodata = .;
        *(.rodata .rodata.*)
        . = ALIGN(4);
        _erodata = .;
    } > ram

    _sidata = LOADADDR(.data);
    .data : {
        . = ALIGN(4);
        _sdata = .;
        *(.sdata .sdata.*)
        *(.data .data.*)
        . = ALIGN(4);
        _edata = .;
    } > ram

    .bss : {
        . = ALIGN(4);
        _sbss = .;
        *(.bss .bss.*)
        *(COMMON)
        . = ALIGN(4);
        _ebss = .;
    } > ram

    . = . + _stack_size;
    /* Make sure that stack pointer is 16 byte aligned */
    . = ALIGN(16);
    _estack = .;
}
