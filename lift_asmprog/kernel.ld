_stack_size = 0x1000;

/* from https://github.com/sifive/freedom-e-sdk/blob/v201908-branch/bsp/qemu-sifive-e31/metal.default.lds */
MEMORY
{
    flash (rxai!w) : ORIGIN = 0x20400000, LENGTH = 0x1fc00000
    ram (wxa!ri)   : ORIGIN = 0x80000000, LENGTH = 0x400000
}

SECTIONS
{
    .text : {
        . = ALIGN(4);
        _stext = .;
        *(.startup)
        *(.text .text.*)
        . = ALIGN(4);
        _etext = .;
    } > flash

    .rodata : {
        . = ALIGN(4);
        _srodata = .;
        *(.rodata .rodata.*)
        . = ALIGN(4);
        _erodata = .;
    } > flash

    .stack (NOLOAD) : {
        . = ALIGN(8);
        _sstack = .;
        . = . + _stack_size;
        . = ALIGN(8);
        _estack = .;
    } > ram

    _sidata = LOADADDR(.data);
    .data : {
        . = ALIGN(4);
        _sdata = .;
        *(.sdata .sdata.*)
        *(.data .data.*)
        . = ALIGN(4);
        _edata = .;
    } > ram AT > flash

    .bss (NOLOAD) : {
        . = ALIGN(4);
        _sbss = .;
        *(.bss .bss.*)
        *(COMMON)
        . = ALIGN(4);
        _ebss = .;
    } > ram
}
