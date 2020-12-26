_stack_size = 0x1024;

SECTIONS
{
    . = 0x80020000;         /* User prog from 0x80020000 - 0x80040000 */
    .text : 
    {   
        KEEP(*(.init_text)) /* ASM to main function */
        *(.text)            /* Text data */
    }

    _USER_ROM_START = .;
    .rodata :
    {
        *(.rodata)          /* Read only data */
    }
    
    _USER_DATA_START = .;
    .data : 
    {
        *(.data)            /* Data */
    }
    
    _USER_BSS_START = .;
    .bss : 
    {
        *(.bss)             /* Zero-initialied data */
    }
    . = . + _stack_size;
    . = ALIGN(8);
    _ustack = .;
    ASSERT((. < 0x80040000), "overflowed available area")
}
