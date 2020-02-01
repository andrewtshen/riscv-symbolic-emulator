_stack_size = 0x4096;

SECTIONS
{
    . = 0x80800000;         /* User prog from 0x80800000 - 0x81000000 */
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
    . = ALIGN(16);
    _ustack = .;
    ASSERT((. < 0x81000000), "overflowed available area")
}
