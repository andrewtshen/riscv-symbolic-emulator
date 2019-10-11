
kernel.elf:     file format elf32-littleriscv
architecture: riscv:rv32, flags 0x00000112:
EXEC_P, HAS_SYMS, D_PAGED
start address 0x20400000


Disassembly of section .text:
20400000 <_startup> 5fc01117          	auipc	sp,0x5fc01
20400004 <_startup+0x4> 00010113          	addi	sp,sp,0 # 80001000 <_estack>
20400008 <_junk> 0000006f          	jal	zero,20400008 <_junk>
