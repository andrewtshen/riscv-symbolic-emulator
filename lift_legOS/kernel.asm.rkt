#lang reader serval/riscv/objdump

kernel.elf:     file format elf32-littleriscv
architecture: riscv:rv32, flags 0x00000112:
EXEC_P, HAS_SYMS, D_PAGED
start address 0x20400000


Disassembly of section .text:
20400000 <_startup> 5fc01117          	auipc	sp,0x5fc01
20400004 <_startup+0x4> 00010113          	addi	sp,sp,0 # 80001000 <_estack>
20400008 <_startup+0x8> 008000ef          	jal	ra,20400010 <_neg>
2040000c <_junk> 0000006f          	jal	zero,2040000c <_junk>
20400010 <_neg> 00500793          	addi	a5,zero,5
20400014 <_neg+0x4> 00a00813          	addi	a6,zero,10
20400018 <_neg+0x8> 30200073          	mret
