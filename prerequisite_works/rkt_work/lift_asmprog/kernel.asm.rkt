#lang reader serval/riscv/objdump

kernel.elf:     file format elf32-littleriscv
architecture: riscv:rv32, flags 0x00000112:
EXEC_P, HAS_SYMS, D_PAGED
start address 0x20400000


Disassembly of section .text:
20400000 <_startup> 5fc01117          	auipc	sp,0x5fc01
20400004 <_startup+0x4> 00010113          	addi	sp,sp,0 # 80001000 <_estack>
20400008 <_startup+0x8> 008000ef          	jal	ra,20400010 <_sign>
2040000c <_junk> 0000006f          	jal	zero,2040000c <_junk>
20400010 <_sign> 010788b3          	add	a7,a5,a6
20400014 <_sign+0x4> 00064663          	blt	a2,zero,20400020 <_neg>
20400018 <_sign+0x8> 00c04863          	blt	zero,a2,20400028 <_pos>
2040001c <_sign+0xc> 00c05a63          	bge	zero,a2,20400030 <_zero>
20400020 <_neg> fff00693          	addi	a3,zero,-1
20400024 <_neg+0x4> 30200073          	mret
20400028 <_pos> 00100693          	addi	a3,zero,1
2040002c <_pos+0x4> 30200073          	mret
20400030 <_zero> 00000693          	addi	a3,zero,0
20400034 <_zero+0x4> 30200073          	mret
