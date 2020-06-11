#lang reader serval/lang/dwarf

kernel.elf:     file format elf32-littleriscv

Contents of the .debug_info section:

  Compilation Unit @ offset 0x0:
   Length:        0x22 (32-bit)
   Version:       2
   Abbrev Offset: 0x0
   Pointer Size:  4
 <0><b>: Abbrev Number: 1 (DW_TAG_compile_unit)
    <c>   DW_AT_stmt_list   : 0x0
    <10>   DW_AT_low_pc      : 0x20400000
    <14>   DW_AT_high_pc     : 0x20400038
    <18>   DW_AT_name        : (indirect string, offset: 0x0): sign.s
    <1c>   DW_AT_comp_dir    : (indirect string, offset: 0x7): /Users/andrewshen/Documents/primes2019/lift_asmprog
    <20>   DW_AT_producer    : (indirect string, offset: 0x3b): GNU AS 2.32
    <24>   DW_AT_language    : 32769	(MIPS assembler)

