#ifndef RISCV_H
#define RISCV_H

#include <stdint.h>

// Generate Read/Write Functions for Usage
#define STRCONCAT(x, y) x ## y
#define STRCONCAT3(x, y, z) x ## y ## z
#define CSR_GEN(regname) \
  static inline void STRCONCAT(w_, regname)(uint64_t x) { \
    asm volatile("csrw " #regname ", %0" : : "r" (x)); \
  } \
  static inline uint64_t STRCONCAT(r_, regname)(void) { \
    uint64_t x; \
    asm volatile("csrr %0, " #regname : "=r" (x)); \
    return x; \
  }

// Machine Exception Program Counter, holds the
// instruction address to which a return from
// exception will go.
CSR_GEN(mepc)
// Machine Trap Vector
CSR_GEN(mtvec)
// Machine Val
CSR_GEN(mtval)
// Machine Interrupt Cause
CSR_GEN(mcause)
// Machine Scratch
CSR_GEN(mscratch)
// Machine Interrupt Delegation
CSR_GEN(mideleg)
// Machine Exception ation
CSR_GEN(medeleg)

// Machine-mode Interrupt Enable
#define MIE_MEIE (1L << 11) // external
#define MIE_MTIE (1L << 7)  // timer
#define MIE_MSIE (1L << 3)  // software
CSR_GEN(mie)

// Machine Status
#define MSTATUS_MPP_MASK (3L << 11)
#define MSTATUS_MPP_M (3L << 11)
#define MSTATUS_MPP_U (0L << 11)
CSR_GEN(mstatus)

// PMP Settings
CSR_GEN(pmpcfg0) // each contains subregions for configuring 16 regions
CSR_GEN(pmpcfg2)
#define PMPCFG_MASK (0xff)
#define PMPCFG(l, a, x, w, r) (((l & 1) << 7) | ((a & 3) << 3) | ((x & 1) << 2) | ((w & 1) << 1) | ((r & 1) << 0))
#define PMPCFG_A_OFF 0
#define PMPCFG_A_TOR 1
#define PMPCFG_A_NA4 2
#define PMPCFG_A_NAPOT 3
#define PMPICFG_GEN(i, regname, offset) \
    static inline void STRCONCAT3(w_pmp, i, cfg)(uint8_t cfg) { \
        uint64_t x; \
        asm volatile("csrr %0, " #regname : "=r" (x)); \
        x &= ~(((uint64_t) PMPCFG_MASK) << offset); \
        x |= ((uint64_t) cfg) << offset; \
        asm volatile("csrw " #regname ", %0" : : "r" (x)); \
    }

PMPICFG_GEN(0, pmpcfg0, 0) // -> static inline void w_pmp0cfg(uint8_t cfg) {...}
PMPICFG_GEN(1, pmpcfg0, 8)
PMPICFG_GEN(2, pmpcfg0, 16)
PMPICFG_GEN(3, pmpcfg0, 24)
PMPICFG_GEN(4, pmpcfg0, 32)
PMPICFG_GEN(5, pmpcfg0, 40)
PMPICFG_GEN(6, pmpcfg0, 48)
PMPICFG_GEN(7, pmpcfg0, 56)
PMPICFG_GEN(8, pmpcfg2, 0)
PMPICFG_GEN(9, pmpcfg2, 8)
PMPICFG_GEN(10, pmpcfg2, 16)
PMPICFG_GEN(11, pmpcfg2, 24)
PMPICFG_GEN(12, pmpcfg2, 32)
PMPICFG_GEN(13, pmpcfg2, 40)
PMPICFG_GEN(14, pmpcfg2, 48)
PMPICFG_GEN(15, pmpcfg2, 56)

CSR_GEN(pmpaddr0)
CSR_GEN(pmpaddr1)
CSR_GEN(pmpaddr2)
CSR_GEN(pmpaddr3)
CSR_GEN(pmpaddr4)
CSR_GEN(pmpaddr5)
CSR_GEN(pmpaddr6)
CSR_GEN(pmpaddr7)
CSR_GEN(pmpaddr8)
CSR_GEN(pmpaddr9)
CSR_GEN(pmpaddr10)
CSR_GEN(pmpaddr11)
CSR_GEN(pmpaddr12)
CSR_GEN(pmpaddr13)
CSR_GEN(pmpaddr14)
CSR_GEN(pmpaddr15)
#define PMPADDR_ENCODE_NAPOT(addr, region_size) (addr | ((region_size >> 3) - 1))

static inline void mret(void) {
  asm volatile("mret");
}

#endif