#include<stdint.h>

#define PMPCFG_A_NAPOT 3
#define PMPCFG(l, a, x, w, r) (((l & 1) << 7) | ((a & 3) << 3) | ((x & 1) << 2) | ((w & 1) << 1) | ((r & 1) << 0))
#define PMPCFG_MASK (0xff)


// Generate Read/Write Functions for Usage
#define STRCONCAT(x, y) x ## y
#define STRCONCAT3(x, y, z) x ## y ## z
#define CSR_GEN(regname) \
    static inline void STRCONCAT(w_, regname)(int64_t x) { \
        asm volatile("csrw " #regname ", %0" : : "r" (x)); \
    } \
    static inline int STRCONCAT(r_, regname)(void) { \
        int x; \
        asm volatile("csrr %0, " #regname : "=r" (x)); \
        return x; \
    }

#define PMPICFG_GEN(i, regname, offset) \
    static inline void STRCONCAT3(w_pmp, i, cfg)(uint64_t cfg) { \
        uint64_t x; \
        asm volatile("csrr %0, " #regname : "=r" (x)); \
        x &= ~(((uint64_t) PMPCFG_MASK) << offset); \
        x |= ((uint64_t) cfg) << offset; \
        asm volatile("csrw " #regname ", %0" : : "r" (x)); \
    } \
    static inline int STRCONCAT3(r_pmp, i, cfg)(void) { \
        int x; \
        asm volatile("csrr %0, " #regname : "=r" (x)); \
        return x; \
    }

// Retrieve Proper NAPOT Address for Base and Size
uint64_t get_pmp_napot_addr(uint64_t base, uint64_t size) {
    uint64_t napot_size = ((size/2)-1);
    uint64_t pmp_addr = (base + napot_size)>>2;
    return pmp_addr;
}

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
PMPICFG_GEN(0, pmpcfg0, 0)
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

static inline uint64_t lsr(uint64_t x, uint64_t n) {
  return (x >> n);
}

static inline int ctz64(uint64_t val) {
    int numz = 0;
    if (val == 0)
        return 0;
    while (val % 2 != 1) {
        val = val / 2;
        numz++;
    }
    return numz;
}

static void pmp_decode_napot(uint64_t a) {
    /*
       aaaa...aaa0   8-byte NAPOT range
       aaaa...aa01   16-byte NAPOT range
       aaaa...a011   32-byte NAPOT range
       ...
       aa01...1111   2^XLEN-byte NAPOT range
       a011...1111   2^(XLEN+1)-byte NAPOT range
       0111...1111   2^(XLEN+2)-byte NAPOT range
       1111...1111   Reserved
    */
    uint64_t t1 = ctz64(~a);
    uint64_t base = (a & ~(((uint64_t)1 << t1) - 1)) << 2;
    uint64_t range = ((uint64_t)1 << (t1 + 3)) - 1;
    return;
}

static void pmp_init(void) {
    // PMP region 0: user has RWX to their memory
    w_pmpaddr0(get_pmp_napot_addr(0x80800000L, 0x800000L));
    w_pmp0cfg(PMPCFG(0, PMPCFG_A_NAPOT, 1, 1, 1));

    // PMP region 1: enable uart for all users
    w_pmpaddr1(get_pmp_napot_addr(0x10000000L, 0x800000L));
    w_pmp1cfg(PMPCFG(0, PMPCFG_A_NAPOT, 1, 1, 1));

    // PMP region 2: user has no access to entire memory range
    // need to make this somehow #xcfffffff.... and do correct lsr
    int64_t v = lsr((~0L), 1);
    w_pmpaddr8(v);
    w_pmp8cfg(PMPCFG(0, PMPCFG_A_NAPOT, 0, 0, 0));
}

int main() {
    pmp_init();
}