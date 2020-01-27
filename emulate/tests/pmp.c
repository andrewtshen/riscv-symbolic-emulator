#define PMPCFG_A_NAPOT 3
#define PMPCFG(l, a, x, w, r) (((l & 1) << 7) | ((a & 3) << 3) | ((x & 1) << 2) | ((w & 1) << 1) | ((r & 1) << 0))
#define PMPCFG_MASK (0xff)


// Generate Read/Write Functions for Usage
#define STRCONCAT(x, y) x ## y
#define STRCONCAT3(x, y, z) x ## y ## z
#define CSR_GEN(regname) \
  static inline void STRCONCAT(w_, regname)(int x) { \
    asm volatile("csrw " #regname ", %0" : : "r" (x)); \
  } \
  static inline int STRCONCAT(r_, regname)(void) { \
    int x; \
    asm volatile("csrr %0, " #regname : "=r" (x)); \
    return x; \
  }

#define PMPICFG_GEN(i, regname, offset) \
    static inline void STRCONCAT3(w_pmp, i, cfg)(int cfg) { \
        int x; \
        asm volatile("csrr %0, " #regname : "=r" (x)); \
        x &= ~(PMPCFG_MASK << offset); \
        x |= cfg << offset; \
        asm volatile("csrw " #regname ", %0" : : "r" (x)); \
    }

int get_pmp_napot_addr(int base, int size) {
    int napot_size = ((size/2)-1);
    int pmp_addr = (base + napot_size)>>2;
    return pmp_addr;
}

CSR_GEN(pmpcfg0)
CSR_GEN(pmpaddr0)
PMPICFG_GEN(0, pmpcfg0, 0)

static inline int lsr(int x, int n) {
  return (int)((unsigned int)x >> n);
}

static inline int ctz64(int val) {
    int numz = 0;
    if (val == 0)
        return 0;
    while (val % 2 != 1) {
        val = lsr(val, 1);
        numz++;
    }
    return numz;
}

static void pmp_decode_napot(int a) {
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
    int t1 = ctz64(~a);
    int base = (a & ~(((int)1 << t1) - 1)) << 2;
    int range = ((int)1 << (t1 + 3)) - 1;
    return;
}

int main() {
    w_pmpaddr0(get_pmp_napot_addr(0x80800000L, 0x800000L));
    w_pmp0cfg(PMPCFG(0, PMPCFG_A_NAPOT, 1, 1, 1));
    int a = get_pmp_napot_addr(0x80800000L, 0x800000L);
    pmp_decode_napot(a);
}