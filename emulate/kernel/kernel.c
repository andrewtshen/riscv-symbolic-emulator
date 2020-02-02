#include <stdarg.h>
#include <stdint.h>
#include "riscv.h"
#include "string.h"

extern const uint8_t _binary_kernel_user_bin_start[];
extern const uint8_t _binary_kernel_user_bin_end[];
extern void _trampoline();
extern void kernelvec();
static void load_user(void);
static void run_user(void);
uint64_t get_pmp_napot_addr(uint64_t base, uint64_t size);
static void pmp_init(void);
static void pmp_decode_napot(uint64_t a);
static inline int ctz64(uint64_t val);

#define USER_BASE 0x80080000L
#define USER_SIZE 0x00002000L
#define PROG_INDEX 0
#define PROG_SIZE (512*1024)
#define DATA_SIZE (512*1024)

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

// Retrieve Proper NAPOT Address for Base and Size
uint64_t get_pmp_napot_addr(uint64_t base, uint64_t size) {
    uint64_t napot_size = ((size/2)-1);
    uint64_t pmp_addr = (base + napot_size)>>2;
    return pmp_addr;
}

static void pmp_init(void) {
    // PMP region 0: user has RWX to their memory
    w_pmpaddr0(get_pmp_napot_addr(0x80080000L, 0x2000L));
    w_pmp0cfg(PMPCFG(0, PMPCFG_A_NAPOT, 1, 1, 1));

    // PMP region 1: enable uart for all users
    w_pmpaddr1(get_pmp_napot_addr(0x10000000L, 0x800000L));
    w_pmp1cfg(PMPCFG(0, PMPCFG_A_NAPOT, 1, 1, 1));

    // PMP region 2: user has no access to entire memory range
    w_pmpaddr8((~0L) >> 1);
    w_pmp8cfg(PMPCFG(0, PMPCFG_A_NAPOT, 0, 0, 0));
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
}

void main() {
    // set up pmp
    pmp_init();

    // set M Previous Privilege mode to User Mode, for mret.
    unsigned long x = r_mstatus();
    x &= ~MSTATUS_MPP_MASK; // clear specific mpp bits
    x |= MSTATUS_MPP_U; // set mpp bits
    w_mstatus(x);

    // set up kernel trap vector
    w_mtvec((uint64_t)kernelvec);

    load_user();
    run_user();
}

static void load_user(void) {
    // const void *prog_start = _binary_kernel_user_bin_start + PROG_INDEX * (PROG_SIZE + DATA_SIZE);
    // memcpy((void *) USER_BASE, prog_start, PROG_SIZE);
    memcpy((void *) USER_BASE, _binary_kernel_user_bin_start, USER_SIZE);
}

static void run_user(void) {
    // set M Exception Program Counter to user, for mret.
    // requires gcc -mcmodel=medany
    w_mepc(USER_BASE);
    uint64_t mstatus = r_mstatus();
    w_mstatus((mstatus & ~MSTATUS_MPP_MASK) | MSTATUS_MPP_U);
    // switch to supervisor mode and jump to main().
    mret();
}
