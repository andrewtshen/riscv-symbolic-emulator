#include "riscv.h"
#include <stddef.h>

#define NELEM(x) (sizeof(x)/sizeof((x)[0]))

static const char *mcause_desc(uint64_t stval);

void traphandler() {
    uint64_t mepc = r_mepc();
    uint64_t mstatus = r_mstatus();
    uint64_t mcause = r_mcause();

    // the yield() may have caused some traps to occur,
    // so restore trap registers for use by kernelvec.S's sepc instruction.
    w_mepc(mepc);
    w_mstatus(mstatus);

    // for now just loop
    while (1) {
        ;
    }
}

static const char *mcause_desc(uint64_t mtval) {
    static const char *intr_desc[16] = {
        [0] "user software interrupt",
        [1] "supervisor software interrupt",
        [2] "<reserved for future standard use>",
        [3] "<reserved for future standard use>",
        [4] "user timer interrupt",
        [5] "supervisor timer interrupt",
        [6] "<reserved for future standard use>",
        [7] "<reserved for future standard use>",
        [8] "user external interrupt",
        [9] "supervisor external interrupt",
        [10] "<reserved for future standard use>",
        [11] "<reserved for future standard use>",
        [12] "<reserved for future standard use>",
        [13] "<reserved for future standard use>",
        [14] "<reserved for future standard use>",
        [15] "<reserved for future standard use>",
        };
        static const char *nointr_desc[16] = {
        [0] "instruction address misaligned",
        [1] "instruction access fault",
        [2] "illegal instruction",
        [3] "breakpoint",
        [4] "load address misaligned",
        [5] "load access fault",
        [6] "store/AMO address misaligned",
        [7] "store/AMO access fault",
        [8] "environment call from U-mode",
        [9] "environment call from S-mode",
        [10] "<reserved for future standard use>",
        [11] "<reserved for future standard use>",
        [12] "instruction page fault",
        [13] "load page fault",
        [14] "<reserved for future standard use>",
        [15] "store/AMO page fault",
    };
    uint64_t interrupt = mtval & 0x8000000000000000L;
    uint64_t code = mtval & ~0x8000000000000000L;
    if (interrupt) {
        if (code < NELEM(intr_desc)) {
            return intr_desc[code];
        } else {
            return "<reserved for platform use>";
    }
    } else {
        if (code < NELEM(nointr_desc)) {
            return nointr_desc[code];
        } else if (code <= 23) {
            return "<reserved for future standard use>";
        } else if (code <= 31) {
            return "<reserved for custom use>";
        } else if (code <= 47) {
            return "<reserved for future standard use>";
        } else if (code <= 63) {
            return "<reserved for custom use>";
        } else {
            return "<reserved for future standard use>";
        }
    }
}