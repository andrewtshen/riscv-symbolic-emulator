#include <asm/csr.h>
#include <asm/csr_bits/status.h>
#include <asm/mcall.h>
#include <asm/pmp.h>
#include <asm/ptrace.h>
#include <asm/sbi.h>
#include <asm/tlbflush.h>
#include <uapi/legOS/syscalls.h>

void main(unsigned int hartid, phys_addr_t dtb) {
    pr_info("Hello from ToyMon!\n");
}

static long sys_hello_world(void) {
    pr_info("Hello world system call!\n");
    return 0;
}

long x;

static long sys_get_and_set(long y) {
    long old = x;
    // if (y == 10) x = y + 1; // Gives you a test case!
    // else x = y; 
    x = y;
    return old;
}

static long sys_test_print(void) {
    pr_info("Test print system call!\n");
    return 0;
}

void do_trap_ecall_s(struct pt_regs *regs) {
    long nr = regs->a7, r = -ENOSYS;

    csr_write(mepc, csr_read(mepc) + 4);

    switch (nr) {
    case SBI_FIRST ... SBI_LAST:
        r = do_mcall(regs);
        break;
    case __NR_hello_world:
        r = sys_hello_world();
        break;
    case __NR_get_and_set:
        r = sys_get_and_set(regs->a0);
        break;
    case __NR_test_print:
        r = sys_test_print();
    }

    regs->a0 = r;
}
