#include <asm/csr.h>
#include <asm/mcall.h>
#include <asm/sbi.h>
#include <uapi/legOS/syscalls.h>

void main() {
    int a = 5;
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

    switch (nr) {
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
