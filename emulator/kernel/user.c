#include <stdint.h>

void main() {
    // printf("Hello World from User!\n");
    int a = 5;

    // // this fails the check
    // uint64_t* ptr = (uint64_t *)0x80082000;
    // *ptr = 5;

    // // this fails the check
    // uint64_t* ptr = (uint64_t *)0x8007FFFF;
    // *ptr = 5;

    // this passes the check
    uint64_t* ptr = (uint64_t *)0x80080000;
    *ptr = 5;

    asm volatile ("ecall");
    return;
}