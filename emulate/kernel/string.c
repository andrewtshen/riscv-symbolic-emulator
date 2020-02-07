#include "string.h"

/* Copy Memory */
void *memcpy(void *dst, const void *src, size_t n) {
    char *d = dst;
    const char *s = src;
    while (n--) {
       *(d++) = *(s++);
    }
    return dst;
}

/* Set Memory */
void *memset(void *ptr, int value, size_t num) {
    char *p = ptr;
    while (num--) {
       *(p++) = value;
    }
    return ptr;
}