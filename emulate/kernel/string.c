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

/* Move Memory */
void *memmove(void *dst, const void *src, size_t n) {
    char *d = dst;
    const char *s = src;
    if (s == d) {
        return dst;
    } else if (d < s) {
        // copy in forward direction
        while (n--) {
            *(d++) = *(s++);
        }
    } else {
        // copy in reverse direction
        d = d + n - 1;
        s = s + n - 1;
        while (n--) {
            *(d--) = *(s--);
        }
    }
    return dst;
}

/* First occurance of char in string */
char *strchr(const char *str, int c) {
    for (; *str; str++) {
        if (*str == c) {
            return (char *) str;
        }
    }
    return NULL;
}

/* Set Memory */
void *memset(void *ptr, int value, size_t num) {
    char *p = ptr;
    while (num--) {
       *(p++) = value;
    }
    return ptr;
}

/* Return string length */
int strlen(const char *s) {
    int n;

    for(n = 0; s[n] != 0; n++)
        ;
    return n;
}

/* Concatenate src to dest of total length size */
char* strcat(char* dest, const char* src, int size) {
    /* Check if overflows dest */
    if (strlen(dest) + strlen(src) > size-1) return 0;

    char* ptr = dest + strlen(dest);

    while (*src != '\0')
        *ptr++ = *src++;

    *ptr = '\0';

    return dest;
}

/* Concatenate char src to destination of total length size */
char* strcatc(char* dest, char src, int size) {
    /* Check if overflows dest*/
    if (strlen(dest) + 1 >= size-1) return 0;

    /* Add char onto end */
    char* p = dest + strlen(dest);
    *p++ = src;
    *p = '\0';

    return dest;
}

/* Copy string from src to dest of total length size */
char* strncpy(char* dest, const char* src, int num, int size) {
    /* Return if no memory is allocated to the dest or dest overflows */
    if (dest == 0) return 0;
    if (strlen(src) >= size-1) return 0;

    /* Copy over string */
    char* ptr = dest;
    while (*src && num--) {
        *dest++ = *src++;
    }
    *dest = '\0';

    return ptr;
}

/* Boolean string equals, returns 0 if true, 1 if false */
int strcmp(const char *s1, const char *s2) {
    while((*s1!='\0') || (*s2!='\0')) {
        if(*s1 > *s2) return 0;
        if(*s1 < *s2) return 0;
        s1++;
        s2++;
    }
    return 1;
}

