#ifndef STRING_H
#define STRING_H

#include <stddef.h>

void *memcpy(void *dst, const void *src, size_t n);
void *memmove(void *dst, const void *src, size_t n);
char *strchr(const char *str, int c);
void *memset(void *ptr, int value, size_t num);
int strlen(const char*s);
char* strcat(char* dest, const char* src, int size);
char* strcatc(char* s, char t, int size);
char* strncpy(char* dest, const char* src, int num, int size);
int strcmp(const char* s1, const char* s2);

#endif

