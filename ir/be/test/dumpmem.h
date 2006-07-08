#include <stdio.h>

static void dumpMem(void* ptr, size_t size)
{
    size_t i;
    char* p = ptr;
    printf("\n");
    for(i = 0; i < size; ++i) {
        printf("%02hhx", p[i]);
        if((i % 4) == 3) {
            printf("\n");
        }
    }
    printf("\n");
}
