#ifndef LFALLOC_H
#define LFALLOC_H

#include<stdlib.h>

void *lf_malloc(size_t size);
void lf_free(void *ptr);
void *lf_calloc(size_t nmemb, size_t size);
void *lf_realloc(void *ptr, size_t size);

int lf_posix_memalign(void **memptr, size_t alignment, size_t size);
void *lf_aligned_alloc(size_t alignment, size_t size);
void *lf_valloc(size_t size);

void *lf_memalign(size_t alignment, size_t size);
void *lf_pvalloc(size_t size);

size_t lf_malloc_usable_size(void *ptr);

void lf_error(void *ptr, void* base, unsigned long size, unsigned int reason,
              char* filename, unsigned int line);

#endif
