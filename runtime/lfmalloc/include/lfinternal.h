#ifndef LFMALLOC_H
#define LFMALLOC_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

uint64_t __lf_index(void* p);
uint64_t __lf_size(void* p);
void* __lf_base(void* p);

void __allocate_regions();
void __init_region(void* base);

uint64_t __lf_search_region_index(uint64_t size);

void *__lf_malloc(size_t size);
void __lf_free(void* ptr);

void *__lf_memalign(size_t alignment, size_t size);

size_t __lf_usable_size(void* ptr);

bool is_pow_two(size_t x);

void __lf_error(void *ptr, void* base, unsigned long size, unsigned int reason,
                char* filename, unsigned int line);

#endif
