#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

#include "lfmalloc.h"
#include "lfinternal.h"
#include "lfdebug.h"

void* lf_malloc(size_t size) {
	PRINTF_DBG("called malloc, size = 0x%lx\n", size);
	return __lf_malloc(size);
}

void lf_free(void *ptr) {
	__lf_free(ptr);
}

void* lf_calloc(size_t nmemb, size_t size) {
	PRINTF_DBG("called calloc, nmemb = 0x%lx, size = 0x%lx\n", nmemb, size);
	uint64_t alloc_size = nmemb * size;
	//check for overflow
	if (nmemb != 0 && alloc_size / nmemb != size) {
		PRINTF_DBG("calloc failed: nmenb * size overflows.\n");
		return NULL;
	}
	void* ptr = __lf_malloc(nmemb * size);
	void* p = memset(ptr, 0, nmemb * size);

	LF_ASSERT_EQ(p, ptr);
	return ptr;
}

void* lf_realloc(void *ptr, size_t size) {
	PRINTF_DBG("called realloc, ptr = %p, size = 0x%lx\n", ptr, size);
	if (ptr == NULL) {
		return __lf_malloc(size);
	} else if (size == 0) {
		PRINTF_DBG("failed: size = 0, freeing");
		__lf_free(ptr);
		return NULL;
	}

	if (__lf_size(ptr) >= size) { //Enough remaining padding => No reallocation necessary
		PRINTF_DBG("enough remaining space available.\n");
		return ptr;
	} else { // New allocation necessary
		PRINTF_DBG("new allocation necessary.\n");
		size_t old_size = __lf_size(ptr);
		void* new_ptr = __lf_malloc(size);
		LF_ASSERT_NE(new_ptr, NULL);
		memcpy(new_ptr, ptr, old_size);
		__lf_free(ptr);
		return new_ptr;
	}
}


void *lf_memalign(size_t alignment, size_t size) {
	PRINTF_DBG("called memalign, alignment = 0x%lx, size = 0x%lx\n", alignment, size);
	return __lf_memalign(alignment, size);
}

int lf_posix_memalign(void **memptr, size_t alignment, size_t size) {
	PRINTF_DBG("called posix_memalign, alignment = 0x%lx, size = 0x%lx\n", alignment, size);
	if (!is_pow_two(alignment)) {
		PRINTF_DBG("failed: alignment not pow of two.\n")
		return EINVAL;
	}
	if (size == 0) {
		PRINTF_DBG("failed: size is zero.\n");
		*memptr = NULL;
		return 0;
	}
	void* p = __lf_memalign(alignment, size);
	if (p != NULL) {
		*memptr = p;
		return 0;
	}
	PRINTF_DBG("failed: posix_memalign ENOMEM\n");
	return ENOMEM;
};

void *lf_aligned_alloc(size_t alignment, size_t size) {
	PRINTF_DBG("called aligned_alloc, alignment = 0x%lx, size = 0x%lx\n", alignment, size);
	if (size % alignment != 0) {
		PRINTF_DBG("failed: size not multiple of alignment.\n")
		errno = EINVAL;
		return NULL;
	}
	return __lf_memalign(alignment, size);;
}
void *lf_valloc(size_t size) {
	PRINTF_DBG("lfalloc: called valloc and failed\n")
	return __lf_memalign(sysconf(_SC_PAGESIZE), size);
}

void *lf_pvalloc(size_t size) {
	PRINTF_DBG("lfalloc: called pvalloc and failed\n")
	size_t page_size = sysconf(_SC_PAGESIZE);
	size_t alloc_size = size - size % page_size + page_size;
	LF_ASSERT(alloc_size % page_size == 0);
	return __lf_memalign(sysconf(_SC_PAGESIZE), alloc_size);
}

size_t lf_malloc_usable_size(void *ptr) {
	PRINTF_DBG("lfalloc: called malloc_usable_size and failed\n")
	return __lf_usable_size(ptr);
}

#ifdef LF_DYNAMIC_LIB
void *malloc(size_t size) {
	return lf_malloc(size);
}
void free(void *ptr) {
	return lf_free(ptr);
}
void *calloc(size_t nmemb, size_t size) {
	return lf_calloc(nmemb, size);
}
void *realloc(void *ptr, size_t size) {
	return lf_realloc(ptr, size);
}

int posix_memalign(void **memptr, size_t alignment, size_t size) {
	return lf_posix_memalign(memptr, alignment, size);
}
void *aligned_alloc(size_t alignment, size_t size) {
	return lf_aligned_alloc(alignment, size);
}
void *valloc(size_t size) {
	return lf_valloc(size);
}

void *memalign(size_t alignment, size_t size) {
	return lf_memalign(alignment, size);
}
void *pvalloc(size_t size) {
	return lf_pvalloc(size);
}

size_t malloc_usable_size(void *ptr) {
	return lf_malloc_usable_size(ptr);
}
#endif

void lf_error(void *ptr, void* base, unsigned long size, unsigned int reason,
              char* filename, unsigned int line) {
	__lf_error(ptr, base, size, reason, filename, line);
}
