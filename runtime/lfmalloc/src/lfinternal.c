#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/cdefs.h>
#include <sys/mman.h>
#include <stdbool.h>

#include <pthread.h>

#include "lfinternal.h"
#include "lfutil.h"
#include "lfdebug.h"

extern void* __libc_malloc(size_t size);
extern void* __libc_memalign(size_t alignment, size_t size);
extern void __libc_free(void* ptr);

#include "gen_lfasan_sizes.h" // defines SIZES and REGION_COUNT

//const uint64_t REGION_BITS = 32;
//const uint64_t REGION_SIZE =  ((uint64_t)1 << REGION_BITS);
//const uint64_t START_REGION = 0x100000000; //Beginning of Region 1;
//const uint64_t MAX_SIZE = (1 << 30);

bool lfmalloc_init = false;

//Represents a region with possible allocations of a specific size
//The region has two parts.
//The first part is a freelist.
//The second is contigous unallocated memory called remainder.
//head points to the beginning of the freelist.
//tail points to the beginning of the remainder.
//if the freelist is full, the tail is moved into the remainder, therefore reducing its size. The newly available space is now used in the freelist.
//if head == tail, the freelist is filled up.
//if head is NULL the region is filled up. (in this case tail has to be null aswell)
//To improve multithreaded performance, each region has a mutex, so that
//multiple threads can allocate different sizes at the same time.
typedef struct region {
	void* head;
	void* tail;
	pthread_mutex_t mutex;
} region;

region region_freelists[REGION_COUNT] = { NULL };

//If malloc is called with size zero, the address of this global is returned.
//Some functions (regcomp) expect malloc to return a unique address if size == 0,
//and think NULL always means nomem.
const char zero_addr;

void __map_region_space() {
  PRINTF_DBG("Allocating regions\n")
  for (int i = 1; i < REGION_COUNT; i++) {
    void *s =
        mmap((void *)(REGION_SIZE * i), REGION_SIZE, PROT_READ | PROT_WRITE,
             MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
	LF_ASSERT_NE(s, NULL);
    // printf("mmap pointer: %p\n", s);
  }
}

uint64_t __lf_index(void* p) {
	return (uint64_t)p >> REGION_BITS;
}

uint64_t __lf_size(void* p) {
	return SIZES[__lf_index(p)];
}

void* __lf_base(void* p) {
	uint64_t size = __lf_size(p);
	return (void*)(((uint64_t)p / size) * size);
}

void* __region_base(void* p) {
	return (void*)(((uint64_t)-1 << REGION_BITS) & (uint64_t)p);
}

void __init_region(void* base) {
	LF_ASSERT_EQ(base, __region_base(base));
	region r;
	r.head = base;
	r.tail = base;
	if (pthread_mutex_init(&r.mutex, NULL) != 0) {
		PRINTF_DBG("Couldnt init mutex");
		abort();
	}
	region_freelists[__lf_index(base)] = r;
}

uint64_t __lf_search_region_index(uint64_t size) {
	// linear search through SIZES. TODO: Maybe optimize somehow
	for (int i = 1; 1; i++) {
		if (SIZES[i] < UINT64_MAX && SIZES[i] >= size) {
			return i;
		}
	}
	return -1;
}

void* __allocate_in_region(size_t region_idx, size_t size) {
	region* r = &region_freelists[region_idx];
	if (r->head == NULL) {
		PRINTF_DBG("failed: region filled up, deferring to __libc_malloc\n");
		return __libc_malloc(size);
	} else {
		PRINTF_DBG(
			"allocating 0x%lx bytes, in region: %lu (0x%lx) at %p\n",
			size,
			region_idx,
			SIZES[region_idx],
			r->head
		);
	}

	void* p = r->head;
	if (r->head == r->tail) { //Freelist filled: use remainder
		uint64_t new_tail = (uint64_t)r->tail + SIZES[region_idx];

		if (new_tail >= REGION_SIZE * region_idx + REGION_SIZE) {
			r->tail = NULL;
			PRINTF_DBG("failed: region %lu was filled up.\n", region_idx);
		} else {
			r->tail = (void*) new_tail;
		}
		r->head = r->tail;
	} else { // remove first element from freelist
		r->head = *(void**)r->head;
	}
	return p;
}

void __deallocate_in_region(size_t region_idx, void* ptr) {
	region* r = &region_freelists[region_idx];
	*(void**) ptr = r->head;
	r->head = ptr;
}

void* __freelist_end(size_t region_idx) {
	region* r = &region_freelists[region_idx];
	void* prev = &r->head;
	while (*(void**)prev != r->tail) {
		prev = *(void**) prev;
	}
	return prev;
}

// Allocate memory at ptr, if impossible return NULL otherwise ptr
void* __allocate_fixed_in_region(size_t region_idx, void* ptr) {
	region* r = &region_freelists[region_idx];
	uint64_t region_base = region_idx * REGION_SIZE;
	if ((uint64_t)ptr < region_base || (uint64_t)ptr > region_base + REGION_SIZE) {
		return NULL; //ptr is not in region.
	}
	if ((uint64_t)ptr % SIZES[region_idx] != 0) {
		return NULL; //ptr is not aligned
	}

	if (ptr < r->tail) {
		//ptr located in freelist
		void* prev = &r->head;
		void* next = r->head;
		while(next != r->tail) {
			if (next == ptr) {
				//ptr found in freelist
				*(void**) prev = *(void**)next;
				return ptr;
			}
			prev = next;
			next = *(void**)next;
		}
		return NULL; //ptr not found in freelist.
	} else {
		//ptr located in remainder
		if (ptr == r->tail) {
			void* e = __freelist_end(region_idx);
			r->tail = ptr + SIZES[region_idx];
			*(void**)e = r->tail;
		} else {
			void* prev = r->tail;
			void* next = prev + SIZES[region_idx];
			r->tail = ptr + SIZES[region_idx];
			while (next < ptr) {
				*(void**) prev = next;
				prev = next;
				next += SIZES[region_idx];
			}
			*(void**) prev = r->tail;
		}
		return ptr;
	}
}

void __lock_region(size_t region_idx) {
	pthread_mutex_lock(&region_freelists[region_idx].mutex);
}

void __unlock_region(size_t region_idx) {
	pthread_mutex_unlock(&region_freelists[region_idx].mutex);
}

//TODO: lock entire allocator when initializing it
void __init() {
	// lf_malloc initialization: mmap syscalls and freelist init
	if (!lfmalloc_init) {
		__map_region_space();
		for (int i = 1; i < REGION_COUNT; i++) {
			__init_region((void *)(REGION_SIZE * i));
		}
		lfmalloc_init = true;
	}
}

void *__lf_malloc(size_t size) {
	if (!__lf_alloc_enabled()) {
		return __libc_malloc(size);
	}
	 //Allow stdlib to use stdlib malloc
	__init();

	// refer to libc_malloc if size is too large
	if (size > MAX_SIZE) {
		PRINTF_DBG("size (0x%lx) too large, deferring to __libc_malloc.\n", size);
		return __libc_malloc(size);

	} else if (size == 0) {
		PRINTF_DBG("failed: size = 0\n");
		return (void*) &zero_addr;
	}

	// actual lf_malloc allocation
	uint64_t region_idx = __lf_search_region_index(size);
	__lock_region(region_idx);
	void* p = __allocate_in_region(region_idx, size);
	__unlock_region(region_idx);

	return p;
}

void __lf_free(void* ptr) {
	if (!__lf_alloc_enabled()) {
		return __libc_free(ptr);
	}

	__init();

	if (ptr == NULL) {
		PRINTF_DBG("freeing NULL\n");
		return;
	}
	if (ptr == (void*)&zero_addr) {
		PRINTF_DBG("freeing zero allocation.\n");
		return;
	}

	void* base = __lf_base(ptr);
	uint64_t region_idx = __lf_index(ptr);
	PRINTF_DBG("freeing pointer %p, region: %lu (0x%lx), base %p\n", ptr, region_idx, SIZES[region_idx], base);
	if (region_idx > 0 && region_idx < REGION_COUNT) { //ptr is in a region.
		LF_ASSERT_EQ(base, ptr);
		__lock_region(region_idx);
		__deallocate_in_region(region_idx, ptr);
		__unlock_region(region_idx);
	} else {
		PRINTF_DBG("freeing pointer (%p) outside of regions: defering to __libc_free\n", ptr);
		__libc_free(ptr);
	}
}

bool is_pow_two(size_t x) {
	size_t s = 2;
	for (int i = 0; i < sizeof(size_t) * 8; i++) {
		if (s == x)
			return true;
		else
			s <<= 1;
	}
	return false;
}

#define DBG_MEMALIGN_RESULT(p) PRINTF_DBG("memalign allocated at %p\n", p)
#define CORRECT_ALIGNMENT(a, x) (((uint64_t)(a - 1) & (uint64_t)x) == 0)

void *__lf_memalign(size_t alignment, size_t size) {
	if (!__lf_alloc_enabled()) {
		return __libc_memalign(alignment, size);
	}
	__init();

	if (!is_pow_two(alignment)) {
		PRINTF_DBG("failed: alignment not power of two.\n");
		return NULL;
	}
	if (alignment >= REGION_SIZE) {
		PRINTF_DBG("failed: alignment too large\n");
		return NULL;
	}

	size_t region_idx = __lf_search_region_index(size);
	__lock_region(region_idx);
	region* r = &region_freelists[region_idx];
	PRINTF_DBG("memalign allocation in region %lu (0x%lx)\n", region_idx, SIZES[region_idx]);
	//First look in freelist for aligned spot
	void* prev = &r->head;
	void* next = r->head;
	while (next != r->tail) {
		if (CORRECT_ALIGNMENT(alignment, next)) {
			*(void**)prev = *(void**)next;
			DBG_MEMALIGN_RESULT(next);
			__unlock_region(region_idx);
			return next;
		}
		next = *(void**)next;
	}

	//otherwise take from remainder
	if (CORRECT_ALIGNMENT(alignment, r->tail)) {
		void *p = __allocate_fixed_in_region(region_idx, r->tail);
		LF_ASSERT_NE(p, NULL);
		DBG_MEMALIGN_RESULT(p);
		__unlock_region(region_idx);
		return p;
	}
	//next aligned address after tail.
	uint64_t m = lcm(alignment, SIZES[region_idx]);
	uint64_t mod = ((uint64_t)r->tail + alignment) % m;
	uint64_t start = ((uint64_t)r->tail + alignment) - mod + m;
	LF_ASSERT(CORRECT_ALIGNMENT(alignment, start));
	if (start < REGION_SIZE * region_idx + REGION_SIZE) {
		void *p = __allocate_fixed_in_region(region_idx, (void*)start);
		LF_ASSERT_NE(p, NULL);
		DBG_MEMALIGN_RESULT(p);
		__unlock_region(region_idx);
		return p;
	}

	//TODO: If it is impossible to allocated from ideal region, try another one.
	__unlock_region(region_idx);
	PRINTF_DBG("failed: No aligned spot available in region\n");
	return NULL;
}

size_t __lf_usable_size(void* ptr) {
	__init();
	size_t region_idx = __lf_index(ptr);

	if (region_idx == 0 || region_idx > REGION_COUNT) {
		//TODO: can't easily libc malloc_usable_size.
		PRINTF_DBG("Pointer not allocated by lfmalloc, returning 0;");
		return 0;
	} else {
		return SIZES[region_idx];
	}
}

void __lf_error(void *ptr, void* base, unsigned long size, unsigned int reason,
                char* filename, unsigned int line) {
	char *s_reason = NULL;
	switch (reason) {
		case MEMORY_WRITE:
			s_reason = "MEMORY WRITE";
			break;
		case MEMORY_READ:
			s_reason = "MEMORY READ";
			break;
		case MEMORY_ESCAPE:
			s_reason = "MEMORY ESCAPE";
			break;
		case FUNCTION_ESCAPE:
			s_reason = "FUNCTION ESCAPE";
			break;
		case RETURN_ESCAPE:
			s_reason = "RETURN ESCAPE";
			break;
		default:
			s_reason = "UNKNOWN";
	}

	if (filename != NULL && line != 0) {
		fprintf(stderr, "%s at %p out of bounds (base: %p, size: 0x%lx) in %s:%i.\n",
		        s_reason, ptr, base, size, filename, line);
	} else {
		fprintf(stderr, "%s at %p out of bounds (base: %p, size: 0x%lx).\n",
		        s_reason, ptr, base, size);
	}
	abort();
}
