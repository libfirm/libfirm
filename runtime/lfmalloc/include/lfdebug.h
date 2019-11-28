#ifndef LFDEBUG_h
#define LFDEBUG_h

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/syscall.h>

void __disable_lf_alloc();
void __enable_lf_alloc();
bool __lf_alloc_enabled();
pthread_t __current_thread_id();

#define USE_STDLIB(x) {__disable_lf_alloc(); x; __enable_lf_alloc();}

/*#define PRINTF_DBG(...) {\
	pthread_t tid = __current_thread_id();\
	USE_STDLIB(fprintf(stderr, "lfmalloc (0x%lx): ", tid))\
	USE_STDLIB(fprintf(stderr, __VA_ARGS__))\
}*/
#define PRINTF_DBG(...)

#ifndef NDEBUG
#define PRINTF_ASSERT(...) USE_STDLIB(fprintf(stderr, "lfmalloc: " __VA_ARGS__))
#define LF_ASSERT(x) \
	do { if (!(x)) { \
		PRINTF_ASSERT("%s: %i: %s: Assertion '%s' failed.\n", __FILE__, __LINE__, __func__, #x); \
		abort(); \
	} } while (0);
#define LF_ASSERT_EQ(a,b) \
	do { if ((a) != (b)) { \
		PRINTF_ASSERT("%s: %i: %s: Assertion '%s == %s' (0x%lx == 0x%lx) failed.\n", __FILE__, __LINE__, __func__, #a, #b, (uint64_t)a, (uint64_t)b); \
		abort(); \
	} } while (0);
#define LF_ASSERT_NE(a,b) \
	do { if ((a) == (b)) { \
		PRINTF_ASSERT("%s: %i: %s: Assertion '%s != %s# (0x%lx != 0x%lx) failed.\n", __FILE__, __LINE__, __func__, #a, #b, (uint64_t)a, (uint64_t)b); \
		abort(); \
	} } while(0);
#else
#define LF_ASSERT(x) do { (void) sizeof(x); } while (0);
#define LF_ASSERT_EQ(a, b) do { (void) sizeof(a); (void) sizeof(b); } while (0);
#define LF_ASSERT_NE(a, b) do { (void) sizeof(a); (void) sizeof(b); } while (0);
#endif

#endif
