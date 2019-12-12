#include <stdbool.h>
#include <pthread.h>
//#include <threads.h>

#include "lfdebug.h"

// Defines whether lfmalloc or stdlib malloc is used.
// This is used to allow lfmalloc call stdlib function which use malloc internally
// without creating an infinite recursion.
// Thread local to prevent race conditions.
//__thread bool lfmalloc_active = true;
bool lfmalloc_active = true;


void __disable_lf_alloc() {
	lfmalloc_active = false;
}

void __enable_lf_alloc() {
	lfmalloc_active = true;
}

bool __lf_alloc_enabled() {
	return lfmalloc_active;
}

pthread_t __current_thread_id() {
	return pthread_self();
}
