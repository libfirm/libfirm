/**
 * Helper code to output instrumentation results
 *  @author Matthias Braun, Steven Schaefer
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Prevent the compiler from mangling the name of this function. */
void __init_firmprof(const char*, unsigned int*, size_t)
     asm("__init_firmprof");

typedef struct _profile_counter_t {
	const char *filename;
	unsigned   *counters;
	unsigned    len;
	struct _profile_counter_t *next;
} profile_counter_t;

static profile_counter_t *counters = NULL;

static void write_profiles(void)
{
	profile_counter_t *counter = counters;
	while(counter != NULL) {
		profile_counter_t *next = counter->next;
		FILE *f = fopen(counter->filename, "wb");
		if (f == NULL) {
			perror("Warning: couldn't open file for writing profiling data");
		} else {
			fputs("firmprof", f);
			fwrite(counter->counters, counter->len * sizeof(unsigned), 1, f);
			fclose(f);
		}
		free(counter);
		counter = next;
	}
}

/**
 * Register a new profile counter. This is called by separate constructors
 * for each translation unit. Incidentally, referring to this function as
 * "__init_firmprof" is perfectly linker friendly.
 */
void __init_firmprof(const char *filename,
                      unsigned int *counts, size_t len)
{
	static int initialized = 0;
	profile_counter_t *counter;

	if (!initialized) {
		initialized = 1;
		atexit(write_profiles);
	}

	counter = (profile_counter_t*) malloc(sizeof(*counter));
	if (counter == NULL)
		return;

	counter->filename = filename;
	counter->counters = counts;
	counter->next     = counters;
	counter->len      = len;

	counters = counter;
}
