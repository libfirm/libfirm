
#ifndef _TICKS_H
#define _TICKS_H

/*
 * To use the Pentium RDTSC timer
 * define TIMING_USE_RDTSC when including
 */
#define TIMING_USE_RDTSC

#include <unistd.h>
#include <time.h>
#include <sys/time.h>

/* define GNU macro for processor affinity stuff if on linux */
#if defined __linux__ && !defined __USE_GNU
#define __USE_GNU
#endif
#include <sched.h>

typedef struct {
#ifdef _POSIX_PRIORITY_SCHEDULING
	struct sched_param params;
#endif
	int scheduler;
#ifdef __linux__
	cpu_set_t affinity;
#endif
} timing_sched_env_t;

/* only use rdtsc on GNU C with x86 */
#if defined TIMING_USE_RDTSC && defined __GNUC__ && defined __i386__

typedef unsigned long long timing_ticks_t;
#define timing_ticks(t)              __asm__ __volatile__ ("rdtsc" : "=A" (t))
#define timing_ticks_init(t)         ((t) = 0)
#define timing_ticks_cmp(a, b, cmp)  ((a) cmp (b))
#define timing_ticks_sub(r, a)       ((r) = (r) - (a))
#define timing_ticks_add(r, a)       ((r) = (r) + (a))
#define timing_ticks_ulong(t)        ((unsigned long) (t))
#define timing_ticks_dbl(t)          ((double) (t))

#else

typedef struct timeval timing_ticks_t;
#define timing_ticks(t)              (gettimeofday(&(t), NULL))
#define timing_ticks_init(t)         ((t).tv_sec = 0, (t).tv_usec = 0)

/*
 * This shamelessly stolen and modified from glibc's
 * /usr/include/sys/time.h
 */
#define timing_ticks_cmp(a, b, CMP)   \
  (((a).tv_sec == (b).tv_sec) ? 	  \
   ((a).tv_usec CMP (b).tv_usec) :    \
   ((a).tv_sec CMP (b).tv_sec))

#define timing_ticks_add(r, a)                       \
	do {							                 \
		(r).tv_sec = (r).tv_sec + (a).tv_sec;        \
		(r).tv_usec = (r).tv_usec + (a).tv_usec;     \
		if ((r).tv_usec >= 1000000) {			     \
			++(r).tv_sec;                            \
			(r).tv_usec -= 1000000;                  \
		}									         \
	} while (0)

#define timing_ticks_sub(r, a)                        \
	do {									          \
		(r).tv_sec = (r).tv_sec - (a).tv_sec;	      \
		(r).tv_usec = (r).tv_usec - (a).tv_usec;      \
		if ((r).tv_usec < 0) {					      \
			--(r).tv_sec;						      \
			(r).tv_usec += 1000000;					  \
		}									          \
	} while (0)

#define timing_ticks_ulong(t)        ((unsigned long) ((t).tv_usec + 1000000 * (t).tv_sec))
#define timing_ticks_dbl(t)          (((t).tv_usec + 1000000.0 * (t).tv_sec))

#endif /* TIMING_USE_RDTSC ... */

/**
 * Set the current schedule parameters.
 * @return 1, if succeeded, 0 if not (see errno, for details).
 */
int timing_sched_set(const timing_sched_env_t *env);

/**
 * Get the schedule parameters.
 * @return 1, if succeeded, 0 if not (see errno, for details).
 */
timing_sched_env_t *timing_sched_get(timing_sched_env_t *env);

/**
 * Prepare schedule parameters which limit the process on one CPU
 * and set the maximum task priority.
 * @return The paramter @p env.
 */
timing_sched_env_t *timing_sched_prepare_max_prio(timing_sched_env_t *env);

#endif /* _TICKS_H */
