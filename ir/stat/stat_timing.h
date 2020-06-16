/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   OS abstraction from time measurement
 * @author  Sebastian Hack, Michael Beck, Matthias Braun
 */
#ifndef FIRM_STAT_TIMING_H
#define FIRM_STAT_TIMING_H

#include <stddef.h>

#if defined(__linux__) || defined(__APPLE__)
#   include <sys/time.h>
#elif defined(_WIN32)
#   include <windows.h>
#   include <time.h>

static int gettimeofday(struct timeval *tv, struct timezone *tz)
{
    LARGE_INTEGER tick;
    LARGE_INTEGER tick_per_second;

    time_t t = 0;
    time(&t);
    tv->tv_sec = (long) t;

    QueryPerformanceFrequency(&tick_per_second);
    QueryPerformanceCounter(&tick);

    tv->tv_usec = (tick.QuadPart % tick_per_second.QuadPart);
    return 0;
}
#endif

typedef unsigned long long timing_ticks_t;

/**
 * returns time in micro seconds.
 * The time is relative to an unspecified start, so it can only be used to
 * measure relative time/timespans.
 */
static inline timing_ticks_t timing_ticks(void)
{
#if defined(__i386__) && (defined(__linux__) || defined(__APPLE__))
	unsigned h;
	unsigned l;
	__asm__ volatile("rdtsc" : "=a" (l), "=d" (h));
	return (timing_ticks_t)h << 32 | l;
#else
	struct timeval tval;
	gettimeofday(&tval, NULL);
	return (unsigned long) (tval.tv_usec + 1000000 * tval.tv_sec);
#endif
}

void timing_enter_max_prio(void);
void timing_leave_max_prio(void);

#endif
