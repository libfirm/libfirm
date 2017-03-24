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

#ifdef __linux__
# include <sys/time.h>
#elif defined(_WIN32)
# include <windows.h>
# include <time.h>
int gettimeofday(struct timeval *tv, struct timezone *tz)
{
	time_t rawtime;
	LARGE_INTEGER tickPerSecond;
	LARGE_INTEGER tick;

	time(&rawtime);
	tv->tv_sec = (long)rawtime;
	QueryPerformanceFrequency(&tickPerSecond);
	QueryPerformanceCounter(&tick);
	tv->tv_usec = (tick.QuadPart % tickPerSecond.QuadPart);
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
#if defined(__i386__) && defined(__GNUC__)
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
