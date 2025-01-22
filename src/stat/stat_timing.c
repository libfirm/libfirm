/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   OS abstraction from time measurement
 * @author  Sebastian Hack, Michael Beck, Matthias Braun
 */
#include "stat_timing.h"

#include <stdio.h>

#if defined(__linux__) || defined(__FreeBSD__) || defined(__OpenBSD__)

#include <unistd.h>
#include <time.h>
#include <sys/time.h>

/* define GNU macro for processor affinity stuff if on linux */
#if defined __linux__ && !defined __USE_GNU
#define __USE_GNU
#endif
#include <sched.h>

/* we can only use the scheduling stuff, if that macro is defined in unistd.h */
#if defined(_XOPEN_REALTIME) && _XOPEN_REALTIME != -1

#define HAVE_IMPL

static int                 in_max_prio = 0;
static cpu_set_t           affinity;
static int                 scheduler;
static struct sched_param  sched_params;

void timing_enter_max_prio(void)
{
	int                 res;
	int                 new_scheduler = SCHED_FIFO;
	struct sched_param  new_sched_params;
	cpu_set_t           new_affinity;

	if (in_max_prio)
		return;

	/* remember old scheduler settings */
	res = sched_getaffinity(0, sizeof(affinity), &affinity);
	if (res < 0)
		return;
	scheduler = sched_getscheduler(0);
	if (scheduler < 0)
		return;
	res = sched_getparam(0, &sched_params);
	if (res < 0)
		return;

	/* set high prio */
	CPU_ZERO(&new_affinity);
	CPU_SET(0, &new_affinity);
	res = sched_setaffinity(0, sizeof(new_affinity), &new_affinity);
	if (res < 0)
		return;
	new_scheduler = SCHED_FIFO;
	new_sched_params = sched_params;
	new_sched_params.sched_priority = sched_get_priority_max(new_scheduler);
	res = sched_setscheduler(0, new_scheduler, &new_sched_params);
	if (res < 0)
		return;

	in_max_prio = 1;
}

void timing_leave_max_prio(void)
{
	int res;

	if (!in_max_prio)
		return;

	/* restore old settings */
	res = sched_setaffinity(0, sizeof(affinity), &affinity);
	if (res < 0)
		return;

	res = sched_setscheduler(0, scheduler, &sched_params);
	if (res < 0)
		return;

	in_max_prio = 0;
}

#endif
#endif


#ifndef HAVE_IMPL

/* dummy implementation */

void timing_enter_max_prio(void)
{
}

void timing_leave_max_prio(void)
{
}

#endif
