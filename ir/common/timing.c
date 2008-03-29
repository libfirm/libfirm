/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   platform neutral timing utilities
 * @version $Id: debug.c 17143 2008-01-02 20:56:33Z beck $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "firm_config.h"

#include <stdio.h>
#include <string.h>

#ifndef _WIN32
#include <unistd.h>
#endif

#include "timing.h"
#include "set.h"
#include "hashptr.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <mmsystem.h>

/* Win32 timer value. */
typedef union {
	unsigned lo_prec;       /**< 32bit low precision time in milli seconds */
	LARGE_INTEGER hi_prec;  /**< 64bit high precision time in micro seconds */
} ir_timer_val_t;

#else

#define HAVE_GETTIMEOFDAY

/*
 * Just, if we have gettimeofday()
 * Someday, we will have a check here.
 */
#ifndef __USE_BSD
#define __USE_BSD
#endif
#include <sys/time.h>

/* Linux timer value. */
typedef struct timeval ir_timer_val_t;

#endif /* _Win32 */

#include <stddef.h>

static INLINE void _time_get(ir_timer_val_t *val);
static INLINE void _time_reset(ir_timer_val_t *val);
static INLINE unsigned long _time_to_msec(const ir_timer_val_t *val);
static INLINE ir_timer_val_t *_time_add(ir_timer_val_t *res,
		const ir_timer_val_t *lhs, const ir_timer_val_t *rhs);
static INLINE ir_timer_val_t *_time_sub(ir_timer_val_t *res,
		const ir_timer_val_t *lhs, const ir_timer_val_t *rhs);

/**
 * A timer.
 */
struct _ir_timer_t {
	ir_timer_val_t elapsed;     /**< the elapsed time so far */
	ir_timer_val_t start;       /**< the start value of the timer */
	ir_timer_t     *link;       /**< link to the next entry in the timer stack */
	const char     *name;       /**< the name of the timer used for identification */
	const char     *desc;       /**< a description if the timer */
	unsigned       running : 1; /**< set if this timer is running */
};

/**
 * Compare two timers.
 */
static int ir_timer_cmp(const void *a, const void *b, size_t size)
{
	const ir_timer_t *t1 = a;
	const ir_timer_t *t2 = b;
	(void) size;
	return strcmp(t1->name, t2->name);
}

/** The set containing all currently registered timers. */
static set *timers = NULL;

/** The top of the timer stack */
static ir_timer_t *timer_stack;

/** Initialize the timer module. */
static void timing_init(void)
{
	timers      = new_set(ir_timer_cmp, 16);
	timer_stack = NULL;
}

ir_timer_t *ir_timer_register(const char *name, const char *desc)
{
	unsigned hash = HASH_STR(name, strlen(name));
	ir_timer_t timer;

	_time_reset(&timer.elapsed);
	_time_reset(&timer.start);
	timer.link = NULL;
	timer.name = name;
    timer.desc = desc;
	timer.running = 0;

	if (!timers)
		timing_init();

	return set_insert(timers, &timer, sizeof(timer), hash);
}

#ifdef HAVE_GETTIMEOFDAY

static INLINE void _time_get(ir_timer_val_t *val)
{
	gettimeofday(val, NULL);
}

static INLINE void _time_reset(ir_timer_val_t *val)
{
	timerclear(val);
}

static INLINE unsigned long _time_to_msec(const ir_timer_val_t *elapsed)
{
	return (unsigned long) elapsed->tv_sec * 1000UL
		+ (unsigned long) elapsed->tv_usec / 1000UL;
}

static INLINE unsigned long _time_to_usec(const ir_timer_val_t *elapsed)
{
	return (unsigned long) elapsed->tv_sec * 1000000UL
		+ (unsigned long) elapsed->tv_usec;
}

static INLINE ir_timer_val_t *_time_add(ir_timer_val_t *res,
		const ir_timer_val_t *lhs, const ir_timer_val_t *rhs)
{
	timeradd(lhs, rhs, res);
		return res;
}

static INLINE ir_timer_val_t *_time_sub(ir_timer_val_t *res,
		const ir_timer_val_t *lhs, const ir_timer_val_t *rhs)
{
	timersub(lhs, rhs, res);
	return res;
}

#elif defined(_WIN32)

static INLINE void _time_get(ir_timer_val_t *val)
{
	if(!QueryPerformanceCounter(&val->hi_prec))
		val->lo_prec = timeGetTime();
}

static INLINE void _time_reset(ir_timer_val_t *val)
{
	memset(val, 0, sizeof(val[0]));
}

static INLINE unsigned long _time_to_msec(const ir_timer_val_t *elapsed)
{
	LARGE_INTEGER freq;

	if(!QueryPerformanceFrequency(&freq))
		return (unsigned long) elapsed->lo_prec;

	return (unsigned long) ((elapsed->hi_prec.QuadPart * 1000) / freq.QuadPart);
}

static INLINE unsigned long _time_to_usec(const ir_timer_val_t *elapsed)
{
	LARGE_INTEGER freq;

	if(!QueryPerformanceFrequency(&freq))
		return (unsigned long) elapsed->lo_prec;

	return (unsigned long) ((elapsed->hi_prec.QuadPart * 1000000) / freq.QuadPart);
}

static INLINE ir_timer_val_t *_time_add(ir_timer_val_t *res, const ir_timer_val_t *lhs, const ir_timer_val_t *rhs)
{
	LARGE_INTEGER dummy;
	if(QueryPerformanceFrequency(&dummy))
		res->hi_prec.QuadPart = lhs->hi_prec.QuadPart + rhs->hi_prec.QuadPart;
	else
		res->lo_prec = lhs->lo_prec + rhs->lo_prec;

	return res;
}

static INLINE ir_timer_val_t *_time_sub(ir_timer_val_t *res, const ir_timer_val_t *lhs, const ir_timer_val_t *rhs)
{
	LARGE_INTEGER dummy;
	if(QueryPerformanceFrequency(&dummy))
		res->hi_prec.QuadPart = lhs->hi_prec.QuadPart - rhs->hi_prec.QuadPart;
	else
		res->lo_prec = lhs->lo_prec - rhs->lo_prec;

	return res;
}

#endif /* _WIN32 */

#if _XOPEN_REALTIME != -1

#include <sys/types.h>
#include <sched.h>

static struct sched_param std_sched_param;
static int std_sched_param_init = 0;

int ir_timer_enter_high_priority(void)
{
	pid_t pid = getpid();

	struct sched_param p;
	int res, max, algo;

	if(!std_sched_param_init) {
		res = sched_getparam(pid, &std_sched_param);
		std_sched_param_init = 1;
	}

	algo = sched_getscheduler(pid);
	max  = sched_get_priority_max(algo);

	memcpy(&p, &std_sched_param, sizeof(p));
	p.sched_priority = max;
	res = sched_setparam(pid, &p);

	return res;
}

int ir_timer_leave_high_priority(void)
{
	int res   = 0;
	pid_t pid = getpid();

	if(std_sched_param_init)
		res = sched_setparam(pid, &std_sched_param);

	return res;
}

#elif defined(_WIN32)

static int initial_priority = THREAD_PRIORITY_NORMAL;

int ir_timer_leave_high_priority(void)
{
	int res = 0;
	if(!SetThreadPriority(GetCurrentThread(), initial_priority)) {
		fprintf(stderr, "Failed to leave high priority (%d)\n", GetLastError());
		res = GetLastError();
	}

	return res;
}

int ir_timer_enter_high_priority(void)
{
	int res = 0;
	initial_priority = GetThreadPriority(GetCurrentThread());
	if(!SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST)) {
		fprintf(stderr, "Failed to enter high priority (%d)\n", GetLastError());
		res = GetLastError();
	}

	return res;
}


#else

int ir_timer_enter_high_priority(void)
{
	fprintf(stderr, "POSIX scheduling API not present\n");
	return 0;
}

int ir_timer_leave_high_priority(void)
{
	return 0;
}

#endif


#ifdef __linux__

#include <malloc.h>
size_t ir_get_heap_used_bytes(void)
{
	struct mallinfo mi = mallinfo();
	return mi.uordblks;
}

#elif defined(_WIN32) /* __linux__ */

#include <malloc.h>

size_t ir_get_heap_used_bytes(void)
{
	_HEAPINFO hinfo;
	int heapstatus;
	size_t res = 0;
	hinfo._pentry = NULL;
	while((heapstatus = _heapwalk(&hinfo)) == _HEAPOK)
		res += hinfo._useflag == _USEDENTRY ? hinfo._size : 0;
	return res;
}

#else

size_t ir_get_heap_used_bytes(void)
{
	fprintf(stderr, "function not implemented\n");
	return 0;
}

#endif

/* reset a timer */
void ir_timer_reset(ir_timer_t *timer)
{
	_time_reset(&timer->elapsed);
	_time_reset(&timer->start);
	timer->running = 0;
}

/* start a timer */
void ir_timer_start(ir_timer_t *timer)
{
	_time_reset(&timer->start);
	_time_get(&timer->start);
	timer->running = 1;
}

void ir_timer_reset_and_start(ir_timer_t *timer)
{
  _time_reset(&timer->elapsed);
  ir_timer_start(timer);
}

/* stop a running timer */
void ir_timer_stop(ir_timer_t *timer)
{
	/* If the timer is running stop, measure the time and add it to the
	 * elapsed time. */
	if(timer->running) {
		ir_timer_val_t val;
		ir_timer_val_t tgt;

		_time_get(&val);
		timer->running = 0;
		_time_add(&timer->elapsed, &timer->elapsed, _time_sub(&tgt, &val, &timer->start));
		_time_reset(&timer->start);
	}
}

/* push a timer on the stack */
int ir_timer_push(ir_timer_t *timer)
{
	if (timer->link)
		return 0;
	timer->link = timer_stack;
	if (timer_stack)
		ir_timer_stop(timer_stack);
	ir_timer_start(timer);
	timer_stack = timer;
	return 1;
}

/* pop a timer from the stack */
ir_timer_t *ir_timer_pop(void)
{
	ir_timer_t *timer = timer_stack;
	if (timer) {
		ir_timer_stop(timer);
		timer_stack = timer->link;
		timer->link = NULL;
		if (timer_stack)
			ir_timer_start(timer_stack);
	}
	return timer;
}

unsigned long ir_timer_elapsed_msec(const ir_timer_t *timer)
{
	ir_timer_val_t v;
	const ir_timer_val_t *elapsed = &timer->elapsed;

	if(timer->running) {
		elapsed = &v;
		_time_get(&v);
		_time_add(&v, &timer->elapsed, _time_sub(&v, &v, &timer->start));
	}
	return _time_to_msec(elapsed);
}

unsigned long ir_timer_elapsed_usec(const ir_timer_t *timer)
{
	ir_timer_val_t v;
	const ir_timer_val_t *elapsed = &timer->elapsed;

	if(timer->running) {
		elapsed = &v;
		_time_get(&v);
		_time_add(&v, &timer->elapsed, _time_sub(&v, &v, &timer->start));
	}
	return _time_to_usec(elapsed);
}

const char *ir_timer_get_name(const ir_timer_t *timer) {
	return timer->name;
}

const char *ir_timer_get_description(const ir_timer_t *timer) {
	return timer->desc;
}
