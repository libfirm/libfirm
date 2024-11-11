/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   platform neutral timing utilities
 */
#include <stdio.h>
#include <string.h>

#include "timing.h"
#include "xmalloc.h"
#include "panic.h"

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

#include <unistd.h>
#define HAVE_GETTIMEOFDAY

/*
 * Just, if we have gettimeofday()
 * Someday, we will have a check here.
 */
#include <sys/time.h>

/* Linux timer value. */
typedef struct timeval ir_timer_val_t;

#endif /* _Win32 */

static inline void _time_reset(ir_timer_val_t *val);

/**
 * A timer.
 */
struct ir_timer_t {
	ir_timer_val_t elapsed;     /**< the elapsed time so far */
	ir_timer_val_t start;       /**< the start value of the timer */
	ir_timer_t     *parent;     /**< parent of a timer */
	ir_timer_t     *displaced;  /**< former timer in case of timer_push */
	unsigned       running : 1; /**< set if this timer is running */
};

/** The top of the timer stack */
static ir_timer_t *timer_stack;

ir_timer_t *ir_timer_new(void)
{
	ir_timer_t *timer = XMALLOCZ(ir_timer_t);
	_time_reset(&timer->elapsed);
	_time_reset(&timer->start);
	return timer;
}

void ir_timer_free(ir_timer_t *timer)
{
	free(timer);
}

#ifdef HAVE_GETTIMEOFDAY

static inline void _time_get(ir_timer_val_t *val)
{
	gettimeofday(val, NULL);
}

static inline void _time_reset(ir_timer_val_t *val)
{
	val->tv_sec  = 0;
	val->tv_usec = 0;
}

static inline unsigned long _time_to_msec(const ir_timer_val_t *elapsed)
{
	return (unsigned long) elapsed->tv_sec * 1000UL
		+ (unsigned long) elapsed->tv_usec / 1000UL;
}

static inline unsigned long _time_to_usec(const ir_timer_val_t *elapsed)
{
	return (unsigned long) elapsed->tv_sec * 1000000UL
		+ (unsigned long) elapsed->tv_usec;
}

static inline double _time_to_sec(const ir_timer_val_t *elapsed)
{
	return (double)elapsed->tv_sec + (double)elapsed->tv_usec / 1000000.0;
}

static inline ir_timer_val_t *_time_add(ir_timer_val_t *res,
                                        const ir_timer_val_t *lhs,
                                        const ir_timer_val_t *rhs)
{
	res->tv_sec  = lhs->tv_sec + rhs->tv_sec;
	res->tv_usec = lhs->tv_usec + rhs->tv_usec;
	if (res->tv_usec >= 1000000) {
		res->tv_sec++;
		res->tv_usec -= 1000000;
	}
	return res;
}

static inline ir_timer_val_t *_time_sub(ir_timer_val_t *res,
                                        const ir_timer_val_t *lhs,
                                        const ir_timer_val_t *rhs)
{
	res->tv_sec  = lhs->tv_sec - rhs->tv_sec;
	res->tv_usec = lhs->tv_usec - rhs->tv_usec;
	if (res->tv_usec < 0) {
		res->tv_sec--;
		res->tv_usec += 1000000;
	}
	return res;
}

#elif defined(_WIN32)

static inline void _time_get(ir_timer_val_t *val)
{
	if (!QueryPerformanceCounter(&val->hi_prec))
		val->lo_prec = timeGetTime();
}

static inline void _time_reset(ir_timer_val_t *val)
{
	memset(val, 0, sizeof(val[0]));
}

static inline unsigned long _time_to_msec(const ir_timer_val_t *elapsed)
{
	LARGE_INTEGER freq;

	if (!QueryPerformanceFrequency(&freq))
		return (unsigned long) elapsed->lo_prec;

	return (unsigned long) ((elapsed->hi_prec.QuadPart * 1000) / freq.QuadPart);
}

static inline unsigned long _time_to_usec(const ir_timer_val_t *elapsed)
{
	LARGE_INTEGER freq;

	if (!QueryPerformanceFrequency(&freq))
		return (unsigned long) elapsed->lo_prec * 1000;

	return (unsigned long) ((elapsed->hi_prec.QuadPart * 1000000) / freq.QuadPart);
}

static inline double _time_to_sec(const ir_timer_val_t *elapsed)
{
	LARGE_INTEGER freq;

	if (!QueryPerformanceFrequency(&freq))
		return (double) elapsed->lo_prec / 1000.;

	return (double)elapsed->hi_prec.QuadPart / (double)freq.QuadPart;
}

static inline ir_timer_val_t *_time_add(ir_timer_val_t *res, const ir_timer_val_t *lhs, const ir_timer_val_t *rhs)
{
	LARGE_INTEGER dummy;
	if (QueryPerformanceFrequency(&dummy))
		res->hi_prec.QuadPart = lhs->hi_prec.QuadPart + rhs->hi_prec.QuadPart;
	else
		res->lo_prec = lhs->lo_prec + rhs->lo_prec;

	return res;
}

static inline ir_timer_val_t *_time_sub(ir_timer_val_t *res, const ir_timer_val_t *lhs, const ir_timer_val_t *rhs)
{
	LARGE_INTEGER dummy;
	if (QueryPerformanceFrequency(&dummy))
		res->hi_prec.QuadPart = lhs->hi_prec.QuadPart - rhs->hi_prec.QuadPart;
	else
		res->lo_prec = lhs->lo_prec - rhs->lo_prec;

	return res;
}

#endif /* _WIN32 */

#if defined(_XOPEN_REALTIME) && _XOPEN_REALTIME != -1

#include <sys/types.h>
#include <sched.h>

static struct sched_param std_sched_param;
static int std_sched_param_init = 0;

int ir_timer_enter_high_priority(void)
{
	pid_t pid = getpid();

	struct sched_param p;
	int res, max, algo;

	if (!std_sched_param_init) {
		res = sched_getparam(pid, &std_sched_param);
		if (res != 0)
			return res;
		std_sched_param_init = 1;
	}

	algo = sched_getscheduler(pid);
	max  = sched_get_priority_max(algo);

	p = std_sched_param;
	p.sched_priority = max;
	res = sched_setparam(pid, &p);

	return res;
}

int ir_timer_leave_high_priority(void)
{
	int res   = 0;
	pid_t pid = getpid();

	if (std_sched_param_init)
		res = sched_setparam(pid, &std_sched_param);

	return res;
}

#elif defined(_WIN32)

static int initial_priority = THREAD_PRIORITY_NORMAL;

int ir_timer_leave_high_priority(void)
{
	int res = 0;
	if (!SetThreadPriority(GetCurrentThread(), initial_priority)) {
		fprintf(stderr, "Failed to leave high priority (%d)\n", GetLastError());
		res = GetLastError();
	}

	return res;
}

int ir_timer_enter_high_priority(void)
{
	int res = 0;
	initial_priority = GetThreadPriority(GetCurrentThread());
	if (!SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST)) {
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
	if (timer->running)
		panic("timer started twice");

	_time_reset(&timer->start);
	_time_get(&timer->start);
	timer->running = 1;

	if (timer->parent == NULL) {
		timer->parent = timer_stack;
	} else if (timer->parent != timer_stack) {
		panic("timer used at different stack positions");
	}
	timer_stack = timer;
}

void ir_timer_reset_and_start(ir_timer_t *timer)
{
  _time_reset(&timer->elapsed);
  ir_timer_start(timer);
}

void ir_timer_stop(ir_timer_t *timer)
{
	if (!timer->running)
		panic("attempting to stop stopped timer");
	if (timer != timer_stack)
		panic("timer stack error");
	timer_stack = timer->parent;

	ir_timer_val_t val;
	ir_timer_val_t tgt;

	_time_get(&val);
	timer->running = 0;
	_time_add(&timer->elapsed, &timer->elapsed, _time_sub(&tgt, &val, &timer->start));
}

void ir_timer_init_parent(ir_timer_t *timer)
{
	if (timer == NULL)
		return;
	if (timer->parent != NULL && timer->parent != timer_stack)
		panic("timer parent mismatch");
	timer->parent = timer_stack;
}

void ir_timer_push(ir_timer_t *timer)
{
	if (timer->running)
		panic("timer started twice");

	ir_timer_t *parent = timer->parent;
	if (timer->parent == NULL)
		panic("pushing timer with unknown parent");

	timer->displaced = timer_stack;
	for (ir_timer_t *t = timer_stack; t != parent; t = t->parent) {
		if (t == NULL)
			panic("parent timer not on stack");
		ir_timer_stop(t);
	}
	timer_stack = parent;

	ir_timer_start(timer);
}

static void start_stack(ir_timer_t *timer, ir_timer_t *stop)
{
	if (timer == stop)
		return;
	start_stack(timer->parent, stop);
	ir_timer_start(timer);
}

void ir_timer_pop(ir_timer_t *timer)
{
	if (!timer->running)
		panic("attempting to stop stopped timer");
	ir_timer_t *displaced = timer->displaced;
	if (displaced == NULL)
		panic("timer start/stop/push/pop mismatch");

	ir_timer_t *parent = timer->parent;
	timer->displaced = NULL;

	ir_timer_stop(timer);
	start_stack(displaced, parent);
}

unsigned long ir_timer_elapsed_msec(const ir_timer_t *timer)
{
	ir_timer_val_t v;
	const ir_timer_val_t *elapsed = &timer->elapsed;

	if (timer->running) {
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

	if (timer->running) {
		elapsed = &v;
		_time_get(&v);
		_time_add(&v, &timer->elapsed, _time_sub(&v, &v, &timer->start));
	}
	return _time_to_usec(elapsed);
}

double ir_timer_elapsed_sec(const ir_timer_t *timer)
{
	ir_timer_val_t v;
	const ir_timer_val_t *elapsed = &timer->elapsed;

	if (timer->running) {
		elapsed = &v;
		_time_get(&v);
		_time_add(&v, &timer->elapsed, _time_sub(&v, &v, &timer->start));
	}
	return _time_to_sec(elapsed);
}
