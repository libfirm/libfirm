/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    platform neutral timing utilities
 */
#ifndef FIRM_TIMING_H
#define FIRM_TIMING_H

#include "begin.h"

/**
 * A timer
 *
 * A timer can be started/stopped multiple times and measures the (wallclock)
 * time spent between start and stop calls.
 */
typedef struct ir_timer_t ir_timer_t;

/**
 * Switch to real-time scheduling.
 * This shall make measurements more precise.
 * @note Does not work for all operating systems.
 * @note You could need special user privileges.
 * @return 0 on success, else UNIX error code.
 */
FIRM_API int ir_timer_enter_high_priority(void);

/**
 * Leave the high priority mode.
 * @see ir_timer_enter_high_priority()
 * @return 0 on success, else UNIX error code.
 */
FIRM_API int ir_timer_leave_high_priority(void);

/**
 * Create a new timer
 * @return The timer.
 * @see #ir_timer_t
 */
FIRM_API ir_timer_t *ir_timer_new(void);

/**
 * free memory occupied by a timer
 * @param timer The timer
 */
FIRM_API void ir_timer_free(ir_timer_t *timer);

/**
 * Start a timer.
 * @param timer The timer.
 */
FIRM_API void ir_timer_start(ir_timer_t *timer);

/**
 * Reset a timer and start it.
 * @param timer The timer.
 */
FIRM_API void ir_timer_reset_and_start(ir_timer_t *timer);

/**
 * Reset a timer.
 * @param timer The timer.
 */
FIRM_API void ir_timer_reset(ir_timer_t *timer);

/**
 * Stop a timer.
 * Stopping a stopped timer has no effect.
 * @param timer The timer.
 */
FIRM_API void ir_timer_stop(ir_timer_t *timer);

/**
 * Set currently running timer as parent to @p timer
 */
FIRM_API void ir_timer_init_parent(ir_timer_t *timer);

/**
 * Push a timer of the timer stack. This automatically
 * stop the previous timer on tos and start the new one.
 *
 * @param timer   The timer to push on stack.
 * @return non-zero on succes, zero if the timer is already on the stack.
 */
FIRM_API void ir_timer_push(ir_timer_t *timer);

/**
 * Pop the current timer. This automatically stops it and
 * start the timer that is now on the stack.
 * @return the popped timer
 */
FIRM_API void ir_timer_pop(ir_timer_t *timer);

/**
 * Returns the number of milliseconds, the timer has elapsed.
 * @param timer The timer.
 * @return The number of milliseconds the timer is (was) running.
 */
FIRM_API unsigned long ir_timer_elapsed_msec(const ir_timer_t *timer);

/**
 * Returns the number of microseconds, the timer has elapsed.
 * @param timer The timer.
 * @return The number of milliseconds the timer is (was) running.
 */
FIRM_API unsigned long ir_timer_elapsed_usec(const ir_timer_t *timer);

/**
 * Returns the number of seconds, the timer has elapsed.
 */
FIRM_API double ir_timer_elapsed_sec(const ir_timer_t *timer);

#include "end.h"

#endif
