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
 * @brief    platform neutral timing utilities
 * @version  $Id: tv.h 17143 2008-01-02 20:56:33Z beck $
 */
#ifndef FIRM_TIMING_H
#define FIRM_TIMING_H

#include <stdlib.h>

typedef struct _ir_timer_t ir_timer_t;

/**
 * Switch to real-time scheduling.
 * This shall make measurements more precise.
 * @note Does not work for all operating systems.
 * @note You could need special user privileges.
 * @return 0 on success, else UNIX error code.
 */
int ir_timer_enter_high_priority(void);

/**
 * Leave the high priority mode.
 * @see ir_timer_enter_high_priority()
 * @return 0 on success, else UNIX error code.
 */
int ir_timer_leave_high_priority(void);

/**
 * Get the amount of bytes allocated on the heap.
 * @return The number of bytes allocated on the heap.
 */
size_t ir_get_heap_used_bytes(void);

/**
 * Register a new timer.
 * If the timer was registered before, the registered timer is returned.
 * @param name  The name of the timer.
 * @param desc  The description of the timer.
 * @return The timer.
 */
ir_timer_t *ir_timer_register(const char *name, const char *desc);

/**
 * Start a timer.
 * @param timer The timer.
 */
void ir_timer_start(ir_timer_t *timer);

/**
 * Reset a timer and start it.
 * @param timer The timer.
 */
void ir_timer_reset_and_start(ir_timer_t *timer);

/**
 * Reset a timer.
 * @param timer The timer.
 */
void ir_timer_reset(ir_timer_t *timer);

/**
 * Stop a timer.
 * Stopping a stopped timer has no effect.
 * @param timer The timer.
 */
void ir_timer_stop(ir_timer_t *timer);

/**
 * Push a timer of the timer stack. This automatically
 * stop the previous timer on tos and start the new one.
 *
 * @param timer   The timer to push on stack.
 * @return non-zero on succes, zero if the timer is already on the stack.
 */
int ir_timer_push(ir_timer_t *timer);

/**
 * Pop the current timer. This automatically stops it and
 * start the timer that is now on the stack.
 * @return the popped timer
 */
ir_timer_t *ir_timer_pop(void);

/**
 * Get the number of milliseconds, the timer has elapsed.
 * @param timer The timer.
 * @return The number of milliseconds the timer is (was) running.
 */
unsigned long ir_timer_elapsed_msec(const ir_timer_t *timer);

/**
 * Get the number of microseconds, the timer has elapsed.
 * @param timer The timer.
 * @return The number of milliseconds the timer is (was) running.
 */
unsigned long ir_timer_elapsed_usec(const ir_timer_t *timer);

/**
 * Get name of given timer.
 * @param timer The timer.
 * @return The name of the timer.
 */
const char *ir_timer_get_name(const ir_timer_t *timer);

/**
 * Get description of given timer.
 * @param timer The timer.
 * @return The description of the timer.
 */
const char *ir_timer_get_description(const ir_timer_t *timer);

#endif
