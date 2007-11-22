/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Statistic events.
 * @author      Sebastian Hack
 * @date        17.06.2007
 * @version     $Id$
 */

#ifndef FIRM_STATEVENT_H
#define FIRM_STATEVENT_H

#ifndef FIRM_STATISTICS

#define stat_ev_do(expr)
#define stat_ev_if                  if (0)
#define stat_ev_dbl(name, val)
#define stat_ev_int(name, val)
#define stat_ev(name)

#define stat_ev_cnt_decl(var)
#define stat_ev_cnt_inc(var)
#define stat_ev_cnt_done(name, var)
#define stat_ev_tim_push()
#define stat_ev_tim_pop(name)

#define stat_ev_ctx_push(key)
#define stat_ev_ctx_push_str(key, str)
#define stat_ev_ctx_push_fmt(key, fmt, value)
#define stat_ev_ctx_push_fobj(key, firm_object)
#define stat_ev_ctx_pop(key)
#define stat_ev_flush()

#else

#include <stdio.h>
#include "timing.h"

extern void               stat_ev_printf(char ev_type, const char *key, const char *fmt, ...);

extern int                stat_ev_enabled;
extern int                stat_ev_timer_sp;
extern timing_ticks_t     stat_ev_timer_elapsed[];
extern timing_ticks_t     stat_ev_timer_start[];
extern timing_sched_env_t stat_ev_sched_rt;
extern timing_sched_env_t stat_ev_sched_normal;

static INLINE __attribute__((unused)) void stat_ev_tim_push(void) {
	timing_ticks_t temp;
	int sp = stat_ev_timer_sp++;
	timing_ticks(temp);
	if (sp == 0)
		timing_sched_set(&stat_ev_sched_rt);
	else {
		timing_ticks_sub(temp, stat_ev_timer_start[sp - 1]);
		timing_ticks_add(stat_ev_timer_elapsed[sp - 1], temp);
	}
	timing_ticks_init(stat_ev_timer_elapsed[sp]);
	timing_ticks(stat_ev_timer_start[sp]);
}

static INLINE __attribute__((unused)) void stat_ev_tim_pop(const char *name) {
	int sp;
	timing_ticks_t temp;
	timing_ticks(temp);
	sp = --stat_ev_timer_sp;
	timing_ticks_sub(temp, stat_ev_timer_start[sp]);
	timing_ticks_add(stat_ev_timer_elapsed[sp], temp);
	if (name != NULL && stat_ev_enabled)
		stat_ev_printf('E', name, "%g", timing_ticks_dbl(stat_ev_timer_elapsed[sp]));
	if (sp == 0)
		timing_sched_set(&stat_ev_sched_normal);
	else
		timing_ticks(stat_ev_timer_start[sp - 1]);
}

#define stat_ev_ctx_push_fmt(key, fmt, value) \
	do { \
		if (stat_ev_enabled) { \
			stat_ev_tim_push(); \
			stat_ev_printf('P', key, fmt, (value)); \
			stat_ev_tim_pop(NULL); \
		} \
	} while(0)

#define stat_ev_ctx_pop(key) \
	do { \
		if (stat_ev_enabled) { \
			stat_ev_tim_push(); \
			stat_ev_printf('O', key, NULL); \
			stat_ev_tim_pop(NULL); \
		} \
	} while(0)

#define stat_ev_emit(name, value) \
	do { \
		if (stat_ev_enabled) { \
			stat_ev_tim_push(); \
			stat_ev_printf('E', name, "%g", (double) (value)); \
			stat_ev_tim_pop(NULL); \
		} \
	} while(0)

#define stat_ev_ctx_push_fobj(key, firm_object) stat_ev_ctx_push_fmt((key), "%+F", (firm_object))
#define stat_ev_ctx_push_str(key, str)          stat_ev_ctx_push_fmt((key), "%s", (str))
#define stat_ev_ctx_push(key)                   stat_ev_ctx_push_fmt((key), "X", NULL)

#define stat_ev_dbl(name, val)      stat_ev_emit((name), (val))
#define stat_ev_int(name, val)      stat_ev_dbl((name), (double) (val))
#define stat_ev(name)               stat_ev_emit((name), 0.0)

#define stat_ev_cnt_decl(var)       int stat_ev_cnt_var_ ## var = 0
#define stat_ev_cnt_inc(var)        do { ++stat_ev_cnt_var_ ## var; } while(0)
#define stat_ev_cnt_done(var, name) stat_ev_emit((name), stat_ev_cnt_var_ ## var)

#define stat_ev_do(expr)            (stat_ev_enabled ? ((expr), 1) : 0)
#define stat_ev_if                  if (stat_ev_enabled)

/**
 * Initialize the stat ev machinery.
 * @param filename_prefix  The prefix of the filename (.ev or .ev.gz will be appended).
 * @param filter           All pushes, pops and events will be filtered by this.
 *                         If we have regex support, you can give an extended regex here.
 *                         If not, each key will be matched against this.
 *                         Matched means, we look if the key starts with @p filter.
 *                         If NULL is given, each key passes, ie thefilter is always TRUE.
 */
void stat_ev_begin(const char *filename_prefix, const char *filter);
void stat_ev_end(void);

#define stat_ev_flush()             do { if (stat_ev_enabled) fflush(stat_ev_enabled); } while(0)

#endif

#endif /* FIRM_STATEVENT_H */
