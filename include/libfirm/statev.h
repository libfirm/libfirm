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
 * @brief       Statistic events.
 * @author      Sebastian Hack
 */
#ifndef FIRM_STATEVENT_H
#define FIRM_STATEVENT_H

/**
 * @defgroup statev Statistic Events
 *
 * The statistics system helps to evaluate the effects of compiler passes and
 * transformations. It is typically used to collect information like the number
 * of instruction before and behind a pass, counting specific patterns or
 * measuring the timing of subcomponents. This can create quite a bit of data,
 * so we provide a framework for producing such data in an organzied way and
 * some external tools to analyse such data.
 *
 * The system is based on 2 concepts: Events and Contexts. A statistic-event is
 * a pair of an event name and a double value. Events are put into a context
 * like the compilation unit, the currently compiled function or the currently
 * allocated register class. So contexts refine events and allow a grouping of
 * events based on their context later. The context is managed in an
 * hierarchic manner. You can push key/value pairs to refine the previous
 * context or pop them again to get back to the previous broader context.
 *
 * @{
 */

/** Pushes a new setting on the context stack. */
FIRM_API void stat_ev_ctx_push_fmt(const char *key, const char *fmt, ...);
/** Pushes a new setting with a string value on the context stack. */
FIRM_API void stat_ev_ctx_push_str(const char *key, const char *str);
/** Pops last setting from context stack. */
FIRM_API void stat_ev_ctx_pop(const char *key);
/** Emits a statistic event with a double value. */
FIRM_API void stat_ev_dbl(const char *name, double value);
/** Emits a statistic event with an integer value. */
FIRM_API void stat_ev_int(const char *name, int value);
/** Emits a statistic event with an unsigned long long value. */
FIRM_API void stat_ev_ull(const char *name, unsigned long long value);
/** Emits a statistic event (without an additional value). */
FIRM_API void stat_ev(const char *name);

/**
 * Initialize the stat ev machinery.
 * @param filename_prefix  The prefix of the filename (.ev or .ev.gz will be
 *                         appended).
 * @param filter           All pushes, pops and events will be filtered by this.
 *                         If we have regex support, you can give an extended
 *                         regex here. If not, each key will be matched against
 *                         this. Matched means, we look if the key starts with
 *                         @p filter. If NULL is given, each key passes, ie
 *                         the filter is always TRUE.
 */
FIRM_API void stat_ev_begin(const char *filename_prefix, const char *filter);

/**
 * Shuts down stat ev machinery
 */
FIRM_API void stat_ev_end(void);

/**
 * This variable indicates whether statev output is enabled.
 */
FIRM_API int stat_ev_enabled;

/** @} */

#endif
