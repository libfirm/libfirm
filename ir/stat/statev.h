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

#define stat_ev_cnt_decl(var)
#define stat_ev_cnt_inc(var)
#define stat_ev_cnt_done(name, var)
#define stat_ev(name)

#define stat_ev_ctx_push_fobj(key, firm_object)
#define stat_ev_ctx_push(key, value)
#define stat_ev_ctx_pop()
#define stat_ev_begin(prefix)
#define stat_ev_end()

#else

extern int stat_ev_enabled;

#define stat_ev_do(expr)            (stat_ev_enabled ? ((expr), 1) : 0)

#define stat_ev_dbl(name, val)      do { if (stat_ev_enabled) { stat_ev_emit(name, val); } } while(0)

#define stat_ev_cnt_decl(var)       int stat_ev_cnt_var_ ## var = 0
#define stat_ev_cnt_inc(var)        do { ++stat_ev_cnt_var_ ## var; } while(0)
#define stat_ev_cnt_done(var, name) stat_ev_dbl((name), stat_ev_cnt_var_ ## var)

#define stat_ev(name)               stat_ev_dbl((name), 0.0)

void stat_ev_ctx_push_fobj(const char *key, const void *firm_object);
void stat_ev_ctx_push(const char *key, const char *value);
void stat_ev_ctx_pop(void);
void stat_ev_emit(const char *name, double val);
void stat_ev_begin(const char *prefix);
void stat_ev_end(void);

#endif

#endif /* FIRM_STATEVENT_H */
