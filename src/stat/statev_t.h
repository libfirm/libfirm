/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Statistic events.
 * @author      Sebastian Hack
 */
#ifndef FIRM_STATEVENT_T_H
#define FIRM_STATEVENT_T_H

#include "statev.h"
#include <stdarg.h>

#ifdef DISABLE_STATEV

#define stat_ev_enabled                          0
#define stat_ev_dbl(name, val)                   ((void)0)
#define stat_ev_int(name, val)                   ((void)0)
#define stat_ev_ull(name, val)                   ((void)0)
#define stat_ev(name)                            ((void)0)

#define stat_ev_cnt_decl(var)                    ((void)0)
#define stat_ev_cnt_inc(var)                     ((void)0)
#define stat_ev_cnt_done(name, var)              ((void)0)
#define stat_ev_tim_push()                       ((void)0)
#define stat_ev_tim_pop(name)                    ((void)0)

#define stat_ev_ctx_push(key)                    ((void)0)
#define stat_ev_ctx_push_str(key, str)           ((void)0)
#define stat_ev_ctx_push_fmt(key, fmt, value)    ((void)0)
#define stat_ev_ctx_pop(key)                     ((void)0)

#else

void stat_ev_tim_push(void);
void stat_ev_tim_pop(const char *name);

void do_stat_ev_int(const char *name, int value);
void do_stat_ev_dbl(const char *name, double value);
void do_stat_ev_ull(const char *name, unsigned long long value);
void do_stat_ev(const char *name);
void do_stat_ev_ctx_push_vfmt(const char *name, const char *fmt, va_list ap);
void do_stat_ev_ctx_pop(const char *key);

static inline void stat_ev_int_(const char *name, int value)
{
	if (!stat_ev_enabled)
		return;
	(do_stat_ev_int)(name, value);
}
static inline void stat_ev_dbl_(const char *name, double value)
{
	if (!stat_ev_enabled)
		return;
	(do_stat_ev_dbl)(name, value);
}
static inline void stat_ev_ull_(const char *name, unsigned long long value)
{
	if (!stat_ev_enabled)
		return;
	(do_stat_ev_ull)(name, value);
}
static inline void stat_ev_(const char *name)
{
	if (!stat_ev_enabled)
		return;
	(do_stat_ev)(name);
}
static inline void stat_ev_ctx_push_fmt_(const char *name, const char *fmt, ...)
{
	if (!stat_ev_enabled)
		return;
	va_list ap;
	va_start(ap, fmt);
	do_stat_ev_ctx_push_vfmt(name, fmt, ap);
	va_end(ap);
}
static inline void stat_ev_ctx_push_str_(const char *name, const char *str)
{
	stat_ev_ctx_push_fmt_(name, "%s", str);
}
static inline void stat_ev_ctx_pop_(const char *key)
{
	if (!stat_ev_enabled)
		return;
	do_stat_ev_ctx_pop(key);
}
#define stat_ev_int(name, value)        stat_ev_int_(name, value)
#define stat_ev_dbl(name, value)        stat_ev_dbl_(name, value)
#define stat_ev_ull(name, value)        stat_ev_ull_(name, value)
#define stat_ev(name)                   stat_ev_(name)
#define stat_ev_ctx_push_fmt(name, fmt, value) \
                                        stat_ev_ctx_push_fmt_(name, fmt, value)
#define stat_ev_ctx_push_str(name, str) stat_ev_ctx_push_str_(name, str)
#define stat_ev_ctx_pop(name)           stat_ev_ctx_pop_(name)

#define stat_ev_cnt_decl(var)       int stat_ev_cnt_var_ ## var = 0
#define stat_ev_cnt_inc(var)        do { ++stat_ev_cnt_var_ ## var; } while(0)
#define stat_ev_cnt_done(var, name) stat_ev_int((name), stat_ev_cnt_var_ ## var)

#endif

#endif
