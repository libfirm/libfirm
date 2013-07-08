/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Statistic events.
 * @author      Sebastian Hack
 * @date        17.06.2007
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <regex.h>
#include <assert.h>

#include "stat_timing.h"
#include "irprintf.h"
#include "statev_t.h"
#include "util.h"

#define MAX_TIMER 256

int (stat_ev_enabled) = 0;

static FILE          *stat_ev_file     = NULL;
static int            stat_ev_timer_sp = 0;
static timing_ticks_t stat_ev_timer_elapsed[MAX_TIMER];
static timing_ticks_t stat_ev_timer_start[MAX_TIMER];

static regex_t  regex;
static regex_t *filter = NULL;
static inline int key_matches(const char *key)
{
	if (!filter)
		return 1;

	return regexec(filter, key, 0, NULL, 0) == 0;
}

static void stat_ev_vprintf(char ev, const char *key, const char *fmt, va_list ap)
{
	if (!key_matches(key))
		return;

	fprintf(stat_ev_file, "%c;%s", ev, key);
	if (fmt != NULL) {
		char buf[256];

		ir_vsnprintf(buf, sizeof(buf), fmt, ap);
		fprintf(stat_ev_file, ";%s", buf);
	}
	fprintf(stat_ev_file, "\n");
}

static void stat_ev_printf(char ev, const char *key, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	stat_ev_vprintf(ev, key, fmt, ap);
	va_end(ap);
}

void stat_ev_tim_push(void)
{
	int            sp   = stat_ev_timer_sp++;
	assert((size_t)sp < ARRAY_SIZE(stat_ev_timer_start));
	timing_ticks_t temp = timing_ticks();
	stat_ev_timer_elapsed[sp] = 0;
	stat_ev_timer_start[sp]   = temp;
	if (sp == 0) {
		timing_enter_max_prio();
	} else {
		temp -= stat_ev_timer_start[sp-1];
		stat_ev_timer_elapsed[sp-1] += temp;
	}
}

void stat_ev_tim_pop(const char *name)
{
	int sp = --stat_ev_timer_sp;
	assert(sp >= 0);
	timing_ticks_t temp = timing_ticks();
	temp -= stat_ev_timer_start[sp];
	stat_ev_timer_elapsed[sp] += temp;
	if (name != NULL && stat_ev_enabled)
		stat_ev_ull(name, stat_ev_timer_elapsed[sp]);

	if (sp == 0) {
		timing_leave_max_prio();
	} else {
		stat_ev_timer_start[sp-1] = timing_ticks();
	}
}

void do_stat_ev_ctx_push_vfmt(const char *key, const char *fmt, va_list ap)
{
	stat_ev_tim_push();
	stat_ev_vprintf('P', key, fmt, ap);
	stat_ev_tim_pop(NULL);
}

void (stat_ev_ctx_push_fmt)(const char *key, const char *fmt, ...)
{
	if (!stat_ev_enabled)
		return;

	va_list ap;
	va_start(ap, fmt);
	do_stat_ev_ctx_push_vfmt(key, fmt, ap);
	va_end(ap);
}

void (stat_ev_ctx_push_str)(const char *key, const char *str)
{
	stat_ev_ctx_push_str_(key, str);
}

void do_stat_ev_ctx_pop(const char *key)
{
	stat_ev_tim_push();
	stat_ev_printf('O', key, NULL);
	stat_ev_tim_pop(NULL);
}

void (stat_ev_ctx_pop)(const char *key)
{
	stat_ev_ctx_pop_(key);
}

void do_stat_ev_dbl(const char *name, double value)
{
	stat_ev_tim_push();
	stat_ev_printf('E', name, "%g", value);
	stat_ev_tim_pop(NULL);
}

void (stat_ev_dbl)(const char *name, double value)
{
	stat_ev_dbl_(name, value);
}

void do_stat_ev_int(const char *name, int value)
{
	stat_ev_tim_push();
	stat_ev_printf('E', name, "%d", value);
	stat_ev_tim_pop(NULL);
}

void (stat_ev_int)(const char *name, int value)
{
	stat_ev_int_(name, value);
}

void do_stat_ev_ull(const char *name, unsigned long long value)
{
	stat_ev_tim_push();
	stat_ev_printf('E', name, "%llu", value);
	stat_ev_tim_pop(NULL);
}

void (stat_ev_ull)(const char *name, unsigned long long value)
{
	stat_ev_ull_(name, value);
}

void do_stat_ev(const char *name)
{
	stat_ev_tim_push();
	stat_ev_printf('E', name, "0.0");
	stat_ev_tim_pop(NULL);
}

void (stat_ev)(const char *name)
{
	stat_ev_(name);
}

void stat_ev_begin(const char *prefix, const char *filt)
{
	char buf[512];

	snprintf(buf, sizeof(buf), "%s.ev", prefix);
	stat_ev_file = fopen(buf, "wt");

	if (filt && filt[0] != '\0') {
		filter = NULL;
		if (regcomp(&regex, filt, REG_EXTENDED) == 0)
			filter = &regex;
	}

	stat_ev_enabled = stat_ev_file != NULL;
}

void stat_ev_end(void)
{
	if (stat_ev_file != NULL) {
		fclose(stat_ev_file);
		stat_ev_file    = NULL;
		stat_ev_enabled = 0;
	}
	if (filter != NULL) {
		regfree(filter);
		filter = NULL;
	}
}
