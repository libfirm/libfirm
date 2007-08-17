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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include <libcore/lc_timing.h>

#include "util.h"
#include "hashptr.h"
#include "irprintf.h"

#define MAX_CTX 128

typedef struct {
	char key[32];
	char value[96];
	unsigned hash;
} ctx_t;

static ctx_t ctx_stack[MAX_CTX];

static unsigned long time_in_ev = 0;
static int ctx_sp     = -1;
static FILE *file_ev  = NULL;

static lc_timer_t *timer = NULL;

int stat_ev_enabled = 0;

#define get_time() lc_timer_elapsed_usec(timer)

void stat_ev_ctx_push(const char *key, const char *value)
{
	if (file_ev) {
		unsigned long start = get_time();
		ctx_t *ctx    = &ctx_stack[ctx_sp + 1];
		unsigned hash = firm_fnv_hash_str(key);

		hash = HASH_COMBINE(hash, firm_fnv_hash_str(value));
		if (ctx_sp >= 0)
			hash = HASH_COMBINE(hash, ctx_stack[ctx_sp].hash);

		strncpy(ctx->key, key, array_size(ctx->key));
		strncpy(ctx->value, value, array_size(ctx->key));
		ctx->hash  = hash | 1;
		++ctx_sp;

		fprintf(file_ev, "P %10x %30s %30s\n", ctx->hash, key, value);

		time_in_ev += get_time() - start;
	}
}

void stat_ev_ctx_push_fobj(const char *key, const void *firm_object)
{
	if (file_ev) {
		char buf[96];
		ir_snprintf(buf, sizeof(buf), "%+F", firm_object);
		stat_ev_ctx_push(key, buf);
	}
}

void stat_ev_ctx_pop(void)
{
	if (ctx_sp >= 0) {
		if (file_ev)
			fprintf(file_ev, "O %10x\n", ctx_stack[ctx_sp].hash);
		--ctx_sp;
	}
}

void stat_ev_emit(const char *name, double value)
{
	if (file_ev) {
		unsigned long start = get_time();
		unsigned         id = ctx_sp >= 0 ? ctx_stack[ctx_sp].hash : 0;

		fprintf(file_ev, "E %10x %30s %30f %10ld %10ld\n", id, name, value, start, time_in_ev);
		time_in_ev += get_time() - start;
	}
}

void stat_ev_begin(const char *prefix)
{
	char buf[512];

	snprintf(buf, sizeof(buf), "%s.ev", prefix);

	stat_ev_enabled = 1;
	ctx_sp     = -1;
	time_in_ev = 0;
	file_ev    = fopen(buf, "wt");
	timer      = lc_timer_register("stat_ev", "firm stat event timer");

	lc_timer_start(timer);
}

void stat_ev_end(void)
{
	if (timer)
		lc_timer_stop(timer);
	if (file_ev)
		fclose(file_ev);
}

void stat_ev_flush(void)
{
	unsigned long start = get_time();
	if (file_ev)
		fflush(file_ev);
	time_in_ev += get_time() - start;
}
