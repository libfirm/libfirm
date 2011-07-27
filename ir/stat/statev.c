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
 * @date        17.06.2007
 * @version     $Id$
 */
#include "config.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <regex.h>

#include "util.h"
#include "stat_timing.h"
#include "irprintf.h"
#include "statev.h"

#include "config.h"

#ifndef DISABLE_STATEV

#define MAX_TIMER 256

static FILE* stat_ev_file  = NULL;

int            stat_ev_enabled = 0;
int            stat_ev_timer_sp = 0;
timing_ticks_t stat_ev_timer_elapsed[MAX_TIMER];
timing_ticks_t stat_ev_timer_start[MAX_TIMER];

static regex_t regex;
static regex_t *filter = NULL;
static inline int key_matches(const char *key)
{
	if (!filter)
		return 1;

	return regexec(filter, key, 0, NULL, 0) == 0;
}

void stat_ev_printf(char ev, const char *key, const char *fmt, ...)
{
	if (!key_matches(key))
		return;

	fprintf(stat_ev_file, "%c;%s", ev, key);
	if (fmt != NULL) {
		char buf[256];
		va_list args;

		va_start(args, fmt);
		ir_vsnprintf(buf, sizeof(buf), fmt, args);
		va_end(args);
		fprintf(stat_ev_file, ";%s", buf);
	}
	fprintf(stat_ev_file, "\n");
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
	if (stat_ev_file) {
		fclose(stat_ev_file);
	}
	if (filter != NULL)
		regfree(filter);
}

#endif
