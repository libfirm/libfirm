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
#include <stdlib.h>
#include <stdarg.h>

#include "util.h"
#include "timing.h"
#include "irprintf.h"
#include "statev.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_REGEX_H
#define FIRM_HAVE_REGEX
#endif

#if defined HAVE_LIBZ && defined HAVE_ZLIB_H
#define FIRM_HAVE_LIBZ
#endif


#define MAX_TIMER 256

#ifdef FIRM_HAVE_LIBZ
#include <zlib.h>

#define mfprintf   gzprintf
static gzFile*     stat_ev_file  = NULL;

#else

#define mfprintf   fprintf
static FILE*       stat_ev_file  = NULL;

#endif /* FIRM_HAVE_LIBZ */

int                stat_ev_enabled = 0;
int                stat_ev_timer_sp = 0;
timing_ticks_t     stat_ev_timer_elapsed[MAX_TIMER];
timing_ticks_t     stat_ev_timer_start[MAX_TIMER];
timing_sched_env_t stat_ev_sched_rt;
timing_sched_env_t stat_ev_sched_normal;

#ifdef FIRM_HAVE_REGEX
#include <regex.h>
static regex_t regex;
static regex_t *filter = NULL;
static INLINE int key_matches(const char *key)
{
	if (!filter)
		return 1;

	return regexec(filter, key, 0, NULL, 0) == 0;
}

#else
static char filter[128] = { '\0' };
static INLINE int key_matches(const char *key)
{
	int i = 0;

	for (i = 0; filter[i] != '\0'; ++i) {
		if (key[i] != filter[i])
			return 0;
	}

	return 1;
}
#endif /* FIRM_HAVE_REGEX */


void stat_ev_printf(char ev, const char *key, const char *fmt, ...)
{
	if (!key_matches(key))
		return;

	mfprintf(stat_ev_file, "%c;%s", ev, key);
	if (fmt != NULL) {
		char buf[256];
		va_list args;

		va_start(args, fmt);
		ir_vsnprintf(buf, sizeof(buf), fmt, args);
		va_end(args);
		mfprintf(stat_ev_file, ";%s", buf);
	}
	mfprintf(stat_ev_file, "\n");
}

void stat_ev_begin(const char *prefix, const char *filt)
{
	char buf[512];

#ifdef FIRM_HAVE_LIBZ
	snprintf(buf, sizeof(buf), "%s.ev.gz", prefix);
	stat_ev_file    = gzopen(buf, "wt9");
#else
	snprintf(buf, sizeof(buf), "%s.ev", prefix);
	stat_ev_file    = fopen(buf, "wt");
#endif

	if (filt && filt[0] != '\0') {
#ifdef FIRM_HAVE_REGEX
		filter = NULL;
		if (regcomp(&regex, filt, REG_EXTENDED) == 0)
			filter = &regex;
#else
		strncpy(filter, filt, sizeof(filter) - sizeof(filter[0]));
#endif /* FIRM_HAVE_REGEX */
	}

	stat_ev_enabled = stat_ev_file != NULL;
	timing_sched_get(&stat_ev_sched_normal);
	timing_sched_prepare_max_prio(&stat_ev_sched_rt);
}

void stat_ev_end(void)
{
	if (stat_ev_file) {
#ifdef FIRM_HAVE_LIBZ
		gzflush(stat_ev_file, 1);
		gzclose(stat_ev_file);
#else
		fclose(stat_ev_file);
#endif
	}
}
