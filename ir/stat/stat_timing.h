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
 * @brief   OS abstraction from time measurement
 * @author  Sebastian Hack, Michael Beck, Matthias Braun
 * @version $Id$
 */
#ifndef FIRM_STAT_TIMING_H
#define FIRM_STAT_TIMING_H

#if defined(__i386__) || defined(_M_IX86)

#if defined(__GNUC__)
typedef unsigned long long timing_ticks_t;
static inline timing_ticks_t __timing_ticks(void) { timing_ticks_t result; __asm__ __volatile__ ("rdtsc" : "=A" (result)); return result; }
#elif defined(_MSC_VER)
/* win32 implementation using rdtsc */
typedef unsigned __int64 timing_ticks_t;
static __inline timing_ticks_t __timing_ticks(void) { __asm { rdtsc } }
#else
#error need a 64bit int type
#endif

#define timing_ticks(t)              ((t) = __timing_ticks())
#define timing_ticks_init(t)         ((t) = 0)
#define timing_ticks_cmp(a, b, cmp)  ((a) cmp (b))
#define timing_ticks_sub(r, a)       ((r) = (r) - (a))
#define timing_ticks_add(r, a)       ((r) = (r) + (a))
#define timing_ticks_ulong(t)        ((unsigned long) (t))
#define timing_ticks_dbl(t)          ((double) (t))

#else /* !__i386__ */

#include <sys/time.h>

typedef struct timeval timing_ticks_t;
#define timing_ticks(t)              (gettimeofday(&(t), NULL))
#define timing_ticks_init(t)         memset(&(t), 0, sizeof(t))

/*
 * This shamelessly stolen and modified from glibc's
 * /usr/include/sys/time.h
 */
#define timing_ticks_cmp(a, b, CMP)   \
  (((a).tv_sec == (b).tv_sec) ? 	  \
   ((a).tv_usec CMP (b).tv_usec) :    \
   ((a).tv_sec CMP (b).tv_sec))

#define timing_ticks_add(r, a)                       \
	do {							                 \
		(r).tv_sec = (r).tv_sec + (a).tv_sec;        \
		(r).tv_usec = (r).tv_usec + (a).tv_usec;     \
		if ((r).tv_usec >= 1000000) {			     \
			++(r).tv_sec;                            \
			(r).tv_usec -= 1000000;                  \
		}									         \
	} while (0)

#define timing_ticks_sub(r, a)                        \
	do {									          \
		(r).tv_sec = (r).tv_sec - (a).tv_sec;	      \
		(r).tv_usec = (r).tv_usec - (a).tv_usec;      \
		if ((r).tv_usec < 0) {					      \
			--(r).tv_sec;						      \
			(r).tv_usec += 1000000;					  \
		}									          \
	} while (0)

#define timing_ticks_ulong(t)        ((unsigned long) ((t).tv_usec + 1000000 * (t).tv_sec))
#define timing_ticks_dbl(t)          (((t).tv_usec + 1000000.0 * (t).tv_sec))

#endif

void timing_enter_max_prio(void);
void timing_leave_max_prio(void);

#endif
