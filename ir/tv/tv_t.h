/* Declarations for Target Values.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

#ifndef _TV_T_H_
#define _TV_T_H_

# include "tv.h"
# include "misc.h"

int tarval_print (XP_PAR1, const xprintf_info *, XP_PARN);

/* Hash function on tarvals */
unsigned tarval_hash (tarval *);


#ifdef NDEBUG
#define TARVAL_VRFY(val) ((void)0)
#else
#define TARVAL_VRFY(val) _tarval_vrfy ((val))
extern void _tarval_vrfy (const tarval *);
#endif

#ifdef STATS
void tarval_stats (void);
#else
#define tarval_stats() ((void)0)
#endif

#endif /* _TV_T_H_ */
