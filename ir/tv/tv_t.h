/* Declarations for Target Values.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

#ifndef _TV_T_H_
#define _TV_T_H_

# include "tv.h"
# include "misc.h"

#include "gmp.h"
#undef __need_size_t		/* erroneously defined by 1.3.2's gmp.h */


tarval *tarval_S_from_str (const char *s, size_t len);
tarval *tarval_s_from_str (const char *s, size_t len);
tarval *tarval_B_from_str (const char *s, size_t len);
tarval_B tv_val_B (tarval *tv);
tarval_s tv_val_s (tarval *tv);


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
