/*
 * Project:     libFIRM
 * File name:   ir/adt/debug.h
 * Purpose:     Declarations for debug
 * Author:      Christian von Roques
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Christian von Roques
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifndef _DEBUG_H_
#define _DEBUG_H_

void d_init (int nflags);
int  d_ (int flag, unsigned level);
int  d_level (int flag);
int  d_set_level (int flag, unsigned level);
void d_parse (const char *s);

#ifdef DEBUG

#include "deflag.h"

extern unsigned char *d_vec;

# define d_init(n) (d_init) ((n))
# define d_(flag, level) (d_vec[(flag)] >= (level))
# define d_level(flag) (d_vec[(flag)])
# define d_set_level(flag, level) (d_vec[(flag)] = (level))
# define d_parse(s) (d_parse) ((s))

#else	/* !DEBUG */

# define d_init(n) ((void)0)
# define d_(flag, level) 0
# define d_level(flag) 0
# define d_set_level(flag, level) (level)
# define d_parse(s) ((void)0)

#endif	/* !DEBUG */

#endif
