/* Declarations for ident.
   Copyright (C) 1995, 1996 Markus Armbruster */

/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer
*/

/* $Id$ */

# ifndef _IDENT_T_H_
# define _IDENT_T_H_

# include "ident.h"

# include "xprintf.h"
# include "xp_help.h"

void id_init (void);
int ident_print (XP_PAR1, const xprintf_info *, XP_PARN);

/* @@@ tune */
#define ID_HASH(str, len) \
  (((  ((unsigned char *)(str))[0] * 33 \
     + ((unsigned char *)(str))[(len)>>1]) * 31 \
    + ((unsigned char *)(str))[(len)-1]) * 9 \
   + (len))

# endif /* _IDENT_T_H_ */
