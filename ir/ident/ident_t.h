/* Declarations for ident.
   Copyright (C) 1995, 1996 Markus Armbruster */

/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
*/

# ifndef _IDENT_T_H_
# define _IDENT_T_H_

#include <assert.h>
#include <stddef.h>
#include "misc.h"
#include "set.h"
#include "ident.h"

# include "xprintf.h"
# include "xp_help.h"

/* Caution: strings _not_ zero-terminated! */
#define ID_FROM_STR(str, len) \
  (assert ((len) > 0), \
   (const set_entry *)set_hinsert (id_set, (str), (len), ID_HASH ((str), (len))))
#define ID_TO_STR(id) ((const char *)&(id)->dptr[0])
#define ID_TO_STRLEN(id) ((id)->size)
#define ID_TO_HASH(id) ((long)(id) + (id)->hash)

ident *new_id_derived (const char *pfx, ident *);
ident *new_id_internal (void);
bool id_is_internal (ident *);
void id_init (void);

/* Vormals Debugunterstuetzung, entfernt (debug.h). */
# define ID_VRFY(id) ((void)0)

#ifdef NDEBUG
# define IDS_VRFY(id) ((void)0)
#else
# define IDS_VRFY(id) ids_vrfy ((id))
void ids_vrfy (ident **id);
#endif


#ifdef STATS
# define id_stats() set_stats (id_set)
#else
# define id_stats() ((void)0)
#endif

/* Private */

/* @@@ tune */
#define ID_HASH(str, len) \
  (((  ((unsigned char *)(str))[0] * 33 \
     + ((unsigned char *)(str))[(len)>>1]) * 31 \
    + ((unsigned char *)(str))[(len)-1]) * 9 \
   + (len))

extern set *id_set;

#if 0
# define id_from_str  ID_FROM_STR
# define id_to_str    ID_TO_STR
# define id_to_strlen ID_TO_STRLEN
#endif

int ident_print (XP_PAR1, const xprintf_info *, XP_PARN);

# endif /* _IDENT_T_H_ */
