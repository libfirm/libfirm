/* Ident --- unique handles for identifiers
   Copyright (C) 1995, 1996 Markus Armbruster
   All rights reserved. */

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stddef.h>

#include "ident_t.h"
#include "array.h"
#include "tune.h"
#include "misc.h"
#include "set.h"

#define ID_TO_STR(id) ((const char *)&(id)->dptr[0])
#define ID_TO_STRLEN(id) ((id)->size)
#define ID_TO_HASH(id) ((long)(id) + (id)->hash)

/* Vormals Debugunterstuetzung, entfernt (debug.h). */
# define ID_VRFY(id) ((void)0)
# define IDS_VRFY(id) ((void)0)

#ifdef STATS
# define id_stats() set_stats (id_set)
#else
# define id_stats() ((void)0)
#endif


static set *id_set;

void id_init(void)
{
  id_set = new_set(memcmp, TUNE_NIDENTS);
}

INLINE ident *id_from_str (const char *str, int len)
{
  assert(len > 0);
  return set_hinsert0(id_set, str, len, ID_HASH(str, len));
}

ident *new_id_from_str(const char *str)
{
  assert(str);
  return id_from_str(str, strlen(str));
}

INLINE const char *id_to_str(ident *id)
{
  return (const char *)id->dptr;
}

INLINE int id_to_strlen(ident *id)
{
  return id->size;
}

int id_is_prefix(ident *prefix, ident *id)
{
  if (id_to_strlen(prefix) > id_to_strlen(id)) return 0;
  return 0 == memcmp(prefix->dptr, id->dptr, id_to_strlen(prefix));
}

int id_is_suffix(ident *suffix, ident *id)
{
  int suflen = id_to_strlen(suffix);
  int idlen  = id_to_strlen(id);
  char *part;

  if (suflen > idlen) return 0;

  part = (char *)id->dptr;
  part = part + (idlen - suflen);

  return 0 == memcmp(suffix->dptr, part, suflen);
}

int id_contains_char(ident *id, char c)
{
  return strchr(id_to_str(id), c) != NULL;
}

int print_id (ident *id)
{
  return xprintf("%I", id);
}

int fprint_id (FILE *F, ident *id)
{
  return xfprintf(F, "%I", id);
}

int
ident_print (XP_PAR1, const xprintf_info *info ATTRIBUTE((unused)), XP_PARN)
{
  ident *id = XP_GETARG (ident *, 0);
  return XPMR (ID_TO_STR (id), ID_TO_STRLEN (id));
}
