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

/* Caution: strings _not_ zero-terminated! */
#define ID_FROM_STR(str, len) \
  (assert ((len) > 0), \
   (const set_entry *)set_hinsert (id_set, (str), (len), ID_HASH ((str), (len))))
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

extern set *id_set;

#define XX_USER(name) ident *id_##name;
#define XX_INTERNAL(name, str) XX_USER(name)
#undef XX_USER
#undef XX_INTERNAL

static set *id_set;

#if 0 /* nowhere used */
static ident *
new_id_derived (const char *pfx, ident *id)
{
  int pfx_len = strlen (pfx);
  int len = pfx_len + ID_TO_STRLEN (id);
  char *str = alloca (len);

  memcpy (str, pfx, pfx_len);
  memcpy (str+pfx_len, ID_TO_STR (id), ID_TO_STRLEN (id));
  return ID_FROM_STR (str, pfx_len + ID_TO_STRLEN (id));
}

static ident *
new_id_internal (void)
{
  static char str[] = "_0000000";
  int i;

  i = sizeof (str) - 2;
  while (++str[i] == '9'+1) {
    str[i--] = '0';
    /* if following assertion fails, we get called far too often ;-) */
    assert (i >= 0);
  }
  assert (('0' <= str[i]) && (str[i] <= '9'));

  return ID_FROM_STR (str, sizeof (str) - 1);
}


static bool
id_is_internal (ident *id)
{
  assert (ID_TO_STRLEN (id));
  return !!ispunct (ID_TO_STR(id)[0]);
}
#endif


int
ident_print (XP_PAR1, const xprintf_info *info ATTRIBUTE((unused)), XP_PARN)
{
  ident *id = XP_GETARG (ident *, 0);
  return XPMR (ID_TO_STR (id), ID_TO_STRLEN (id));
}


void
id_init (void)
{
  id_set = new_set (memcmp, TUNE_NIDENTS);

#define XX_USER(name) id_##name = ID_FROM_STR(#name, sizeof(#name)- 1);
#define XX_INTERNAL(name, str) id_##name = ID_FROM_STR((str), sizeof((str))-1);
#undef XX_USER
#undef XX_INTERNAL
}


INLINE ident *id_from_str (const char *str, int len) {
  assert (len > 0);
  return  (const set_entry *) set_hinsert (id_set,
					   (str),
					   (len),
					   ID_HASH ((str), (len)));
}

INLINE const char *id_to_str   (ident *id) {
  return ((const char *)&(id)->dptr[0]);
}

INLINE int id_to_strlen(ident *id) {
  return ((id)->size);
}

int id_is_prefix (ident *prefix, ident *id) {
  if (id_to_strlen(prefix) > id_to_strlen(id)) return 0;
  if (0 == memcmp(&(prefix->dptr[0]), &(id->dptr[0]), id_to_strlen(prefix)))
    return 1;
  return 0;
}

int id_is_suffix (ident *suffix, ident *id) {
  int suflen = id_to_strlen(suffix);
  int idlen = id_to_strlen(id);
  char *part;
  if (suflen > idlen) return 0;

  part = (char *) &id->dptr[0];
  part = part + (idlen - suflen);
  if (0 == memcmp(&(suffix->dptr[0]), part, suflen))
    return 1;
  return 0;
}

int print_id (ident *id) {
  xprintf("%I", id);
  return(0);
}

int fprint_id (FILE *F, ident *id) {
  xfprintf(F, "%I", id);
  return(0);
}
