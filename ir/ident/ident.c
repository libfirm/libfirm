/* Ident --- unique handles for identifiers
   Copyright (C) 1995, 1996 Markus Armbruster
   All rights reserved. */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include "array.h"
#include "tune.h"
#include "ident.h"
#include "xprintf.h"

#define XX_USER(name) ident *id_##name;
#define XX_INTERNAL(name, str) XX_USER(name)
#undef XX_USER
#undef XX_INTERNAL

set *id_set;


ident *
new_id_derived (const char *pfx, ident *id)
{
  int pfx_len = strlen (pfx);
  int len = pfx_len + ID_TO_STRLEN (id);
  char *str = alloca (len);

  memcpy (str, pfx, pfx_len);
  memcpy (str+pfx_len, ID_TO_STR (id), ID_TO_STRLEN (id));
  return ID_FROM_STR (str, pfx_len + ID_TO_STRLEN (id));
}


ident *
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


bool
id_is_internal (ident *id)
{
  assert (ID_TO_STRLEN (id));
  return !!ispunct (ID_TO_STR(id)[0]);
}


#ifndef NDEBUG

void
ids_vrfy (ident **id)
{
  int i;

  for (i = 0;  i < ARR_LEN (id);  ++i) {
    ID_VRFY (id[i]);
  }
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
