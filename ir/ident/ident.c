/*
 * Project:     libFIRM
 * File name:   ir/common/ident.c
 * Purpose:     Hash table to store names.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

#include "ident_t.h"
#include "array.h"
#include "set.h"

static set *id_set;

void id_init(int initial_n_idents)
{
  id_set = new_set(memcmp, initial_n_idents);
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

INLINE const char *get_id_str(ident *id)
{
  return (const char *)id->dptr;
}

INLINE int get_id_strlen(ident *id)
{
  return id->size;
}

int id_is_prefix(ident *prefix, ident *id)
{
  if (get_id_strlen(prefix) > get_id_strlen(id)) return 0;
  return 0 == memcmp(prefix->dptr, id->dptr, get_id_strlen(prefix));
}

int id_is_suffix(ident *suffix, ident *id)
{
  int suflen = get_id_strlen(suffix);
  int idlen  = get_id_strlen(id);
  char *part;

  if (suflen > idlen) return 0;

  part = (char *)id->dptr;
  part = part + (idlen - suflen);

  return 0 == memcmp(suffix->dptr, part, suflen);
}

int id_contains_char(ident *id, char c)
{
  return strchr(get_id_str(id), c) != NULL;
}

int print_id (ident *id)
{
  return printf("%s", get_id_str(id));
}

int fprint_id (FILE *F, ident *id)
{
  return fprintf(F, "%s", get_id_str(id));
}
