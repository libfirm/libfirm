/*
 * Project:     libFIRM
 * File name:   ir/tr/mangle.c
 * Purpose:     Methods to manipulate names.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "mangle.h"
# include "obst.h"

/* Make types visible to allow most efficient access */
# include "entity_t.h"
# include "type_t.h"
# include "tpop_t.h"

static struct obstack mangle_obst;

static INLINE ident *
mangle_type (type *tp)
{
  assert (tp->kind == k_type);
  return tp->name;
}

ident *
mangle_entity (entity *ent)
{
  ident *type_id;
  char *cp;
  int len;
  ident *res;

  type_id = mangle_type ((type *) ent->owner);
  obstack_grow(&mangle_obst, get_id_str(type_id), get_id_strlen(type_id));
  obstack_1grow(&mangle_obst,'_');
  obstack_grow(&mangle_obst,get_id_str(ent->name),get_id_strlen(ent->name));
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = new_id_from_chars(cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}


/* Returns a new ident that represents firstscnd. */
ident *mangle (ident *first, ident* scnd) {
  char *cp;
  int len;
  ident *res;

  obstack_grow(&mangle_obst, get_id_str(first), get_id_strlen(first));
  obstack_grow(&mangle_obst, get_id_str(scnd), get_id_strlen(scnd));
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = new_id_from_chars (cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}

/* Returns a new ident that represents first_scnd. */
ident *mangle_u (ident *first, ident* scnd) {
  char *cp;
  int len;
  ident *res;

  obstack_grow(&mangle_obst, get_id_str(first), get_id_strlen(first));
  obstack_1grow(&mangle_obst,'_');
  obstack_grow(&mangle_obst,get_id_str(scnd),get_id_strlen(scnd));
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = new_id_from_chars (cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}


void
firm_init_mangle (void)
{
  obstack_init(&mangle_obst);
}
