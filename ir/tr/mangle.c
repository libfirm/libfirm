/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer
*
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "mangle.h"
# include "obst.h"
# include "misc.h"

/* Make types visible to allow most efficient access */
# include "entity_t.h"
# include "type_t.h"
# include "tpop_t.h"

static struct obstack mangle_obst;

ident *
mangle_entity (entity *ent)
{
  ident *type_id;
  char *cp;
  int len;
  ident *res;

  type_id = mangle_type ((type *) ent->owner);
  obstack_grow(&mangle_obst, id_to_str(type_id), id_to_strlen(type_id));
  obstack_1grow(&mangle_obst,'_');
  obstack_grow(&mangle_obst,id_to_str(ent->name),id_to_strlen(ent->name));
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = id_from_str(cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}

ident *
mangle_type (type *tp)
{
  assert (tp->kind == k_type);
  return tp->name;
}

/* Returns a new ident that represents firstscnd. */
ident *mangle (ident *first, ident* scnd) {
  char *cp;
  int len;
  ident *res;

  obstack_grow(&mangle_obst, id_to_str(first), id_to_strlen(first));
  obstack_grow(&mangle_obst, id_to_str(scnd), id_to_strlen(scnd));
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = id_from_str (cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}

/* Returns a new ident that represents first_scnd. */
ident *mangle_u (ident *first, ident* scnd) {
  char *cp;
  int len;
  ident *res;

  obstack_grow(&mangle_obst, id_to_str(first), id_to_strlen(first));
  obstack_1grow(&mangle_obst,'_');
  obstack_grow(&mangle_obst,id_to_str(scnd),id_to_strlen(scnd));
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = id_from_str (cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}


void
init_mangle (void)
{
  obstack_init(&mangle_obst);
}
