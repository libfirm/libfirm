/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "mangle.h"
# include <obstack.h>
# include "obst.h"
# include <stdlib.h>
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
  xoprintf (&mangle_obst, "%I_%I", type_id, ent->name);
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = id_from_str (cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}

ident *
mangle_type (type *type)
{
  char *cp;
  int len;
  ident *res;

  assert (type->kind == k_type);
  /* assert (type->type_op->code == tpo_class); */

  xoprintf (&mangle_obst, "%I", type->name);
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = id_from_str (cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}

/* Returns a new ident that represents first_scnd. */
ident *mangle (ident *first, ident* scnd) {
  char *cp;
  int len;
  ident *res;

  xoprintf (&mangle_obst, "%I_%I",  first, scnd);
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = id_from_str (cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}

void
init_mangle (void)
{
  obstack_init (&mangle_obst);
}
