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

# include "xprintf.h"
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
mangle_type (type *tp)
{
  char *cp;
  int len;
  ident *res;

  assert (tp->kind == k_type);
  /* assert (tp->type_op->code == tpo_class); */

  xoprintf (&mangle_obst, "%I", tp->name);
  len = obstack_object_size (&mangle_obst);
  cp = obstack_finish (&mangle_obst);
  res = id_from_str (cp, len);
  obstack_free (&mangle_obst, cp);
  return res;
}

/* Returns a new ident that represents firstscnd. */
ident *mangle (ident *first, ident* scnd) {
  char *cp;
  int len;
  ident *res;

  xoprintf (&mangle_obst, "%I%I",  first, scnd);
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
