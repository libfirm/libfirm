/* -*- c -*- */

/*
  Project:     libFIRM
  File name:   ir/ana/pto_name.c
  Purpose:     Names for abstract objects
  Author:      Florian
  Modified by:
  Created:     Sat Nov 13 19:35:27 CET 2004
  CVS-ID:      $Id$
  Copyright:   (c) 1999-2004 Universität Karlsruhe
  Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif

/*
  pto_name: Names for abstract objects
*/

# include "pto.h"
# include "pto_name.h"
# include "pto_util.h"

# include <string.h>            /* for memcpy */
# include <obstack.h>

# include "irnode.h"
# include "irprog.h"
# include "xmalloc.h"

# include "pto_debug.h"

/* Local Defines: */
# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free free

# define NALLOC(size)   obstack_alloc (name_obst, size)

/* Local Data Types: */

/* Local Variables: */
struct obstack *qset_obst = NULL;
struct obstack *name_obst = NULL;

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */
/* See whether the given entity is a field. */
static int is_field (entity *ent)
{
  type *tp = get_entity_type (ent);

  if (is_primitive_type (tp) || is_pointer_type (tp)) {
    /* actually, we could get by by restricting ourselves to pointer types */
    return (TRUE);
  } else {
    return (FALSE);
  }
}

/* Helper to collect_fields(type*): collect all fields of the given
  clazz and its super classes into the given obstack. */
static void _collect_fields (type *clazz, struct obstack *obst)
{
  int n_members = get_class_n_members (clazz);
  int n_supers  = get_class_n_supertypes (clazz);
  int i;

  for (i = 0; i < n_members; i ++) {
    entity *ent = get_class_member (clazz, i);

    if (is_field (ent)) {
      if (allocation_static != get_entity_allocation (ent)) {
        obstack_ptr_grow (obst, ent);
      }
    }
  }

  for (i = 0; i < n_supers; i ++) {
    type *s_clazz = get_class_supertype (clazz, i);

    _collect_fields (s_clazz, obst);
  }
}

/* Collect the fields of the given class and its super classes into an array.
  The last entry of the array is written NULL. */
static entity **collect_fields (type *clazz)
{
  if (NULL != get_type_link (clazz)) {
    DBGPRINT (3, (stdout, "%s: reusing field list for \"%s\"\n",
                  __FUNCTION__, get_type_name (clazz)));

    return ((entity **) get_type_link (clazz));
  } else {
    DBGPRINT (2, (stdout, "%s: new field list for \"%s\"\n",
                  __FUNCTION__, get_type_name (clazz)));
  }

  struct obstack obst;

  obstack_init (&obst);

  _collect_fields (clazz, &obst);

  /* append terminating NULL */
  int *the_null = NULL;
  obstack_ptr_grow (&obst, the_null);

  int n_fields = obstack_object_size (&obst) / sizeof (void*);

  entity ** fields = (entity**) NALLOC (n_fields * sizeof (entity*));
  void *tmp = obstack_finish (&obst);

  memcpy (fields, tmp, n_fields * sizeof (entity*));

  obstack_free (&obst, NULL);

  set_type_link (clazz, (void*) fields);

  return (fields);
}



/* ===================================================
   Exported Implementation:
   =================================================== */
static desc_t *all_descs = NULL;

/* get a new descriptor for the given type at the given node */
desc_t *new_name (type *tp, ir_node *node)
{
  desc_t *desc = NULL;

  assert ((is_class_type (tp) || is_array_type (tp)) && "unsuitable type");

  DBGPRINT (2, (stdout, "%s: new name for type \"%s\"\n", __FUNCTION__,
                get_type_name (tp)));
  fflush (stdout);

  if (is_class_type (tp)) {
    obj_desc_t *obj_desc = (obj_desc_t*) NALLOC (sizeof (obj_desc_t));
    int i;
    int n_fields;

    obj_desc->kind = object;
    obj_desc->fields = collect_fields (tp);

    for (n_fields = 0; (NULL != obj_desc->fields [n_fields]); n_fields ++) {
      /* nothing, just count ... */
    }

    obj_desc->n_fields = n_fields;
    obj_desc->values = (qset_t**) NALLOC (n_fields * sizeof (qset_t));

    for (i = 0; i < n_fields; i ++) {
      obj_desc->values [i] = qset_new (N_INITIAL_OJBS, qset_obst);
    }

    desc = (desc_t*) obj_desc;
  } else if (is_array_type (tp)) {
    arr_desc_t *arr_desc = (arr_desc_t*) NALLOC (sizeof (arr_desc_t));

    arr_desc->kind = array;
    arr_desc->value = qset_new (N_INITIAL_OJBS, qset_obst);

    desc = (desc_t*) arr_desc;
  }

  desc->tp   = tp;
  desc->node = node;

  desc->prev = all_descs;
  all_descs = desc;

  return (desc);
}

# define N_GLOB_INITIAL_FIELDS  20
static obj_desc_t *obj_glob = NULL;
static int n_glob_fields = N_GLOB_INITIAL_FIELDS;

/* get a new descriptor for the given (presumably static) entity */
desc_t *new_ent_name (entity *ent)
{
  int i;
  int missing = TRUE;
  type *tp = get_entity_type (ent);

  assert (is_pointer_type (tp));
  tp = get_pointer_points_to_type (tp);
  assert (is_class_type (tp));

  assert (((allocation_static == get_entity_allocation (ent)) ||
           (allocation_automatic == get_entity_allocation (ent))) &&
          "not a static/automatic field");

  if (NULL == obj_glob) {
    obj_glob = (obj_desc_t*) NALLOC (sizeof (obj_desc_t));

    obj_glob->kind = object;
    obj_glob->tp   = get_glob_type ();
    obj_glob->node = NULL;

    obj_glob->n_fields = 0;
    obj_glob->fields = (entity**) NALLOC (N_GLOB_INITIAL_FIELDS * sizeof (entity*));
    obj_glob->values = (qset_t**) NALLOC (N_GLOB_INITIAL_FIELDS * sizeof (qset_t*));

    obj_glob->prev = all_descs;
    all_descs = (desc_t*) obj_glob;
  }

  for (i = 0; missing && (i < obj_glob->n_fields); i ++) {
    if (ent == obj_glob->fields [i]) {
      missing = FALSE;
    }
  }

  if (missing) {
    if (obj_glob->n_fields == n_glob_fields) {
      entity **fields = obj_glob->fields;
      qset_t **values = obj_glob->values;

      n_glob_fields *= 2;
      obj_glob->fields = (entity**) NALLOC (n_glob_fields * sizeof (entity*));
      obj_glob->values = (qset_t**) NALLOC (n_glob_fields * sizeof (qset_t*));

      memcpy (obj_glob->fields, fields, obj_glob->n_fields * sizeof (entity*));
      memcpy (obj_glob->values, values, obj_glob->n_fields * sizeof (qset_t*));

      /* free (fields); */
      /* free (values); */
    }

    obj_glob->fields [obj_glob->n_fields   ] = ent;
    obj_glob->values [obj_glob->n_fields ++] = qset_new (N_INITIAL_OJBS, qset_obst);
  }

  return ((desc_t*) obj_glob);
}
# undef N_GLOB_INITIAL_FIELDS

/* Initialise the name module */
void pto_name_init ()
{
  DBGPRINT (3, (stdout, "(%s:%i) %s\n", __FILE__, __LINE__, __FUNCTION__));
  assert (NULL == name_obst);
  assert (NULL == qset_obst);

  name_obst = xmalloc (sizeof (struct obstack));
  qset_obst = xmalloc (sizeof (struct obstack));

  obstack_init (name_obst);
  obstack_init (qset_obst);
}

/* Cleanup the name module */
void pto_name_cleanup ()
{
  DBGPRINT (3, (stdout, "(%s:%i) %s\n", __FILE__, __LINE__, __FUNCTION__));
  obstack_free (name_obst, NULL);
  obstack_free (qset_obst, NULL);

  memset (name_obst, 0x00, sizeof (struct obstack));
  memset (qset_obst, 0x00, sizeof (struct obstack));

  free (name_obst);
  free (qset_obst);

  name_obst = NULL;
  qset_obst = NULL;
}


/*
  $Log$
  Revision 1.2  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
