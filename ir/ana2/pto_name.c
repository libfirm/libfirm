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
#  include "config.h"
# endif

/*
  pto_name: Names for abstract objects
*/

# include "pto.h"
# include "pto_name.h"
# include "pto_util.h"

# include <string.h>            /* for memcpy */
# include <obstack.h>
# include <errno.h>

# include "irnode.h"
# include "irprog.h"
# include "xmalloc.h"

# include "pto_debug.h"
# include "gnu_ext.h"

/* Local Defines: */
# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free free

# define NALLOC(size)   obstack_alloc (name_obst, size)

/* Local Data Types: */

/* Local Variables: */
struct obstack *qset_obst = NULL;
struct obstack *name_obst = NULL;

static desc_t *all_descs = NULL;

static int name_id = 0;

# define PTO_COLOR

# ifdef PTO_COLOR
static int last_col_idx = 0;

/* Local Prototypes: */
static int pto_name_alloc_color (desc_t*);
static void pto_name_set_color (desc_t*, int);
# endif /* defined PTO_COLOR */

/* ===================================================
   Local Implementation:
   =================================================== */
# ifdef PTO_COLOR
/* set a nice color to an object descriptor */
static void pto_name_set_obj_color (obj_desc_t *obj_desc, int col_idx)
{
  int i;

  for (i = 0; i < obj_desc->n_fields; i ++) {
    qset_t *ptos = obj_desc->values [i];

    desc_t *tgt = (desc_t*) qset_start (ptos);

    while (NULL != tgt) {
      pto_name_set_color (tgt, col_idx);

      tgt = (desc_t*) qset_next (ptos);
    }
  }
}

/* set a nice color to an array descriptor */
static void pto_name_set_arr_color (arr_desc_t *arr_desc, int col_idx)
{
  qset_t *ptos = arr_desc->value;

  desc_t *tgt = (desc_t*) qset_start (ptos);

  while (NULL != tgt) {
    pto_name_set_color (tgt, col_idx);

    tgt = (desc_t*) qset_next (ptos);
  }
}

/* set a nice color to a descriptor */
static void pto_name_set_color (desc_t *desc, int col_idx)
{
  /* assert (0 == desc->col_idx); */

  desc->col_idx = col_idx;

  if (FALSE == desc->visit) {
    desc->visit = TRUE;
    if (object == desc->kind) {
      pto_name_set_obj_color ((obj_desc_t*) desc, col_idx);
    } else if (array == desc->kind) {
      pto_name_set_arr_color ((arr_desc_t*) desc, col_idx);
    }

    desc->visit = FALSE;
  }
}


/* allocate nice colors for an object descriptor */
static int pto_name_alloc_obj_color (obj_desc_t *obj_desc)
{
  int i;
  int col_idx = 0;

  for (i = 0; (0 == col_idx) && (i < obj_desc->n_fields); i ++) {
    qset_t *ptos = obj_desc->values [i];
    desc_t *tgt = (desc_t*) qset_start (ptos);

    while ((0 == col_idx) && (NULL != tgt)) {
      col_idx = pto_name_alloc_color (tgt);

      tgt = (desc_t*) qset_next (ptos);
    }
  }

  return (col_idx);
}

/* allocate nice colors for an array descriptor */
static int pto_name_alloc_arr_color (arr_desc_t *arr_desc)
{
  int col_idx = 0;

  qset_t *ptos = arr_desc->value;

  desc_t *tgt = (desc_t*) qset_start (ptos);

  while ((NULL != tgt) && (0 == col_idx)) {
    col_idx = pto_name_alloc_color (tgt);
    tgt = (desc_t*) qset_next (ptos);
  }

  return (col_idx);
}

/* allocate nice colors for the given descriptor */
static int pto_name_alloc_color (desc_t *desc)
{
  int col_idx = 0;

  if (0 != desc->col_idx) {
    return (desc->col_idx);
  }

  if (FALSE == desc->visit) {
    desc->visit = TRUE;
    if (object == desc->kind) {
      col_idx = pto_name_alloc_obj_color ((obj_desc_t*) desc);
    } else if (array == desc->kind) {
      col_idx = pto_name_alloc_arr_color ((arr_desc_t*) desc);
    }
    desc->visit = FALSE;
  }

  if (0 == col_idx) {
    col_idx = ++ last_col_idx;
  }

  pto_name_set_color (desc, col_idx);

  return (col_idx);
}

/* allocate nice colors */
static void pto_name_alloc_colors (void)
{
  desc_t *desc = all_descs;

  while (NULL != desc) {
    pto_name_alloc_color (desc);

    desc = desc->prev;
  }
}
# endif /* defined PTO_COLOR */

/* See whether the given entity is a field. */
static int is_field (entity *ent)
{
  type *tp = get_entity_type (ent);

  if (is_Primitive_type (tp) || is_Pointer_type (tp)) {
    /* actually, we don't get by by restricting ourselves to pointer types */
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
  struct obstack obst;
  int n_fields;
  entity ** fields;
  void *tmp;

  if (NULL != get_type_link (clazz)) {
    DBGPRINT (3, (stdout, "%s: reusing field list for \"%s\"\n",
                  __FUNCTION__,
                  get_type_name (clazz)));

    return ((entity **) get_type_link (clazz));
  } else {
    DBGPRINT (2, (stdout, "%s: new field list for \"%s\"\n",
                  __FUNCTION__,
                  get_type_name (clazz)));
  }

  obstack_init (&obst);

  _collect_fields (clazz, &obst);

  /* append terminating NULL */
  obstack_ptr_grow (&obst, NULL);

  n_fields = obstack_object_size (&obst) / sizeof (void*);

  fields = NALLOC (n_fields * sizeof (entity*));
  tmp = obstack_finish(&obst);

  memcpy (fields, tmp, n_fields * sizeof (entity*));

  obstack_free (&obst, NULL);

  set_type_link (clazz, fields);

  return (fields);
}

/* Write the intro text for a name dump into the given stream */
static void pto_name_dump_start (FILE *stream)
{
# ifdef PTO_COLOR
  pto_name_alloc_colors ();
# endif /* defined PTO_COLOR */

  fprintf (stream, "digraph \"Names\" {\n");
  fprintf (stream, "\tgraph [rankdir=\"LR\", ordering=\"out\", size=\"11,7\", rotate=\"90\"];\n");
  fprintf (stream, "\tnode [shape=\"record\", style=\"filled\"];\n");
  fprintf (stream, "\tedge [color=\"black\"];\n");
  fprintf (stream, "\n");
}

/* Write the extro text for a name dump into the given stream */
static void pto_name_dump_finish (FILE *stream)
{
  fprintf (stream, "}\n");
}

/* Write a node for the given descriptor into the given stream */
static void pto_name_dump_desc (desc_t *desc, FILE *stream)
{
  type *tp = desc->tp;
  const char *tp_name = get_type_name (tp);
  ir_node *nd;

  fprintf (stream, "\t/* %s \"%s\" */\n",
           object == desc->kind ? "Object" : "Array",
           tp_name);

  fprintf (stream, "\tdesc_%i [label=\"<HEAD>type \\[%i\\]",
           desc->id, desc->id);

  if (-1 != desc->ctx) {
    fprintf (stream, ", ctx = %i", desc->ctx);
  } else {
    fprintf (stream, " (global)");
  }

  fprintf (stream, "\\lname=\\\"%s\\\"",
           tp_name);

  nd = desc->node;

  if (NULL != nd) {
    ir_graph *graph = get_irn_irg (nd);
    entity *method = get_irg_entity (graph);
    const char *ent_name = get_entity_name (method);
    const char *own_name = get_type_name (get_entity_owner (method));

    fprintf (stream, "\\lnode=%s\\[%li\\]",
             get_op_name (get_irn_op (nd)),
             get_irn_node_nr (nd));
    fprintf (stream, "\\lgraph=\\\"%s.%s\\\"",
             own_name,
             ent_name);
  }

  if (desc->kind == object) {
    obj_desc_t *obj_desc = (obj_desc_t*) desc;

    int i;
    for (i = 0; i < obj_desc->n_fields; i ++) {
      entity *field = obj_desc->fields [i];

      if (is_Pointer_type (get_entity_type (field))) {
        const char *ent_name = get_entity_name (field);

        fprintf (stream, "|<%i>%s", i, ent_name);
      }
    }
  } else if (array == desc->kind) {
    fprintf (stream, "|<arr>[]");
  } else {
    assert (0 && "invalid descriptor");
  }

  /* end label string */
  fprintf (stream, "\"");

# ifdef PTO_COLOR
  {
    const char *fontcolor;
    int col_idx = desc->col_idx;
    float hue = (float) col_idx / (float) last_col_idx;
    float sat = 1.000f;            /* 0.300 .. 1.000 */
    float val = 0.800f;            /* 0.300 .. 1.000 */

  # define MAX_COLORS 12
    if (last_col_idx > MAX_COLORS) {
      /* too many colors ... vary value too */
      float div = (float) MAX_COLORS / (float) last_col_idx;

      sat = (div * ((col_idx / MAX_COLORS)));
      val = (div * ((col_idx / MAX_COLORS)));

      col_idx = col_idx % MAX_COLORS;
      hue = (float) col_idx / (float) MAX_COLORS;

      // re-adjust sat and val
      {
        const float sat_min = 0.200f; /* 0.200 .. 0.400 */
        const float val_min = 0.300f; /* 0.300 .. 0.400 */

        sat = sat_min + ((1.0f - sat_min) * sat);
        val = val_min + ((1.0f - val_min) * val);
      }
    }
  # undef MAX_COLORS

    fprintf (stream, ", color=\"%01.3f, %01.3f, %01.3f\"", hue, sat, val);

    if ((hue > 0.3) && (sat < 0.5)) {
      fontcolor = "white";
    } else if (sat < 0.4) {
      fontcolor = "white";
    } else {
      fontcolor = "black";
    }

    fprintf (stream, ", fontcolor=\"%s\"", fontcolor);
  }
# else /* if defined PTO_COLOR */
  fprintf (stream, ", color=\"lightgrey\"");
# endif /* defined PTO_COLOR */

  /* end attributes */
  fprintf (stream, "];\n");
  fprintf (stream, "\n");

  /* now the edges */
  if (desc->kind == object) {
    obj_desc_t *obj_desc = (obj_desc_t*) desc;

    int i;
    for (i = 0; i < obj_desc->n_fields; i ++) {
      desc_t *tgt = (desc_t*) qset_start (obj_desc->values [i]);

      while (NULL != tgt) {
        fprintf (stream, "\tdesc_%i:%i -> desc_%i:HEAD;\n",
                 desc->id, i, tgt->id);

        tgt = (desc_t*) qset_next (obj_desc->values [i]);
      }
    }
  } else if (array == desc->kind) {
    arr_desc_t *arr_desc = (arr_desc_t*) desc;

    desc_t *tgt = (desc_t*) qset_start (arr_desc->value);

    while (NULL != tgt) {
      fprintf (stream, "\tdesc_%i:arr -> desc_%i:HEAD;\n",
               desc->id, tgt->id);

      tgt = (desc_t*) qset_next (arr_desc->value);
    }
  }

  fprintf (stream, "\n");
}


/* ===================================================
   Exported Implementation:
   =================================================== */
/* Find the given descriptor's entry for the given entity */
qset_t *get_entry (desc_t *desc, entity *ent)
{

  if (desc->kind == object) {
    obj_desc_t *obj_desc = (obj_desc_t*) desc;
    int i;
    const int n_fields = obj_desc->n_fields;

    for (i = 0; i < n_fields; i ++) {
      if (ent == obj_desc->fields [i]) {
        return (obj_desc->values [i]);
      }
    }

    assert (0 && "entry not found");
  } else if (desc->kind == array) {
    arr_desc_t *arr_desc = (arr_desc_t*) desc;

    return (arr_desc->value);
  } else {
    assert (0 && "invalid descriptor");
    return NULL;
  }
}


/* get a new descriptor for the given type at the given node */
desc_t *new_name (type *tp, ir_node *node, int ctx)
{
  desc_t *desc = NULL;

  assert ((is_Class_type (tp) || is_Array_type (tp)) && "unsuitable type");

  DBGPRINT (2, (stdout, "%s: new name for type \"%s\"\n",
                __FUNCTION__,
                get_type_name (tp)));
  fflush (stdout);

  if (is_Class_type (tp)) {
    obj_desc_t *obj_desc = NALLOC (sizeof (obj_desc_t));
    int i;
    int n_fields;

    obj_desc->kind = object;
    obj_desc->fields = collect_fields (tp);

    for (n_fields = 0; (NULL != obj_desc->fields [n_fields]); n_fields ++) {
      /* nothing, just count ... */
    }

    obj_desc->n_fields = n_fields;
    obj_desc->values = (qset_t**) NALLOC (n_fields * sizeof (qset_t*));

    for (i = 0; i < n_fields; i ++) {
      obj_desc->values [i] = qset_new (N_INITIAL_OJBS, qset_obst);
    }

    desc = (desc_t*) obj_desc;
  } else if (is_Array_type (tp)) {
    arr_desc_t *arr_desc = (arr_desc_t*) NALLOC (sizeof (arr_desc_t));

    arr_desc->kind = array;
    arr_desc->value = qset_new (N_INITIAL_OJBS, qset_obst);

    desc = (desc_t*) arr_desc;
  }

  desc->id    = name_id ++;
  desc->col_idx = 0;
  desc->tp    = tp;
  desc->visit = FALSE;
  desc->ctx   = ctx;
  desc->node  = node;

  desc->prev  = all_descs;
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

  assert (is_Pointer_type (tp));
  tp = get_pointer_points_to_type (tp);
  assert (is_Class_type (tp));

  DBGPRINT (2, (stdout, "%s: new name for entity \"%s\"\n",
                __FUNCTION__,
                get_entity_name (ent)));
  DBGEXE (2, (fflush (stdout)));

  assert (((allocation_static == get_entity_allocation (ent)) ||
           (allocation_automatic == get_entity_allocation (ent))) &&
          "not a static/automatic field");

  if (NULL == obj_glob) {
    obj_glob = (obj_desc_t*) NALLOC (sizeof (obj_desc_t));

    obj_glob->id   = name_id ++;
    obj_glob->ctx  = -1;
    obj_glob->col_idx = 0;
    obj_glob->visit = FALSE;
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

/* Dump all names to a file of the given name */
void pto_dump_names (const char *name)
{
  desc_t *desc = all_descs;
  FILE *stream = fopen (name, "w");

  errno = 0;
  if  (NULL == stream) {
    fprintf (stderr, "%s: unable to open %s (%s)\n",
             __FUNCTION__, name, strerror (errno));
    return;
  }

  pto_name_dump_start (stream);

  while (NULL != desc) {
    pto_name_dump_desc (desc, stream);

    desc = desc->prev;
  }

  pto_name_dump_finish (stream);
  fclose (stream);
}

/* Initialise the name module */
void pto_name_init (void)
{
  DBGPRINT (3, (stdout, "%s\n", __FUNCTION__));
  assert (NULL == name_obst);
  assert (NULL == qset_obst);

  name_obst = xmalloc (sizeof (struct obstack));
  qset_obst = xmalloc (sizeof (struct obstack));

  obstack_init (name_obst);
  obstack_init (qset_obst);
}

/* Cleanup the name module */
void pto_name_cleanup (void)
{
  DBGPRINT (3, (stdout, "%s\n", __FUNCTION__));
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
  Revision 1.12  2005/01/14 14:13:56  liekweg
  fix gnu extension, fix fprintf's, fix allocs

  Revision 1.11  2005/01/05 14:25:54  beck
  renames all is_x*_type() functions to is_X*_type() to prevent name clash with EDG fronten

  Revision 1.10  2004/12/22 14:43:14  beck
  made allocations C-like

  Revision 1.9  2004/12/21 15:34:09  beck
  removed C99 constructs
  make const float
  add default return

  Revision 1.8  2004/12/15 13:30:30  liekweg
  use DBGEXE correctly; print yet nicer names

  Revision 1.7  2004/12/15 09:18:18  liekweg
  pto_name.c

  Revision 1.6  2004/12/06 12:52:09  liekweg
  colorize name dump

  Revision 1.4  2004/11/30 15:49:27  liekweg
  include 'dump'

  Revision 1.3  2004/11/30 14:47:54  liekweg
  fix initialisation; do correct iteration

  Revision 1.2  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
