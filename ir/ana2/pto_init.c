/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_init.c
   Purpose:     Initialisation Functions
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
 pto_init: Initialisation Functions
*/

# include <obstack.h>
# include <string.h>

# include "pto.h"
# include "pto_init.h"
# include "pto_debug.h"
# include "pto_comp.h"
# include "pto_name.h"
# include "pto_util.h"

# include "typewalk.h"
# include "irgwalk.h"
# include "xmalloc.h"

/* Local Defines: */
# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free  free

/* Local Data Types: */
typedef struct init_env_str
{
  int n_ctxs;
} init_env_t;

typedef struct reset_env_str
{
  int ctx_idx;
} reset_env_t;

/* Local Variables: */
extern struct obstack *qset_obst; /* from pto_name */

static struct obstack *pto_obst = NULL; /* all pto_t's go onto this one */

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */
/* Allocate a new pto */
static pto_t *new_pto (ir_node *node)
{
  pto_t *pto = obstack_alloc (pto_obst, sizeof (pto_t));
  pto->values = qset_new (N_INITIAL_OJBS, qset_obst);

  return (pto);
}

/* Allocate a new alloc_pto */
static alloc_pto_t *new_alloc_pto (ir_node *alloc, int n_ctxs)
{
  assert (iro_Alloc == get_irn_opcode (alloc));

  int i;
  alloc_pto_t *alloc_pto = obstack_alloc (pto_obst, sizeof (alloc_pto_t));
  type *tp = get_Alloc_type (alloc);

  alloc_pto->ptos = (pto_t**) obstack_alloc (pto_obst, n_ctxs * sizeof (pto_t*));

  for (i = 0; i < n_ctxs; i ++) {
    desc_t *desc = new_name (tp, alloc);
    alloc_pto->ptos [i] = new_pto (alloc);
    qset_insert (alloc_pto->ptos [i]->values, desc);
  }

  return (alloc_pto);
}

/* Allocate a new pto for a symconst */
static pto_t* new_symconst_pto (ir_node *symconst)
{
  assert (iro_SymConst == get_irn_opcode (symconst));

  pto_t *pto = new_pto (symconst);
  entity *ent = get_SymConst_entity (symconst);
  desc_t *desc = new_ent_name (ent);

  qset_insert (pto->values, desc);

  return (pto);
}

/* Helper to pto_init --- clear the link fields of class types */
static void clear_type_link (type_or_ent *thing, void *__unused)
{
  if (is_type (thing)) {
    type *tp = (type*) thing;

    if (is_class_type (tp)) {
      DBGPRINT (1, (stdout, "%s (\"%s\")\n",
                    __FUNCTION__, get_type_name (tp)));

      set_type_link (tp, NULL);
    }
  } else if (is_entity (thing)) {
    entity *ent = (entity*) thing;

    DBGPRINT (1, (stdout, "%s (\"%s\")\n",
                  __FUNCTION__, get_entity_name (ent)));

    set_entity_link (ent, NULL);
  }
}

/* Helper to pto_init_graph --- clear the links of the given node */
static void clear_node_link (ir_node *node, void *__unused)
{
  set_irn_link (node, NULL);
}

/* Helper to pto_init_graph --- clear the links of all nodes */
static void clear_graph_links (ir_graph *graph)
{
  irg_walk_graph (graph, clear_node_link, NULL, NULL);
}

/* Reset ALL the pto values for a new pass */
static void reset_node_pto (ir_node *node, void *env)
{
  reset_env_t *reset_env = (reset_env_t*) env;
  int ctx_idx = reset_env->ctx_idx;
  const opcode op = get_irn_opcode (node);

  switch (op) {
  case (iro_Load):
  case (iro_SymConst):
  case (iro_Const):
  case (iro_Call):
  case (iro_Phi): {
    /* allocate 'empty' pto values */
    pto_t *pto = new_pto (node);
    set_node_pto (node, pto);
    } break;

  case (iro_Alloc): {
    /* set alloc to 'right' current pto */
    alloc_pto_t *alloc_pto = (alloc_pto_t*) get_irn_link (node);
    alloc_pto->curr_pto = alloc_pto->ptos [ctx_idx];
    } break;

  default: {
    /* nothing */
  } break;
  }
}

/* Temporary fix until we get 'real' ptos: Allocate some dummy for pto */
static void init_pto (ir_node *node, void *env)
{
  init_env_t *init_env = (init_env_t*) env;
  int n_ctxs = init_env->n_ctxs;

  const opcode op = get_irn_opcode (node);

  switch (op) {
  case (iro_SymConst): {
    const symconst_kind kind = get_SymConst_kind (node);

    if ((kind == symconst_addr_name) || (kind == symconst_addr_ent)) {
      entity *ent = get_SymConst_entity (node);

      if (is_pointer_type (get_entity_type (ent))) {
        DBGPRINT (0, (stdout, "%s: new name \"%s\" for symconst \"%s[%li]\"\n",
                      __FUNCTION__,
                      get_entity_name (ent),
                      OPNAME (node),
                      OPNUM (node)));

        pto_t *symconst_pto = new_symconst_pto (node);
        set_node_pto (node, symconst_pto);
      }
    }
  } break;
  case (iro_Alloc): {
    type *tp = get_Alloc_type (node); /* debugging only */
    alloc_pto_t *alloc_pto = new_alloc_pto (node, n_ctxs);
    set_alloc_pto (node, alloc_pto);

    DBGPRINT (0, (stdout, "%s: %i names \"%s\" for alloc \"%s[%li]\"\n",
                  __FUNCTION__,
                  n_ctxs,
                  get_type_name (tp),
                  OPNAME (node),
                  OPNUM (node)));
  } break;

  case (iro_Load):
  case (iro_Call):
  case (iro_Phi):
  case (iro_Const):
    /* nothing --- handled by reset_node_pto on each pass */
    break;
  default: {
    /* nothing */
  } break;
  }
}


/* Initialise the given graph for a new pass run */
static void pto_init_graph_allocs (ir_graph *graph)
{
  graph_info_t *ginfo = ecg_get_info (graph);
  int n_ctxs = ginfo->n_ctxs;

  init_env_t *init_env = xmalloc (sizeof (init_env_t));
  init_env->n_ctxs = n_ctxs;

  HERE ("start");

  irg_walk_graph (graph, init_pto, NULL, init_env);

  memset (init_env, 0x00, sizeof (init_env_t));
  free (init_env);

  HERE ("end");
}

/* ===================================================
   Exported Implementation:
   =================================================== */
/* "Fake" the arguments to the main method */
void fake_main_args (ir_graph *graph)
{
  HERE ("start");

  entity *ent = get_irg_entity (graph);
  type *mtp = get_entity_type (ent);
  ir_node **args = find_irg_args (graph);
  type *ctp = get_method_param_type (mtp, 1); /* ctp == char[]*[]* */

  /* 'main' has signature 'void(int, char[]*[]*)' */
  assert (NULL == args [2]);

  assert (is_pointer_type (ctp));

  ctp = get_pointer_points_to_type (ctp); /* ctp == char[]*[] */

  assert (is_array_type (ctp));

  desc_t *arg_desc = new_name (ctp, args [1]);
  pto_t *arg_pto = new_pto (args [1]);
  /* todo: simulate 'store' to arg1[] ?!? */
  qset_insert (arg_pto->values, arg_desc);

  set_node_pto (args [1], arg_pto);

# ifdef TEST_MAIN_TYPE
  ctp = get_array_element_type (ctp); /* ctp == char[]* */

  assert (is_pointer_type (ctp));

  ctp = get_pointer_points_to_type (ctp); /* ctp == char[] */

  assert (is_array_type (ctp));

  ctp = get_array_element_type (ctp); /* ctp == char */

  assert (is_primitive_type (ctp));
# endif /* defined  TEST_MAIN_TYPE */

  HERE ("end");
}

/* Initialise the Init module */
void pto_init_init ()
{
  pto_obst = (struct obstack*) xmalloc (sizeof (struct obstack));

  obstack_init (pto_obst);
}

/* Cleanup the Init module */
void pto_init_cleanup ()
{
  obstack_free (pto_obst, NULL);
  memset (pto_obst, 0x00, sizeof (struct obstack));
  free (pto_obst);
  pto_obst = NULL;
}


/* Initialise the Names of the Types/Entities */
void pto_init_type_names ()
{
  HERE ("start");
  type_walk (clear_type_link, NULL, NULL);
  HERE ("end");
}

/* Initialise the given graph for a new pass run */
void pto_init_graph (ir_graph *graph)
{
  graph_info_t *ginfo = ecg_get_info (graph);
  const int n_ctxs = ginfo->n_ctxs;

  /* only for debugging stuff: */
  entity *ent = get_irg_entity (graph);
  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  DBGPRINT (0, (stdout, "%s: init \"%s.%s\" for %i ctxs\n", __FUNCTION__,
                own_name, ent_name, n_ctxs));

  HERE ("start");

  clear_graph_links     (graph);
  pto_init_graph_allocs (graph);

  HERE ("end");
}

/* Reset the given graph for a new pass run */
void pto_reset_graph_pto (ir_graph *graph, int ctx_idx)
{
  HERE ("start");

  reset_env_t *reset_env = (reset_env_t*) xmalloc (sizeof (reset_env_t));
  reset_env->ctx_idx = ctx_idx;

  irg_walk_graph (graph, reset_node_pto, NULL, reset_env);

  memset (reset_env, 0x00, sizeof (reset_env_t));
  free (reset_env);
  HERE ("end");
}


/*
  $Log$
  Revision 1.5  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.4  2004/11/20 21:21:56  liekweg
  Finalise initialisation

  Revision 1.3  2004/11/18 16:37:07  liekweg
  rewrite


*/
