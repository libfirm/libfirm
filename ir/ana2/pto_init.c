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

# include "pto_init.h"
# include "pto_debug.h"
# include "pto_comp.h"

# include "typewalk.h"
# include "irgwalk.h"
# include "xmalloc.h"

/* Local Defines: */

/* Local Data Types: */
typedef struct init_env_str
{
  int n_ctxs;
} init_env_t;

/* Local Variables: */

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */
/* Allocate a new pto */
static pto_t *new_pto (ir_node *node)
{
  /* dummy implementation for fake_pto */
  pto_t *pto = xmalloc (sizeof (pto_t));

  return (pto);
}

/* Allocate a new alloc_pto */
static alloc_pto_t *new_alloc_pto (ir_node *node, int n_ctxs)
{
  int i;
  /* dummy implementation for fake_pto */
  alloc_pto_t *alloc_pto = xmalloc (sizeof (alloc_pto_t));

  alloc_pto->ptos = (pto_t**) xmalloc (n_ctxs * sizeof (pto_t*));

  for (i = 0; i < n_ctxs; i ++) {
    alloc_pto->ptos [i] = new_pto (node);
  }

  return (alloc_pto);
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

/* Temporary fix until we get 'real' ptos: Allocate some dummy for pto */
static void fake_pto (ir_node *node, void *env)
{
  init_env_t *init_env = (init_env_t*) env;
  int n_ctxs = init_env->n_ctxs;

  const opcode op = get_irn_opcode (node);

  switch (op) {
  case (iro_Load):
  case (iro_SymConst):
  case (iro_Const):
  case (iro_Call):
  case (iro_Phi): {
    pto_t *pto = new_pto (node);
    set_node_pto (node, pto);
    } break;

  case (iro_Alloc): {
    alloc_pto_t *alloc_pto = new_alloc_pto (node, n_ctxs);
    set_alloc_pto (node, alloc_pto);

    } break;
  default:
    HERE ("no pto");
  }
}

/* ===================================================
   Exported Implementation:
   =================================================== */
/* Initialise the Names of the Types/Entities */
void pto_init_type_names ()
{
  HERE ("start");
  type_walk (clear_type_link, NULL, NULL);
}

/* Initialise the given graph for a new pass run */
void pto_init_graph (ir_graph *graph)
{
  graph_info_t *ginfo = ecg_get_info (graph);
  int n_ctxs = ginfo->n_ctxs;

  init_env_t *init_env = xmalloc (sizeof (init_env_t));
  init_env->n_ctxs = n_ctxs;

  HERE ("start");

  clear_graph_links (graph);

  entity *ent = get_irg_entity (graph);

  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  DBGPRINT (0, (stdout, "%s: init \"%s.%s\" for %i ctxs\n", __FUNCTION__,
                own_name, ent_name, n_ctxs));

  irg_walk_graph (graph, fake_pto, NULL, init_env);

  HERE ("end");
}

/* Set all alloc names to the right ptos for a new pass run */
void pto_init_allocs (graph_info_t *ginfo, int ctx_idx)
{
  assert (NULL != ginfo);

  alloc_info_t *ainfo = ginfo->allocs;

  HERE ("start");

  while (NULL != ainfo) {
    ir_node *alloc = ainfo->alloc;
    alloc_pto_t *alloc_pto = (alloc_pto_t*) get_irn_link (alloc);

    alloc_pto->curr_pto = alloc_pto->ptos [ctx_idx];

    DBGPRINT (0, (stdout, "%s:%i (%s[%li]): ctx_idx = %i\n",
                  __FUNCTION__, __LINE__, OPNAME (alloc), OPNUM (alloc), ctx_idx));

    ainfo = ainfo->prev;
  }

  HERE ("end");
}


/*
  $Log$
  Revision 1.3  2004/11/18 16:37:07  liekweg
  rewrite


*/
