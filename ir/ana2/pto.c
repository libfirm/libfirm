/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana/pto.c
 * Purpose:     Pto
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif

# include "pto.h"

# include "irnode_t.h"
# include "irprog_t.h"

# include "eset.h"
# include "irgwalk.h"
# include "irgmod.h"
# include "irvrfy.h"
# include "trvrfy.h"
# include "xmalloc.h"

# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

typedef struct walk_mem_env_str {
  ir_graph *graph;              /* the graph we're visiting */
  int visited;                  /* 'visited' marker */
  irg_walk_func *pre;           /* pre action */
  irg_walk_func *post;          /* post action */
  void *env;                    /* user-defined environment */

  struct walk_mem_env_str *prev; /* link up walking instances */
  /* what else? */
} walk_mem_env_t;

/* Link up walking instances */
static walk_mem_env_t *walk_envs = NULL;

/* MEMORY WALK */

/* BEGIN TEST */
static void print_node_pre (ir_node *node, void *__unused)
{
  fprintf (stdout, "PRE  MEM Node (0x%08x) (%s)\n",
           (int)  node,
           get_op_name (get_irn_op (node)));
}

static void print_node_post (ir_node *node, void *__unused)
{
  const opcode op = get_irn_opcode (node);


  if (iro_Call == op) {
    entity *ent = NULL;
    ir_graph *graph = NULL;
    fprintf (stdout, "POST MEM Call Node (0x%08x)\n",
             (int) node);

    ir_node *ptr = get_Call_ptr (node);

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    } else if (iro_SymConst == get_irn_opcode (ptr)) {
      if (get_SymConst_kind(ptr) == symconst_addr_ent) {
        ent = get_SymConst_entity (ptr);
      }
    }

    if (NULL != ent) {
      graph = get_entity_irg (ent);
      if (NULL != graph) {
        if (! get_irg_is_mem_visited (graph)) {

          fprintf (stdout, " -> visit  graph (0x%08x) of \"%s.%s\"\n",
                   (int) graph,
                   get_type_name (get_entity_owner (get_irg_entity (graph))),
                   get_entity_name (get_irg_entity (graph)));

          irg_walk_mem (graph, print_node_pre, print_node_post, NULL);
        }
      }
    }

  } else {
    fprintf (stdout, "POST MEM Node (0x%08x) (%s)\n",
             (int)  node,
             get_op_name (get_irn_op (node)));
  }

}


/* END TEST */

/*
   See whether the given graph is being visited right now.
*/
int get_irg_is_mem_visited (ir_graph *graph)
{
  walk_mem_env_t *walk_env = walk_envs;

  while (NULL != walk_env) {
    if (graph == walk_env->graph) {
      return (TRUE);
    }

    walk_env = walk_env->prev;
  }

  return (FALSE);
}

/*
  Walk over the firm nodes of a graph via the memory edges (only)
  starting from a node that has a memory input.
*/
void irg_walk_mem_node (ir_node *node,
                        walk_mem_env_t *walk_env)
{
  const opcode op = get_irn_opcode (node);
  ir_node *in = NULL;

  if (get_irn_visited (node) >= walk_env->visited) {
    return;
  } else {
    set_irn_visited (node, walk_env->visited + 1);
  }

  fprintf (stdout, "Node (0x%08x).op = %s\n", (int)
           node,
           get_op_name (get_irn_op (node)));

  if (NULL != walk_env->pre) {
    walk_env->pre (node, walk_env->env);
  }

  switch (op) {
  case (iro_Start): {
  } break;
  case (iro_Load): {
    in = get_Load_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Store): {
    in = get_Store_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Alloc): {
    in = get_Alloc_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Free): {
    in = get_Free_mem (node);
    /* WTF? */
    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Raise): {
    in = get_Raise_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Sel): {
    in = get_Sel_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Call): {
    in = get_Call_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Return): {
    in = get_Return_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Proj): {
    in = get_Proj_pred (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Phi): {
    int i;
    int n_ins = get_irn_arity (node);


    for (i = 0; i < n_ins; i ++) {
      in = get_irn_n (node, i);

      irg_walk_mem_node (in, walk_env);
    }
  } break;
  case (iro_Div): {
    in = get_Div_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Quot): {
    in = get_Quot_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_Mod): {
    in = get_Mod_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  case (iro_DivMod): {
    in = get_DivMod_mem (node);

    irg_walk_mem_node (in, walk_env);
  } break;
  default: {
    assert (0 && "something not handled");
  }
  }

  if (NULL != walk_env->post) {
    walk_env->post (node, walk_env->env);
  }
}

/*
  Walk over the nodes of the given graph via the memory edges (only).
  Each graph can only be subject to this walk once at any given time.
*/
void irg_walk_mem (ir_graph *graph,
                   irg_walk_func *pre, irg_walk_func *post,
                   void *env)
{
  int i;
  ir_node *ret = NULL;
  ir_node *end = get_irg_end_block (graph);
  int n_ins;
  walk_mem_env_t *walk_env = (walk_mem_env_t*) xmalloc (sizeof (walk_mem_env_t));

  assert (! get_irg_is_mem_visited (graph));

  walk_env->graph = graph;
  inc_irg_visited (walk_env->graph);
  walk_env->visited = get_irg_visited (graph);

  walk_env->prev = walk_envs;
  walk_envs = walk_env;

  walk_env->pre = pre;
  walk_env->post = post;
  walk_env->env  = env;

  /* 'graph' is not actually being visited right now, but it should be reported that way */
  assert (get_irg_is_mem_visited (graph));

  /* all return nodes */
  n_ins = get_irn_arity (end);
  for (i = 0; i < n_ins; i ++) {
    ret = get_irn_n (end, i);

    irg_walk_mem_node (ret, walk_env);
  }

  /*
    The end NODE sometimes has some more ins. not sure whether we need to walk them.
  */

  /* allow only properly nested calls right now */
  assert (walk_envs == walk_env);
  walk_envs = walk_envs->prev;

  free (walk_env);

  assert (! get_irg_is_mem_visited (graph));
}

/*
   Test irg_walk_mem
*/
void pto_test_mem ()
{
  int i;

  fprintf (stdout, "START PTO TEST\n");

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_graph *graph = get_irp_irg (i);

    fprintf (stdout, "START GRAPH (0x%08x) of \"%s.%s\"\n",
             (int) graph,
             get_type_name (get_entity_owner (get_irg_entity (graph))),
             get_entity_name (get_irg_entity (graph)));
    irg_walk_mem (graph, print_node_pre, print_node_post, NULL);
    fprintf (stdout, "END   GRAPH (0x%08x)\n", (int) graph);
  }

  fprintf (stdout, "END   PTO TEST\n");
}


/*
 * $Log$
 * Revision 1.1  2004/10/20 14:59:42  liekweg
 * Added ana2, added ecg and pto
 *
 */
