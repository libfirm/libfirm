/*
 * Project:     libFIRM
 * File name:   ir/ir/dags.c
 * Purpose:     Statistics for Firm. DAG's in graphs.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "irprintf.h"
#include "irdump.h"
#include "dags.h"

enum dag_counting_options_t {
  FIRMSTAT_COPY_CONSTANTS = 0x00000001,		/**< if set, constants will be treated as they are in
						     the same block as its successors */
  FIRMSTAT_LOAD_IS_LEAVE  = 0x00000002,         /**< Load nodes are always leaves */
  FIRMSTAT_CALL_IS_LEAVE  = 0x00000004,         /**< Call nodes are always leaves */
  FIRMSTAT_ARGS_ARE_ROOTS = 0x00000008,         /**< arguments (Proj(Proj(Start)) are roots */
};

/**
 * walker for clearing node links
 */
static void clear_links(ir_node *node, void *env)
{
  set_irn_link(node, NULL);
}

typedef struct _dag_entry_t dag_entry_t;

/**
 * Environment for connecting DAG's
 */
typedef struct _dag_env_t {
  struct obstack obst;
  unsigned       num_of_dags;
  dag_entry_t    *list_of_dags;
  unsigned       options;		/**< DAG counting options */
} dag_env_t;

/**
 * a DAG Entry
 */
struct _dag_entry_t {
  unsigned    id;                       /**< assigned ID for this DAG */
  ir_node     *root;                    /**< one root of the DAG */
  unsigned    num_roots;                /**< number of root nodes in the DAG */
  unsigned    num_nodes;                /**< overall number of nodes in the DAG */
  unsigned    num_inner_nodes;          /**< number of inner nodes in the DAG */
  unsigned    is_dead;                  /**< marks a dead entry */
  dag_entry_t *next;                    /**< link all entries of a DAG */
  dag_entry_t *link;                    /**< if set, this entry is an ID */
};

/**
 * return an DAG entry for the node n
 */
static dag_entry_t *get_irn_dag_entry(ir_node *n)
{
  dag_entry_t *res = get_irn_link(n);

  if (res) {
    dag_entry_t *p;

    for (p = res; p->link; p = p->link);

    if (p != res)
      set_irn_link(n, p);

    return p;
  }
  return NULL;
}

#define set_irn_dag_entry(n, e) set_irn_link(n, e)

/**
 * checks wether a node is an arg
 */
static int is_arg(ir_node *node)
{
  if (! is_Proj(node))
    return 0;

  node = get_Proj_pred(node);
  if (! is_Proj(node))
    return 0;

  node = get_Proj_pred(node);
  return get_irn_op(node) == op_Start;
}

/**
 * walker for connecting DAGs and counting.
 */
static void connect_dags(ir_node *node, void *env)
{
  dag_env_t   *dag_env = env;
  int         i, arity;
  ir_node     *block;
  dag_entry_t *entry;
  ir_mode     *mode;

  if (is_Block(node))
    return;

  block = get_nodes_block(node);

  /* ignore start end end blocks */
  if (block == get_irg_start_block(current_ir_graph) ||
      block == get_irg_end_block(current_ir_graph))
    return;

  if (is_Phi(node))
    return;

  if (dag_env->options & FIRMSTAT_ARGS_ARE_ROOTS && is_arg(node))
    return;

  mode = get_irn_mode(node);
  if (mode == mode_X || mode == mode_M) {
    /* do NOT count mode_X nodes */
    return;
  }

  entry = get_irn_dag_entry(node);

  if (! entry) {
    /* found a not assigned node, maybe a new root */
    entry = obstack_alloc(&dag_env->obst, sizeof(*entry));

    entry->num_nodes       = 1;
    entry->num_roots       = 1;
    entry->num_inner_nodes = 0;
    entry->root            = node;
    entry->is_dead         = 0;
    entry->next            = dag_env->list_of_dags;
    entry->link            = NULL;

    ++dag_env->num_of_dags;
    dag_env->list_of_dags = entry;

    set_irn_dag_entry(node, entry);
  }

  /* if this option is set, Loads are allways leaves */
  if (dag_env->options & FIRMSTAT_LOAD_IS_LEAVE && get_irn_op(node) == op_Load)
    return;

  if (dag_env->options & FIRMSTAT_CALL_IS_LEAVE && get_irn_op(node) == op_Call)
    return;

  /* put the predecessors into the same DAG as the current */
  for (i = 0, arity = get_irn_arity(node); i < arity; ++i) {
    ir_node *prev = get_irn_n(node, i);
    ir_mode *mode = get_irn_mode(prev);

    if (is_Phi(prev))
      continue;

    if (mode == mode_X || mode == mode_M)
      continue;

    /*
     * copy constants if requested into the DAG's
     * beware, do NOT add a link, as this will result in
     * wrong intersections
     */
    if (dag_env->options & FIRMSTAT_COPY_CONSTANTS) {
      if (get_irn_op(prev) == op_Const || get_irn_op(prev) == op_SymConst) {
	++entry->num_nodes;
	++entry->num_inner_nodes;
      }
    }

    /* only nodes from the same block goes into the DAG */
    if (get_nodes_block(prev) == block) {
      dag_entry_t *prev_entry = get_irn_dag_entry(prev);

      if (! prev_entry) {
	/* not assigned node, put it into the same DAG */
	set_irn_dag_entry(prev, entry);
	++entry->num_nodes;
	++entry->num_inner_nodes;
      }
      else {
        if (prev_entry != entry) {

          /* two DAGs intersect */
          entry->num_roots       += prev_entry->num_roots;
          entry->num_nodes       += prev_entry->num_nodes;
          entry->num_inner_nodes += prev_entry->num_inner_nodes;

          --dag_env->num_of_dags;

          prev_entry->is_dead = 1;
          prev_entry->link    = entry;
        }
      }
    }
  }
}

#define DEFAULT_RET     1
#define COLOR_RET       1

static unsigned mark_options;

/**
 * a vcg attribute hook
 */
static int stat_dag_mark_hook(FILE *F, ir_node *n, ir_node *l)
{
  static const char *colors[] = { "purple", "pink", "lightblue", "orange", "khaki", "orchid", "lilac", "turquoise" };
  dag_entry_t *entry;

  /* do not count Bad / NoMem */
  if (l) {
    ir_op *op = get_irn_op(l);

    if (op == op_NoMem || op == op_Bad)
      return DEFAULT_RET;

    /* check for additional options */
    op = get_irn_op(n);

    if (mark_options & FIRMSTAT_LOAD_IS_LEAVE && op == op_Load)
      return DEFAULT_RET;

    if (mark_options & FIRMSTAT_CALL_IS_LEAVE && op == op_Call)
      return DEFAULT_RET;
  }

  entry = get_irn_dag_entry(n);
  if (! entry)
    return DEFAULT_RET;

  fprintf(F, "color: %s info3: \"DAG id: %u\"", colors[entry->id & 7], entry->id);

  /* I know the color! */
  return COLOR_RET;
}

/**
 * count the DAG's size of a graph
 */
void count_dags_in_graph(graph_entry_t *global, graph_entry_t *graph)
{
  dag_env_t   root_env;
  dag_entry_t *entry;
  unsigned id;

  /* do NOT check the const code irg */
  if (graph->irg == get_const_code_irg())
    return;

  /* first step, clear the links */
  irg_walk_graph(graph->irg, clear_links, NULL, NULL);

  obstack_init(&root_env.obst);
  root_env.num_of_dags  = 0;
  root_env.list_of_dags = NULL;
  root_env.options      = FIRMSTAT_COPY_CONSTANTS | FIRMSTAT_LOAD_IS_LEAVE | FIRMSTAT_CALL_IS_LEAVE;

  /* count them */
  irg_walk_graph(graph->irg, connect_dags, NULL, &root_env);

  printf("Graph %p %s --- %d\n", graph->irg, get_entity_name(get_irg_entity(graph->irg)),
      root_env.num_of_dags);

  for (id = 0, entry = root_env.list_of_dags; entry; entry = entry->next) {
    if (entry->is_dead)
      continue;
    entry->id = id++;

    printf("number of roots %d number of nodes %d inner %d %ld\n",
      entry->num_roots,
      entry->num_nodes,
      entry->num_inner_nodes,
      get_irn_node_nr(entry->root));
  }

#if 1
  /* dump for test */
  mark_options = root_env.options;
  set_dump_node_vcgattr_hook(stat_dag_mark_hook);
  dump_ir_block_graph(graph->irg, "-dag");
  set_dump_node_vcgattr_hook(NULL);
#endif

  assert(id == root_env.num_of_dags);

  obstack_free(&root_env.obst, NULL);
}
