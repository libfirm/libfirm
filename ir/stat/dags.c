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

#include "dags.h"

enum dag_counting_options_t {
  FIRMSTAT_COPY_CONSTANTS = 0x00000001,		/**< if set, constants will be treated as they are in
						     the same block as its successors */
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
  ir_node     *root;
  unsigned    num_roots;
  unsigned    num_nodes;
  unsigned    num_inner_nodes;
  unsigned    is_dead;
  dag_entry_t *next;
};

/**
 * a DAG Entry link
 */
typedef struct _dag_link_t {
  dag_entry_t	*entry;
} dag_link_t;

/**
 * walker for connecting DAGs and counting.
 */
static void connect_dags(ir_node *node, void *env)
{
  dag_env_t  *dag_env = env;
  int        i, arity;
  ir_node    *block;
  dag_link_t *link;

  if (is_Block(node))
    return;

  block = get_nodes_block(node);

  /* ignore start end end blocks */
  if (block == get_irg_start_block(current_ir_graph) ||
      block == get_irg_end_block(current_ir_graph))
    return;

  link = get_irn_link(node);

  if (! link) {
    /* not assigned node found, maybe a new root */
    dag_entry_t *entry = obstack_alloc(&dag_env->obst, sizeof(*entry));

    link        = obstack_alloc(&dag_env->obst, sizeof(*link));
    link->entry = entry;

    entry->num_nodes       = 1;
    entry->num_roots       = 1;
    entry->num_inner_nodes = 0;
    entry->root            = node;
    entry->is_dead         = 0;
    entry->next            = dag_env->list_of_dags;

    ++dag_env->num_of_dags;
    dag_env->list_of_dags = entry;

    set_irn_link(node, link);
  }

  /* put the predecessors into the same DAG as the current */
  for (i = 0, arity = get_irn_arity(node); i < arity; ++i) {
    ir_node *prev      = get_irn_n(node, i);
    int     special    = 0;

    /*
     * copy constants if requested into the DAG's
     * beware, do NOT add a link, as this will result in
     * wrong intersections
     */
    if (dag_env->options & FIRMSTAT_COPY_CONSTANTS) {
      if (get_irn_op(prev) == op_Const || get_irn_op(prev) == op_SymConst) {
	++link->entry->num_nodes;
	++link->entry->num_inner_nodes;
      }
    }

    if (get_nodes_block(prev) == block) {
      dag_link_t *prev_link = get_irn_link(prev);

      if (! prev_link) {
	/* not assigned node, do it */
	set_irn_link(prev, link);
	++link->entry->num_nodes;
	++link->entry->num_inner_nodes;
      }
      else if (prev_link->entry != link->entry) {
	/* two DAGs intersect */

	assert(link->entry);

	link->entry->num_roots       += prev_link->entry->num_roots;
	link->entry->num_nodes       += prev_link->entry->num_nodes;
	link->entry->num_inner_nodes += prev_link->entry->num_inner_nodes;

	--dag_env->num_of_dags;

	prev_link->entry->is_dead = 1;
	prev_link->entry          = link->entry;
      }
    }
  }
}

/**
 * count the DAG's size of a graph
 */
void count_dags_in_graph(graph_entry_t *global, graph_entry_t *graph)
{
  dag_env_t   root_env;
  dag_entry_t *entry;

  /* do NOT check the const code irg */
  if (graph->irg == get_const_code_irg())
    return;

  /* first step, clear the links */
  irg_walk_graph(graph->irg, clear_links, NULL, NULL);

  obstack_init(&root_env.obst);
  root_env.num_of_dags  = 0;
  root_env.list_of_dags = NULL;
  root_env.options      = FIRMSTAT_COPY_CONSTANTS;

  /* count them */
  irg_walk_graph(graph->irg, connect_dags, NULL, &root_env);

  printf("Graph %p %s --- %d\n", graph->irg, get_entity_name(get_irg_entity(graph->irg)),
      root_env.num_of_dags);

  for (entry = root_env.list_of_dags; entry; entry = entry->next) {
    if (entry->is_dead)
      continue;

    printf("number of roots %d number of nodes %d inner %d %d\n",
      entry->num_roots,
      entry->num_nodes,
      entry->num_inner_nodes,
      get_irn_node_nr(entry->root));
  }

  obstack_free(&root_env.obst, NULL);
}
