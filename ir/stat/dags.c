/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Statistics for Firm. DAG's in graphs.
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "irprintf.h"
#include "irdump.h"
#include "dags.h"
#include "irtools.h"

enum dag_counting_options_t {
	FIRMSTAT_COPY_CONSTANTS = 0x00000001,  /**< if set, constants will be treated as they are in
	                                            the same block as its successors */
	FIRMSTAT_LOAD_IS_LEAVE  = 0x00000002,  /**< Load nodes are always leaves */
	FIRMSTAT_CALL_IS_LEAVE  = 0x00000004,  /**< Call nodes are always leaves */
	FIRMSTAT_ARGS_ARE_ROOTS = 0x00000008,  /**< arguments (Proj(Proj(Start)) are roots */
};

typedef struct _dag_entry_t dag_entry_t;

/**
 * Environment for connecting DAG's
 */
typedef struct _dag_env_t {
	struct obstack obst;
	unsigned       num_of_dags;   /**< Number of found DAGs so far. */
	dag_entry_t    *list_of_dags; /**< List of found DAGs. */
	unsigned       options;       /**< DAG counting options. */
} dag_env_t;

/**
 * a DAG Entry
 */
struct _dag_entry_t {
	unsigned    id;               /**< assigned ID for this DAG */
	ir_node     *root;            /**< one root of the DAG */
	unsigned    num_roots;        /**< number of root nodes in the DAG */
	unsigned    num_nodes;        /**< overall number of nodes in the DAG */
	unsigned    num_inner_nodes;  /**< number of inner nodes in the DAG */
	unsigned    is_dead:1;        /**< marks a dead entry */
	unsigned    is_tree:1;        /**< True if this DAG is a tree. */
	unsigned    is_ext_ref:1;     /**< True if this DAG is external referenced, so it cannot be combined. */
	dag_entry_t *next;            /**< link all entries of a DAG */
	dag_entry_t *link;            /**< if set, this entry is an ID */
};

/**
 * return an DAG entry for the node n
 */
static dag_entry_t *get_irn_dag_entry(ir_node *n)
{
	dag_entry_t *p = get_irn_link(n);

	if (p) {
		/* skip any dead links */
		if (p->link) {
			do {
				p = p->link;
			} while (p->link != NULL);
			set_irn_link(n, p);
		}
	}  /* if */
	return p;
}  /* get_irn_dag_entry */

#define set_irn_dag_entry(n, e) set_irn_link(n, e)

/**
 * checks whether a node is an arg
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
}  /* is_arg */

/**
 * Allocate a new DAG entry.
 */
static dag_entry_t *new_dag_entry(dag_env_t *dag_env, ir_node *node) {
	dag_entry_t *entry = obstack_alloc(&dag_env->obst, sizeof(*entry));

	entry->num_nodes       = 1;
	entry->num_roots       = 1;
	entry->num_inner_nodes = 0;
	entry->root            = node;
	entry->is_dead         = 0;
	entry->is_tree         = 1;
	entry->is_ext_ref      = 0;
	entry->next            = dag_env->list_of_dags;
	entry->link            = NULL;

	++dag_env->num_of_dags;
	dag_env->list_of_dags = entry;

	set_irn_dag_entry(node, entry);
	return entry;
}  /* new_dag_entry */

/**
 * Post-walker to detect DAG roots that are referenced form other blocks
 */
static void find_dag_roots(ir_node *node, void *env)
{
	dag_env_t   *dag_env = env;
	int         i, arity;
	dag_entry_t *entry;
	ir_node     *block;

	if (is_Block(node))
		return;

	block = get_nodes_block(node);

	/* ignore start end end blocks */
	if (block == get_irg_start_block(current_ir_graph) ||
		block == get_irg_end_block(current_ir_graph)) {
		return;
	}  /* if */

	/* Phi nodes always references nodes from "other" block */
	if (is_Phi(node)) {
		if (get_irn_mode(node) != mode_M) {
			for (i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				ir_node *prev = get_irn_n(node, i);

				if (is_Phi(prev))
					continue;

				if (dag_env->options & FIRMSTAT_COPY_CONSTANTS) {
					if (is_irn_constlike(prev))
						continue;
				}  /* if */

				entry = get_irn_dag_entry(prev);

				if (! entry) {
					/* found an unassigned node, a new root */
					entry = new_dag_entry(dag_env, node);
					entry->is_ext_ref = 1;
				}  /* if */
			}  /* for */
		}  /* if */
	} else {

		for (i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				ir_node *prev = get_irn_n(node, i);
				ir_mode *mode = get_irn_mode(prev);

				if (mode == mode_X || mode == mode_M)
					continue;

				if (is_Phi(prev))
					continue;

				if (dag_env->options & FIRMSTAT_COPY_CONSTANTS) {
					if (is_irn_constlike(prev))
						continue;
				}  /* if */

				if (get_nodes_block(prev) != block) {
					/* The predecessor is from another block. It forms
					   a root. */
					entry = get_irn_dag_entry(prev);
					if (! entry) {
						/* found an unassigned node, a new root */
						entry = new_dag_entry(dag_env, node);
						entry->is_ext_ref = 1;
					}  /* if */
				}  /* if */
			}  /* for */
	}  /* if */
}  /* find_dag_roots */

/**
 * Pre-walker for connecting DAGs and counting.
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
		block == get_irg_end_block(current_ir_graph)) {
		return;
	}  /* if */

	/* ignore Phi nodes */
	if (is_Phi(node))
		return;

	if (dag_env->options & FIRMSTAT_ARGS_ARE_ROOTS && is_arg(node))
		return;

	mode = get_irn_mode(node);
	if (mode == mode_X || mode == mode_M) {
		/* do NOT count mode_X and mode_M nodes */
		return;
	}  /* if */

	/* if this option is set, Loads are always leaves */
	if (dag_env->options & FIRMSTAT_LOAD_IS_LEAVE && get_irn_op(node) == op_Load)
		return;

	if (dag_env->options & FIRMSTAT_CALL_IS_LEAVE && get_irn_op(node) == op_Call)
		return;

	entry = get_irn_dag_entry(node);

	if (! entry) {
		/* found an unassigned node, maybe a new root */
		entry = new_dag_entry(dag_env, node);
	}  /* if */

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
			if (is_irn_constlike(prev)) {
				++entry->num_nodes;
				++entry->num_inner_nodes;
			}  /* if */
		}  /* if */

		/* only nodes from the same block goes into the DAG */
		if (get_nodes_block(prev) == block) {
			dag_entry_t *prev_entry = get_irn_dag_entry(prev);

			if (! prev_entry) {
				/* not assigned node, put it into the same DAG */
				set_irn_dag_entry(prev, entry);
				++entry->num_nodes;
				++entry->num_inner_nodes;
			} else {
				if (prev_entry == entry) {
					/* We found a node that is already assigned to this DAG.
					   This DAG is not a tree. */
					entry->is_tree = 0;
				} else {
					/* two DAGs intersect: copy the data to one of them
					   and kill the other */
					entry->num_roots       += prev_entry->num_roots;
					entry->num_nodes       += prev_entry->num_nodes;
					entry->num_inner_nodes += prev_entry->num_inner_nodes;
					entry->is_tree         &= prev_entry->is_tree;

					--dag_env->num_of_dags;

					prev_entry->is_dead = 1;
					prev_entry->link    = entry;
				}  /* if */
			}  /* if */
		}  /* if */
	}  /* for */
}  /* connect_dags */

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
	}  /* if */

	entry = get_irn_dag_entry(n);
	if (! entry)
		return DEFAULT_RET;

	fprintf(F, "color: %s info3: \"DAG id: %u\"", colors[entry->id & 7], entry->id);

	/* I know the color! */
	return COLOR_RET;
}  /* stat_dag_mark_hook */

/**
 * count the DAG's size of a graph
 *
 * @param global  the global entry
 * @param graph   the current graph entry
 */
void count_dags_in_graph(graph_entry_t *global, graph_entry_t *graph)
{
	dag_env_t   root_env;
	dag_entry_t *entry;
	unsigned id;
	(void) global;

	/* do NOT check the const code irg */
	if (graph->irg == get_const_code_irg())
		return;

	/* first step, clear the links */
	irg_walk_graph(graph->irg, firm_clear_link, NULL, NULL);

	obstack_init(&root_env.obst);
	root_env.num_of_dags  = 0;
	root_env.list_of_dags = NULL;
	root_env.options      = FIRMSTAT_COPY_CONSTANTS | FIRMSTAT_LOAD_IS_LEAVE | FIRMSTAT_CALL_IS_LEAVE;

	/* find the DAG roots that are referenced from other block */
	irg_walk_graph(graph->irg, NULL, find_dag_roots, &root_env);

	/* connect and count them */
	irg_walk_graph(graph->irg, connect_dags, NULL, &root_env);

	printf("Graph %p %s --- %d\n", (void *)graph->irg, get_entity_name(get_irg_entity(graph->irg)),
		root_env.num_of_dags);

	for (id = 0, entry = root_env.list_of_dags; entry; entry = entry->next) {
		if (entry->is_dead)
			continue;
		entry->id = id++;

		printf("number of roots %d number of nodes %d inner %d tree %u %ld\n",
			entry->num_roots,
			entry->num_nodes,
			entry->num_inner_nodes,
			entry->is_tree,
			get_irn_node_nr(entry->root));
	}  /* for */

#if 1
	/* dump for test */
	mark_options = root_env.options;
	set_dump_node_vcgattr_hook(stat_dag_mark_hook);
	dump_ir_block_graph(graph->irg, "-dag");
	set_dump_node_vcgattr_hook(NULL);
#endif

	assert(id == root_env.num_of_dags);

	obstack_free(&root_env.obst, NULL);
}  /* count_dags_in_graph */
