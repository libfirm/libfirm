/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief   Functions for traversing ir graphs
 * @author  Boris Boesler, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 * @brief
 *  traverse an ir graph
 *  - execute the pre function before recursion
 *  - execute the post function after recursion
 */
#include "config.h"

#include <stdlib.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "entity_t.h"

#include "error.h"
#include "pset_new.h"
#include "array.h"

/**
 * specialized version of irg_walk_2, called if only pre callback exists
 *
 * @return number of visited nodes
 */
static unsigned irg_walk_2_pre(ir_node *node, irg_walk_func *pre, void *env)
{
	int i;
	unsigned cnt = 1;
	ir_graph *irg = get_irn_irg(node);

	set_irn_visited(node, irg->visited);

	pre(node, env);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_pre(pred, pre, env);
	}
	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_pre(pred, pre, env);
	}
	return cnt;
}

/**
 * specialized version of irg_walk_2, called if only post callback exists
 *
 * @return number of visited nodes
 */
static unsigned irg_walk_2_post(ir_node *node, irg_walk_func *post, void *env)
{
	int i;
	unsigned cnt = 1;
	ir_graph *irg = get_irn_irg(node);

	set_irn_visited(node, irg->visited);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_post(pred, post, env);
	}
	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_post(pred, post, env);
	}

	post(node, env);

	return cnt;
}

/**
 * specialized version of irg_walk_2, called if pre and post callbacks exist
 *
 * @return number of visited nodes
 */
static unsigned irg_walk_2_both(ir_node *node, irg_walk_func *pre,
                                irg_walk_func *post, void *env)
{
	int i;
	unsigned cnt = 1;
	ir_graph *irg = get_irn_irg(node);

	set_irn_visited(node, irg->visited);

	pre(node, env);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_both(pred, pre, post, env);
	}
	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_both(pred, pre, post, env);
	}

	post(node, env);

	return cnt;
}

/**
 * Intraprozedural graph walker.
 *
 * @return number of visited nodes
 */
unsigned irg_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                    void *env)
{
	if (irn_visited(node))
		return 0;

	if      (!post) return irg_walk_2_pre (node, pre, env);
	else if (!pre)  return irg_walk_2_post(node, post, env);
	else            return irg_walk_2_both(node, pre, post, env);
}

/* a counter */
static unsigned nodes_touched = 0;

void irg_walk_core(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                   void *env)
{
	assert(is_ir_node(node));
	nodes_touched = irg_walk_2(node, pre, post, env);
}

void irg_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
              void *env)
{
	ir_graph *irg = get_irn_irg(node);
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	irg_walk_core(node, pre, post, env);
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
	current_ir_graph = rem;
}

/*
 * walk over a graph
 */
void irg_walk_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	ir_graph * rem = current_ir_graph;

	hook_irg_walk(irg, (generic_func *)pre, (generic_func *)post);
	current_ir_graph = irg;
	irg_walk(get_irg_end(irg), pre, post, env);
	irg->estimated_node_count = nodes_touched;
	current_ir_graph = rem;
}

/* Executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
   Sets current_ir_graph properly for each walk.  Conserves current
   current_ir_graph. */
void all_irg_walk(irg_walk_func *pre, irg_walk_func *post, void *env)
{
	size_t i, n;
	ir_graph *irg;

	for (i = 0, n = get_irp_n_irgs(); i < n; i++) {
		irg = get_irp_irg(i);
		irg_walk_graph(irg, pre, post, env);
	}
}

/***************************************************************************/

/**
 * specialized version of irg_walk_in_or_dep_2, called if only pre callback exists
 *
 * @return number of visited nodes
 */
static unsigned irg_walk_in_or_dep_2_pre(ir_node *node, irg_walk_func *pre, void *env)
{
	int i;
	unsigned cnt = 1;
	ir_graph *irg = get_irn_irg(node);

	set_irn_visited(node, irg->visited);

	pre(node, env);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_pre(pred, pre, env);
	}
	for (i = get_irn_ins_or_deps(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_in_or_dep(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_pre(pred, pre, env);
	}
	return cnt;
}

/**
 * specialized version of irg_walk_in_or_dep_2, called if only post callback exists
 *
 * @return number of visited nodes
 */
static unsigned irg_walk_in_or_dep_2_post(ir_node *node, irg_walk_func *post, void *env)
{
	int i;
	unsigned cnt = 1;
	ir_graph *irg = get_irn_irg(node);

	set_irn_visited(node, irg->visited);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_post(pred, post, env);
	}
	for (i = get_irn_ins_or_deps(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_in_or_dep(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_post(pred, post, env);
	}

	post(node, env);

	return cnt;
}

/**
 * specialized version of irg_walk_in_or_dep_2, called if pre and post callbacks exist
 *
 * @return number of visited nodes
 */
static unsigned irg_walk_in_or_dep_2_both(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	int i;
	unsigned cnt = 1;
	ir_graph *irg = get_irn_irg(node);

	set_irn_visited(node, irg->visited);

	pre(node, env);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_both(pred, pre, post, env);
	}
	for (i = get_irn_ins_or_deps(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_in_or_dep(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_both(pred, pre, post, env);
	}

	post(node, env);

	return cnt;
}

/**
 * Intraprozedural graph walker. Follows dependency edges as well.
 *
 * @return number of visited nodes
 */
static unsigned irg_walk_in_or_dep_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	if (irn_visited(node))
		return 0;

	if      (! post) return irg_walk_in_or_dep_2_pre (node, pre, env);
	else if (! pre)  return irg_walk_in_or_dep_2_post(node, post, env);
	else             return irg_walk_in_or_dep_2_both(node, pre, post, env);
}

/*
 * Generic graph walker. Follows dependency edges as well.
 */
void irg_walk_in_or_dep(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	assert(is_ir_node(node));

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(current_ir_graph);
	nodes_touched = irg_walk_in_or_dep_2(node, pre, post, env);
	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
}

/*
 * Walk over a graph. Follow all edges (including dependencies)
 */
void irg_walk_in_or_dep_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	ir_graph * rem = current_ir_graph;

	hook_irg_walk(irg, (generic_func *)pre, (generic_func *)post);
	current_ir_graph = irg;
	irg_walk_in_or_dep(get_irg_end(irg), pre, post, env);
	irg->estimated_node_count = nodes_touched;
	current_ir_graph = rem;
}

/***************************************************************************/

/* Walks back from n until it finds a real cf op. */
static ir_node *get_cf_op(ir_node *n)
{
	while (!is_cfop(n) && !is_fragile_op(n) && !is_Bad(n)) {
		n = skip_Id(n);
		n = skip_Tuple(n);
		n = skip_Proj(n);
	}
	return n;
}

static void irg_block_walk_2(ir_node *node, irg_walk_func *pre,
                             irg_walk_func *post, void *env)
{
	int i;

	if (Block_block_visited(node))
		return;
	mark_Block_block_visited(node);

	if (pre)
		pre(node, env);

	for (i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
		/* find the corresponding predecessor block. */
		ir_node *pred = get_cf_op(get_Block_cfgpred(node, i));
		pred = get_nodes_block(pred);
		if (get_irn_opcode(pred) == iro_Block) {
			/* recursion */
			irg_block_walk_2(pred, pre, post, env);
		} else {
			assert(get_irn_opcode(pred) == iro_Bad);
		}
	}

	if (post)
		post(node, env);
}


/* walks only over Block nodes in the graph.  Has its own visited
   flag, so that it can be interleaved with the other walker.         */
void irg_block_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                    void *env)
{
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *block = is_Block(node) ? node : get_nodes_block(node);

	hook_irg_block_walk(irg, node, (generic_func *)pre, (generic_func *)post);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	irg_block_walk_2(block, pre, post, env);

	/* Some blocks might be only reachable through keep-alive edges */
	if (is_End(node)) {
		int arity = get_irn_arity(node);
		int i;
		for (i = 0; i < arity; i++) {
			ir_node *pred = get_irn_n(node, i);
			if (!is_Block(pred))
				continue;
			irg_block_walk_2(pred, pre, post, env);
		}
	}

	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
}

/*
 * walk over a graph block wise
 */
void irg_block_walk_graph(ir_graph *irg, irg_walk_func *pre,
                          irg_walk_func *post, void *env)
{
	ir_graph * rem = current_ir_graph;
	current_ir_graph = irg;
	irg_block_walk(get_irg_end(irg), pre, post, env);
	current_ir_graph = rem;
}

/*
 * Additionally walk over all anchors. Do NOT increase the visit flag.
 */
void irg_walk_anchors(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	ir_graph * rem = current_ir_graph;
	current_ir_graph = irg;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	irg_walk_2(irg->anchor, pre, post, env);
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

	current_ir_graph = rem;
}

/********************************************************************/

typedef struct walk_env {
	irg_walk_func *pre;
	irg_walk_func *post;
	void *env;
} walk_env;

static void walk_initializer(ir_initializer_t *initializer, walk_env *env)
{
	switch (initializer->kind) {
    case IR_INITIALIZER_CONST:
		irg_walk(initializer->consti.value, env->pre, env->post, env->env);
        return;
    case IR_INITIALIZER_TARVAL:
    case IR_INITIALIZER_NULL:
        return;

    case IR_INITIALIZER_COMPOUND: {
        size_t i;
        for (i = 0; i < initializer->compound.n_initializers; ++i) {
            ir_initializer_t *subinitializer
                = initializer->compound.initializers[i];
            walk_initializer(subinitializer, env);
        }
        return;
    }
	}
	panic("invalid initializer found");
}

/**
 * Walk to all constant expressions in this entity.
 */
static void walk_entity(ir_entity *ent, void *env)
{
	walk_env *my_env = (walk_env *)env;

	if (ent->initializer != NULL) {
		walk_initializer(ent->initializer, my_env);
	} else if (entity_has_compound_ent_values(ent)) {
		size_t i, n_vals = get_compound_ent_n_values(ent);

		for (i = 0; i < n_vals; i++)
			irg_walk(get_compound_ent_value(ent, i), my_env->pre, my_env->post, my_env->env);
	}
}

/* Walks over all code in const_code_irg. */
void walk_const_code(irg_walk_func *pre, irg_walk_func *post, void *env)
{
	walk_env my_env;
	ir_segment_t s;
	size_t i;
	size_t n_types;

	ir_graph *rem = current_ir_graph;
	current_ir_graph = get_const_code_irg();
	inc_irg_visited(current_ir_graph);

	my_env.pre = pre;
	my_env.post = post;
	my_env.env = env;

	/* Walk all types that can contain constant entities.  */
	for (s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s)
		walk_types_entities(get_segment_type(s), &walk_entity, &my_env);
	n_types = get_irp_n_types();
	for (i = 0; i < n_types; i++)
		walk_types_entities(get_irp_type(i), &walk_entity, &my_env);
	for (i = 0; i < get_irp_n_irgs(); i++)
		walk_types_entities(get_irg_frame_type(get_irp_irg(i)), &walk_entity, &my_env);

	/* Walk constant array bounds. */
	for (i = 0; i < n_types; i++) {
		ir_type *tp = get_irp_type(i);
		if (is_Array_type(tp)) {
			size_t j, n_dim = get_array_n_dimensions(tp);
			for (j = 0; j < n_dim; j++) {
				ir_node *n = get_array_lower_bound(tp, j);
				if (n) irg_walk(n, pre, post, env);
				n = get_array_upper_bound(tp, j);
				if (n) irg_walk(n, pre, post, env);
			}
		}
	}

	current_ir_graph = rem;
}
