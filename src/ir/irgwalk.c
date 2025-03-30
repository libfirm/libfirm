/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Functions for traversing ir graphs
 * @author  Boris Boesler, Goetz Lindenmaier, Michael Beck
 * @brief
 *  traverse an ir graph
 *  - execute the pre function before recursion
 *  - execute the post function after recursion
 */
#include "irgwalk.h"

#include "array.h"
#include "entity_t.h"
#include "ircons.h"
#include "irgraph_t.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "irnodeset.h"
#include "panic.h"
#include "pset_new.h"
#include <stdlib.h>

/**
 * specialized version of irg_walk_2, called if only pre callback exists
 */
static void irg_walk_2_pre(ir_node *node, irg_walk_func *pre, void *env)
{
	ir_graph    *irg     = get_irn_irg(node);
	ir_visited_t visited = irg->visited;

	set_irn_visited(node, visited);

	pre(node, env);

	if (!is_Block(node)) {
		ir_node *pred = get_nodes_block(node);
		if (pred->visited < visited)
			irg_walk_2_pre(pred, pre, env);
	}
	foreach_irn_in_r(node, i, pred) {
		if (pred->visited < visited)
			irg_walk_2_pre(pred, pre, env);
	}
}

/**
 * specialized version of irg_walk_2, called if only post callback exists
 */
static void irg_walk_2_post(ir_node *node, irg_walk_func *post, void *env)
{
	ir_graph    *irg     = get_irn_irg(node);
	ir_visited_t visited = irg->visited;

	set_irn_visited(node, visited);

	if (!is_Block(node)) {
		ir_node *pred = get_nodes_block(node);
		if (pred->visited < visited)
			irg_walk_2_post(pred, post, env);
	}
	foreach_irn_in_r(node, i, pred) {
		if (pred->visited < visited)
			irg_walk_2_post(pred, post, env);
	}

	post(node, env);
}

/**
 * specialized version of irg_walk_2, called if pre and post callbacks exist
 */
static void irg_walk_2_both(ir_node *node, irg_walk_func *pre,
                                irg_walk_func *post, void *env)
{
	ir_graph    *irg     = get_irn_irg(node);
	ir_visited_t visited = irg->visited;

	set_irn_visited(node, visited);

	pre(node, env);

	if (!is_Block(node)) {
		ir_node *pred = get_nodes_block(node);
		if (pred->visited < visited)
			irg_walk_2_both(pred, pre, post, env);
	}
	foreach_irn_in_r(node, i, pred) {
		if (pred->visited < visited)
			irg_walk_2_both(pred, pre, post, env);
	}

	post(node, env);
}

void irg_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                void *env)
{
	if (irn_visited(node))
		return;

	if      (post == NULL) irg_walk_2_pre (node, pre, env);
	else if (pre  == NULL) irg_walk_2_post(node, post, env);
	else                   irg_walk_2_both(node, pre, post, env);
}

void irg_walk_core(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                   void *env)
{
	irg_walk_2(node, pre, post, env);
}

void irg_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
              void *env)
{
	ir_graph *irg = get_irn_irg(node);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	irg_walk_core(node, pre, post, env);
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}

void irg_walk_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post,
                    void *env)
{
	irg_walk(get_irg_end(irg), pre, post, env);
}

void all_irg_walk(irg_walk_func *pre, irg_walk_func *post, void *env)
{
	foreach_irp_irg(i, irg) {
		irg_walk_graph(irg, pre, post, env);
	}
}

/**
 * specialized version of irg_walk_in_or_dep_2, called if only pre callback exists
 */
static void irg_walk_in_or_dep_2_pre(ir_node *node, irg_walk_func *pre,
                                     void *env)
{
	ir_graph    *irg     = get_irn_irg(node);
	ir_visited_t visited = irg->visited;

	set_irn_visited(node, visited);

	pre(node, env);

	if (!is_Block(node)) {
		ir_node *pred = get_nodes_block(node);
		if (pred->visited < visited)
			irg_walk_in_or_dep_2_pre(pred, pre, env);
	}
	foreach_irn_in_r(node, i, pred) {
		if (pred->visited < visited)
			irg_walk_in_or_dep_2_pre(pred, pre, env);
	}
}

/**
 * specialized version of irg_walk_in_or_dep_2, called if only post callback exists
 */
static void irg_walk_in_or_dep_2_post(ir_node *node, irg_walk_func *post,
                                      void *env)
{
	ir_graph    *irg     = get_irn_irg(node);
	ir_visited_t visited = irg->visited;

	set_irn_visited(node, visited);

	if (!is_Block(node)) {
		ir_node *pred = get_nodes_block(node);
		if (pred->visited < visited)
			irg_walk_in_or_dep_2_post(pred, post, env);
	}
	foreach_irn_in_r(node, i, pred) {
		if (pred->visited < visited)
			irg_walk_in_or_dep_2_post(pred, post, env);
	}

	post(node, env);
}

/**
 * specialized version of irg_walk_in_or_dep_2, called if pre and post callbacks exist
 */
static void irg_walk_in_or_dep_2_both(ir_node *node, irg_walk_func *pre,
                                      irg_walk_func *post, void *env)
{
	ir_graph    *irg     = get_irn_irg(node);
	ir_visited_t visited = irg->visited;

	set_irn_visited(node, visited);

	pre(node, env);

	if (!is_Block(node)) {
		ir_node *pred = get_nodes_block(node);
		if (pred->visited < visited)
			irg_walk_in_or_dep_2_both(pred, pre, post, env);
	}
	foreach_irn_in_r(node, i, pred) {
		if (pred->visited < visited)
			irg_walk_in_or_dep_2_both(pred, pre, post, env);
	}

	post(node, env);
}

/**
 * Intraprozedural graph walker. Follows dependency edges as well.
 */
static void irg_walk_in_or_dep_2(ir_node *node, irg_walk_func *pre,
                                 irg_walk_func *post, void *env)
{
	if (irn_visited(node))
		return;

	if      (post == NULL) irg_walk_in_or_dep_2_pre (node, pre, env);
	else if (pre == NULL)  irg_walk_in_or_dep_2_post(node, post, env);
	else                   irg_walk_in_or_dep_2_both(node, pre, post, env);
}

void irg_walk_in_or_dep(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                        void *env)
{
	assert(node->kind == k_ir_node);

	ir_graph *const irg = get_irn_irg(node);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	irg_walk_in_or_dep_2(node, pre, post, env);
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}

void irg_walk_in_or_dep_graph(ir_graph *irg, irg_walk_func *pre,
                              irg_walk_func *post, void *env)
{
	irg_walk_in_or_dep(get_irg_end(irg), pre, post, env);
}

static void walk_topo_helper(ir_node *irn, ir_nodeset_t *walker_called, irg_walk_func *walker, void *env)
{
	if (irn_visited(irn)) {
		if (!ir_nodeset_contains(walker_called, irn)) {
			/* We have already visited this node, but not
			 * yet called the walker with it. Now, we are
			 * seeing it a second time, therefore we have
			 * gone around a loop and are now seeing the
			 * loop breaker. We must call the walker now
			 * or the node one recursion above us will be
			 * called before one of its arguments. */
			walker(irn, env);
			ir_nodeset_insert(walker_called, irn);
		}
		return;
	}

	/* Break loops at phi/block nodes. Mark them visited, so
	 * recursion will stop, but don't call the walker yet. */
	const bool is_loop_breaker = is_Phi(irn) || is_Block(irn);
	if (is_loop_breaker)
		mark_irn_visited(irn);

	if (!is_Block(irn)) {
		ir_node *const block = get_nodes_block(irn);
		walk_topo_helper(block, walker_called, walker, env);
	}

	for (int i = 0; i < get_irn_arity(irn); ++i) {
		ir_node *const pred = get_irn_n(irn, i);
		walk_topo_helper(pred, walker_called, walker, env);
	}

	if (!ir_nodeset_contains(walker_called, irn)) {
		walker(irn, env);
		ir_nodeset_insert(walker_called, irn);
	}

	mark_irn_visited(irn);
}

void irg_walk_topological(ir_graph *irg, irg_walk_func *walker, void *env)
{
	inc_irg_visited(irg);
	ir_nodeset_t walker_called;
	ir_nodeset_init(&walker_called);
	walk_topo_helper(get_irg_end(irg), &walker_called, walker, env);
}

/** Walks back from n until it finds a real cf op. */
static ir_node *get_cf_op(ir_node *n)
{
	while (!is_cfop(n) && !is_fragile_op(n) && !is_Bad(n)) {
		n = skip_Tuple(n);
		n = skip_Proj(n);
	}
	return n;
}

static void irg_block_walk_2(ir_node *node, irg_walk_func *pre,
                             irg_walk_func *post, void *env)
{
	if (Block_block_visited(node))
		return;
	mark_Block_block_visited(node);

	if (pre != NULL)
		pre(node, env);

	for (int i = get_Block_n_cfgpreds(node); i-- > 0; ) {
		/* find the corresponding predecessor block. */
		ir_node *pred_cfop  = get_cf_op(get_Block_cfgpred(node, i));
		if (is_Bad(pred_cfop))
			continue;
		ir_node *pred_block = get_nodes_block(pred_cfop);
		/* recursion */
		irg_block_walk_2(pred_block, pre, post, env);
	}

	if (post != NULL)
		post(node, env);
}

void irg_block_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                    void *env)
{
	ir_graph *const irg   = get_irn_irg(node);
	ir_node  *const block = get_block(node);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	irg_block_walk_2(block, pre, post, env);

	/* Some blocks might be only reachable through keep-alive edges */
	if (is_End(node)) {
		foreach_irn_in(node, i, pred) {
			if (!is_Block(pred))
				continue;
			irg_block_walk_2(pred, pre, post, env);
		}
	}

	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
}

void irg_block_walk_graph(ir_graph *irg, irg_walk_func *pre,
                          irg_walk_func *post, void *env)
{
	irg_block_walk(get_irg_end(irg), pre, post, env);
}

void irg_walk_anchors(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post,
                      void *env)
{
	irg_walk(irg->anchor, pre, post, env);
}

typedef struct walk_env {
	irg_walk_func *pre;
	irg_walk_func *post;
	void          *env;
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

    case IR_INITIALIZER_COMPOUND:
        for (size_t i = 0; i < initializer->compound.n_initializers; ++i) {
            ir_initializer_t *subinitializer
                = initializer->compound.initializers[i];
            walk_initializer(subinitializer, env);
        }
        return;
	}
	panic("invalid initializer found");
}

/**
 * Walk to all constant expressions in this entity.
 */
static void walk_entity(ir_entity *ent, void *env)
{
	walk_env *my_env = (walk_env*)env;

	if (get_entity_kind(ent) == IR_ENTITY_NORMAL) {
		ir_initializer_t *const init = get_entity_initializer(ent);
		if (init)
			walk_initializer(init, my_env);
	}
}

void walk_const_code(irg_walk_func *pre, irg_walk_func *post, void *env)
{
	ir_graph *const irg = get_const_code_irg();
	inc_irg_visited(irg);

	walk_env my_env;
	my_env.pre = pre;
	my_env.post = post;
	my_env.env = env;

	/* Walk all types that can contain constant entities.  */
	for (ir_segment_t s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s)
		walk_types_entities(get_segment_type(s), &walk_entity, &my_env);
	size_t n_types = get_irp_n_types();
	for (size_t i = 0; i < n_types; i++)
		walk_types_entities(get_irp_type(i), &walk_entity, &my_env);
	foreach_irp_irg(i, irg) {
		walk_types_entities(get_irg_frame_type(irg), &walk_entity, &my_env);
	}
}
