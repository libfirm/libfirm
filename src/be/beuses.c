/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Methods to compute when a value will be used again.
 * @author      Sebastian Hack, Matthias Braun
 * @date        27.06.2005
 */
#include "beuses.h"

#include "be_t.h"
#include "belive.h"
#include "benode.h"
#include "besched.h"
#include "debug.h"
#include "ircons_t.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "obst.h"
#include "pmap.h"
#include "util.h"
#include <limits.h>
#include <stdlib.h>

#define UNKNOWN_OUTERMOST_LOOP  ((unsigned)-1)

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct be_use_t {
	const ir_node *block;
	const ir_node *node;
	unsigned       outermost_loop;
	unsigned       next_use;
	ir_visited_t   visited;
} be_use_t;

/**
 * The "uses" environment.
 */
struct be_uses_t {
	set           *uses; /**< cache: contains all computed uses so far. */
	const be_lv_t *lv;   /**< the liveness for the graph. */
	ir_visited_t   visited_counter; /**< current search counter. */
};

/**
 * Set-compare two uses.
 */
static int cmp_use(const void *a, const void *b, size_t n)
{
	(void)n;
	const be_use_t *p = (const be_use_t*)a;
	const be_use_t *q = (const be_use_t*)b;
	return p->block != q->block || p->node != q->node;
}

static be_next_use_t get_next_use(be_uses_t *env, ir_node *from,
                                  const ir_node *def, bool skip_from_uses);

/**
 * Return the use for the given definition in the given block if exists,
 * else create it.
 *
 * @param env    the uses environment
 * @param block  the block we search the use in
 * @param def    the definition of the value we are searching
 */
static const be_use_t *get_or_set_use_block(be_uses_t *env,
                                            const ir_node *block,
                                            const ir_node *def)
{
	be_use_t temp;
	temp.block = block;
	temp.node  = def;

	unsigned  hash   = hash_combine(hash_irn(block), hash_irn(def));
	be_use_t *result = set_find(be_use_t, env->uses, &temp, sizeof(temp), hash);

	if (result == NULL) {
		// insert templ first as we might end in a loop in the get_next_use
		// call otherwise
		temp.next_use       = USES_INFINITY;
		temp.outermost_loop = UNKNOWN_OUTERMOST_LOOP;
		temp.visited        = 0;
		result = set_insert(be_use_t, env->uses, &temp, sizeof(temp), hash);
	}

	if (result->outermost_loop == UNKNOWN_OUTERMOST_LOOP
	 && result->visited < env->visited_counter) {
		result->visited = env->visited_counter;
		be_next_use_t next_use = get_next_use(env, sched_first(block), def, 0);
		if (next_use.outermost_loop != UNKNOWN_OUTERMOST_LOOP) {
			result->next_use       = next_use.time;
			result->outermost_loop = next_use.outermost_loop;
			DBG((dbg, LEVEL_5, "Setting nextuse of %+F in block %+F to %u (outermostloop %d)\n",
				def, block, result->next_use, result->outermost_loop));
		}
	}

	return result;
}

/**
 * Check if a value of the given definition is used in the given block
 * as a Phi argument.
 *
 * @param block  the block to check
 * @param def    the definition of the value
 *
 * @return non-zero if the value is used in the given block as a Phi argument
 * in one of its successor blocks.
 */
static bool be_is_phi_argument(const ir_node *block, const ir_node *def)
{
	if (get_irn_n_edges_kind(block, EDGE_KIND_BLOCK) < 1)
		return false;

	const ir_edge_t *edge = get_irn_out_edge_first_kind(block, EDGE_KIND_BLOCK);
	ir_node *const succ_block = get_edge_src_irn(edge);
	if (get_Block_n_cfgpreds(succ_block) <= 1) {
		/* no Phis in the successor */
		return false;
	}

	/* iterate over the Phi nodes in the successor and check if def is
	 * one of its arguments */
	const int i = get_edge_src_pos(edge);
	sched_foreach_phi(succ_block, node) {
		const ir_node *arg = get_irn_n(node, i);
		if (arg == def)
			return true;
	}

	return false;
}

/**
 * Retrieve the scheduled index (the "step") of this node in its block.
 */
static inline unsigned get_step(const ir_node *node)
{
	assert(!is_Block(node));
	return (unsigned)PTR_TO_INT(get_irn_link(node));
}

/**
 * Set the scheduled index (the "step") of this node in its block.
 */
static inline void set_step(ir_node *node, unsigned step)
{
	assert(!is_Block(node));
	set_irn_link(node, INT_TO_PTR(step));
}

/**
 * Find the next use of a value defined by def, starting at node from.
 *
 * @param env             the uses environment
 * @param from            the node at which we should start the search
 * @param def             the definition of the value
 * @param skip_from_uses  if non-zero, ignore from uses
 */
static be_next_use_t get_next_use(be_uses_t *const env, ir_node *const from,
                                  const ir_node *def, bool skip_from_uses)
{
	ir_node *next_use_node = NULL;
	unsigned next_use_step = INT_MAX;
	unsigned timestep      = get_step(from);
	ir_node *block         = get_nodes_block(from);
	foreach_out_edge(def, edge) {
		ir_node *node = get_edge_src_irn(edge);
		if (is_Anchor(node))
			continue;
		if (get_nodes_block(node) != block)
			continue;
		if (is_Phi(node))
			continue;

		unsigned node_step = get_step(node);
		if (node_step < timestep + skip_from_uses)
			continue;
		if (node_step < next_use_step) {
			next_use_node = node;
			next_use_step = node_step;
		}
	}

	if (next_use_node != NULL) {
		be_next_use_t result;
		result.time           = next_use_step - timestep;
		result.outermost_loop = get_loop_depth(get_irn_loop(block));
		result.before         = next_use_node;
		return result;
	}

	ir_node *node = sched_last(block);
	unsigned step = get_step(node) + 1 - timestep;

	if (be_is_phi_argument(block, def)) {
		// TODO we really should continue searching the uses of the phi,
		// as a phi isn't a real use that implies a reload (because we could
		// easily spill the whole phi)

		be_next_use_t result;
		result.time           = step;
		result.outermost_loop = get_loop_depth(get_irn_loop(block));
		result.before         = block;
		return result;
	}

	be_next_use_t result;
	result.before = NULL;
	ir_loop  *loop           = get_irn_loop(block);
	unsigned  loopdepth      = get_loop_depth(loop);
	bool      found_visited  = false;
	bool      found_use      = false;
	unsigned  outermost_loop = loopdepth;
	unsigned  next_use       = USES_INFINITY;
	foreach_block_succ(block, edge) {
		const ir_node *succ_block = get_edge_src_irn(edge);
		DBG((dbg, LEVEL_5, "Checking succ of block %+F: %+F (for use of %+F)\n",
		     block, succ_block, def));
		if (!be_is_live_in(env->lv, succ_block, def)) {
			//next_use = USES_INFINITY;
			DBG((dbg, LEVEL_5, "   not live in\n"));
			continue;
		}

		const be_use_t *use = get_or_set_use_block(env, succ_block, def);
		DBG((dbg, LEVEL_5, "Found %u (loopdepth %u) (we're in block %+F)\n",
		     use->next_use, use->outermost_loop, block));
		if (USES_IS_INFINITE(use->next_use)) {
			if (use->outermost_loop == UNKNOWN_OUTERMOST_LOOP) {
				found_visited = true;
			}
			continue;
		}

		found_use = true;
		unsigned use_dist = use->next_use;

		ir_loop *succ_loop = get_irn_loop(succ_block);
		if (get_loop_depth(succ_loop) < loopdepth) {
			unsigned factor = (loopdepth - get_loop_depth(succ_loop)) * 5000;
			DBG((dbg, LEVEL_5,
			     "Increase usestep because of loop out edge %d -> %d (%u)\n",
			     factor));
			// TODO we should use the number of nodes in the loop or so...
			use_dist += factor;
		}

		if (use_dist < next_use) {
			next_use       = use_dist;
			outermost_loop = use->outermost_loop;
			result.before  = use->node;
		}
	}

	if (loopdepth < outermost_loop)
		outermost_loop = loopdepth;

	result.time           = next_use + step;
	result.outermost_loop = outermost_loop;

	if (!found_use && found_visited) {
		// the current result is correct for the current search, but isn't
		// generally correct, so mark it
		result.outermost_loop = UNKNOWN_OUTERMOST_LOOP;
	}
	DBG((dbg, LEVEL_5, "Result: %d (outerloop: %u)\n", result.time,
	     result.outermost_loop));
	return result;
}

be_next_use_t be_get_next_use(be_uses_t *env, ir_node *from,
                              const ir_node *def, bool skip_from_uses)
{
	++env->visited_counter;
	return get_next_use(env, from, def, skip_from_uses);
}

/**
 * Pre-block walker, set the step number for every scheduled node
 * in increasing order.
 *
 * After this, two scheduled nodes can be easily compared for the
 * "scheduled earlier in block" property.
 */
static void set_sched_step_walker(ir_node *block, void *data)
{
	(void)data;
	unsigned step = 0;

	sched_foreach(block, node) {
		set_step(node, step);
		if (is_Phi(node))
			continue;
		++step;
	}
}

be_uses_t *be_begin_uses(ir_graph *irg, const be_lv_t *lv)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.uses");

	assure_edges(irg);

	/* precalculate sched steps */
	irg_block_walk_graph(irg, set_sched_step_walker, NULL, NULL);

	be_uses_t *env = XMALLOCZ(be_uses_t);
	env->uses = new_set(cmp_use, 512);
	env->lv   = lv;

	return env;
}

void be_end_uses(be_uses_t *env)
{
	del_set(env->uses);
	free(env);
}
