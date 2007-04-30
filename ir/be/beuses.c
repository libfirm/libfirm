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
 * @brief       Methods to compute when a value will be used again.
 * @author      Sebastian Hack, Matthias Braun
 * @date        27.06.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>
#include <stdlib.h>

#include "config.h"
#include "obst.h"
#include "pmap.h"
#include "debug.h"

#include "irgwalk.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "irdom_t.h"

#include "be_t.h"
#include "beutil.h"
#include "belive_t.h"
#include "benode_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch_t.h"
#include "beuses.h"
#include "benodesets.h"

#define SCAN_INTERBLOCK_USES

typedef struct _be_use_t {
	const ir_node *block;
	const ir_node *node;
	int outermost_loop;
	unsigned next_use;
	unsigned visited;
} be_use_t;

struct _be_uses_t {
  	set *uses;
	ir_graph *irg;
	const be_lv_t *lv;
	unsigned visited_counter;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
};

static int cmp_use(const void *a, const void *b, size_t n)
{
	const be_use_t *p = a;
	const be_use_t *q = b;
	return !(p->block == q->block && p->node == q->node);
}

static be_next_use_t get_next_use(be_uses_t *env, ir_node *from,
								  unsigned from_step, const ir_node *def,
								  int skip_from_uses);

static const be_use_t *get_or_set_use_block(be_uses_t *env,
                                            const ir_node *block,
                                            const ir_node *def)
{
	unsigned hash = HASH_COMBINE(nodeset_hash(block), nodeset_hash(def));
	be_use_t temp;
	be_use_t* result;

	temp.block = block;
	temp.node = def;
	result = set_find(env->uses, &temp, sizeof(temp), hash);

	if(result == NULL) {
		// insert templ first as we might end in a loop in the get_next_use
		// call otherwise
		temp.next_use = USES_INFINITY;
		temp.outermost_loop = -1;
		temp.visited = 0;
		result = set_insert(env->uses, &temp, sizeof(temp), hash);
	}

	if(result->outermost_loop < 0 && result->visited < env->visited_counter) {
		be_next_use_t next_use;

		result->visited = env->visited_counter;
		next_use = get_next_use(env, sched_first(block), 0, def, 0);
		if(next_use.outermost_loop >= 0) {
			result->next_use = next_use.time;
			result->outermost_loop = next_use.outermost_loop;
			DBG((env->dbg, LEVEL_5, "Setting nextuse of %+F in block %+F to %u (outermostloop %d)\n", def, block, result->next_use, result->outermost_loop));
		}
	}

	return result;
}

static int be_is_phi_argument(const be_lv_t *lv, const ir_node *block, const ir_node *def)
{
	ir_node *node;
	ir_node *succ_block = NULL;
	const ir_edge_t *edge;
	int arity, i;

#if 0
	if(get_irn_n_edges_kind(block, EDGE_KIND_BLOCK) > 1)
		return 0;
#endif

	foreach_block_succ(block, edge) {
		succ_block = get_edge_src_irn(edge);
		break;
	}

	arity = get_Block_n_cfgpreds(succ_block);
	if(arity <= 1)
		return 0;

	for(i = 0; i < arity; ++i) {
		if(get_Block_cfgpred_block(succ_block, i) == block)
			break;
	}
	assert(i < arity);

	sched_foreach(succ_block, node) {
		ir_node *arg;

		if(!is_Phi(node))
			break;

		arg = get_irn_n(node, i);
		if(arg == def)
			return 1;
	}

	return 0;
}

static be_next_use_t get_next_use(be_uses_t *env, ir_node *from,
								  unsigned from_step, const ir_node *def,
								  int skip_from_uses)
{
	unsigned step = from_step;
	ir_node *block = get_nodes_block(from);
	ir_node *node;
	const ir_edge_t *edge;

	if(skip_from_uses) {
		step++;
		from = sched_next(from);
	}

	sched_foreach_from(from, node) {
		int i, arity;

		if(is_Phi(node)) {
			step++;
			continue;
		}

		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			const ir_node *operand = get_irn_n(node, i);

			if (operand == def) {
				be_next_use_t result;

				DBG((env->dbg, LEVEL_3, "found use of %+F at %+F\n", operand, node));

				/**
				 * Spills/Reloads are a special case, they're not really a
				 * usage of a value, continue searching
				 */
				if (be_is_Spill(node) || be_is_Reload(node)) {
					return be_get_next_use(env, node, step, node, 1);
				}

				result.time = step;
				result.outermost_loop = get_loop_depth(get_irn_loop(block));
				return result;
			}
		}

		step++;
	}

	if(be_is_phi_argument(env->lv, block, def)) {
		// TODO we really should continue searching the uses of the phi,
		// as a phi isn't a real use that implies a reload (because we could
		// easily spill the whole phi)

		be_next_use_t result;
		result.time = step;
		result.outermost_loop = get_loop_depth(get_irn_loop(block));
		return result;
	}

#ifdef SCAN_INTERBLOCK_USES
	{
	unsigned next_use = USES_INFINITY;
	int outermost_loop;
	be_next_use_t result;
	ir_loop *loop = get_irn_loop(block);
	int loopdepth = get_loop_depth(loop);
	int found_visited = 0;
	int found_use = 0;
	ir_graph *irg = get_irn_irg(block);
	ir_node *startblock = get_irg_start_block(irg);

	outermost_loop = loopdepth;
	foreach_block_succ(block, edge) {
		const be_use_t *use;
		const ir_node *succ_block = get_edge_src_irn(edge);
		ir_loop *succ_loop;
		unsigned use_dist;

		if(succ_block == startblock)
			continue;

		DBG((env->dbg, LEVEL_5, "Checking succ of block %+F: %+F (for use of %+F)\n", block, succ_block, def));
		if(!be_is_live_in(env->lv, succ_block, def)) {
			//next_use = USES_INFINITY;
			DBG((env->dbg, LEVEL_5, "   not live in\n"));
			continue;
		}

		use = get_or_set_use_block(env, succ_block, def);
		DBG((env->dbg, LEVEL_5, "Found %u (loopdepth %d) (we're in block %+F)\n", use->next_use,
					use->outermost_loop, block));
		if(USES_IS_INFINITE(use->next_use)) {
			if(use->outermost_loop < 0) {
				found_visited = 1;
			}
			continue;
		}

		found_use = 1;
		use_dist = use->next_use;

		succ_loop = get_irn_loop(succ_block);
		if(get_loop_depth(succ_loop) < loopdepth) {
			unsigned factor = (loopdepth - get_loop_depth(succ_loop)) * 5000;
			DBG((env->dbg, LEVEL_5, "Increase usestep because of loop out edge %d -> %d (%u)\n", factor));
			// TODO we should use the number of nodes in the loop or so...
			use_dist += factor;
		}

		if(use_dist < next_use) {
			next_use = use_dist;
			outermost_loop = use->outermost_loop;
		}
	}

	if(loopdepth < outermost_loop)
		outermost_loop = loopdepth;

	result.time = next_use + step;
	result.outermost_loop = outermost_loop;

	if(!found_use && found_visited) {
		// the current result is correct for the current search, but isn't
		// generally correct, so mark it
		result.outermost_loop = -1;
	}
	DBG((env->dbg, LEVEL_5, "Result: %d (outerloop: %d)\n", result.time, result.outermost_loop));
	return result;
	}
#else
	return USES_INFINITY;
#endif
}

be_next_use_t be_get_next_use(be_uses_t *env, ir_node *from,
                         unsigned from_step, const ir_node *def,
                         int skip_from_uses)
{
	env->visited_counter++;
	return get_next_use(env, from, from_step, def, skip_from_uses);
}

be_uses_t *be_begin_uses(ir_graph *irg, const be_lv_t *lv)
{
	be_uses_t *env = xmalloc(sizeof(env[0]));

	edges_assure(irg);

	env->uses = new_set(cmp_use, 512);
	env->irg = irg;
	env->lv = lv;
	env->visited_counter = 0;
	FIRM_DBG_REGISTER(env->dbg, "firm.be.uses");

	return env;
}

void be_end_uses(be_uses_t *env)
{
	del_set(env->uses);
	free(env);
}
