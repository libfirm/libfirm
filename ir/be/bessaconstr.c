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
 * @brief       SSA construction for a set of nodes
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date        04.05.2005
 * @version     $Id$
 *
 * The problem: Given a value and a set of "copies" that are known to
 * represent the same abstract value, rewire all usages of the original value
 * to their closest copy while introducing phis as necessary.
 *
 * Algorithm: Mark all blocks in the iterated dominance frontiers of the value
 * and it's copies. Link the copies ordered by dominance to the blocks.  The
 * we search for each use all all definitions in the current block, if none is
 * found, then we search one in the immediate dominator. If we are in a block
 * of the dominance frontier, create a phi and search do the same search for
 * the phi arguments.
 *
 * A copy in this context means, that you want to introduce several new
 * abstract values (in Firm: nodes) for which you know, that they
 * represent the same concrete value. This is the case if you
 * - copy
 * - spill and reload
 * - re-materialize
 * a value.
 *
 * This function reroutes all uses of the original value to the copies in the
 * corresponding dominance subtrees and creates Phi functions where necessary.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bessaconstr.h"
#include "bemodule.h"
#include "besched_t.h"
#include "beintlive_t.h"
#include "beirg_t.h"

#include "debug.h"
#include "error.h"
#include "pdeq.h"
#include "array.h"
#include "irdom.h"

#include "ircons.h"
#include "iredges_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Calculates the iterated dominance frontier of a set of blocks. Marks the
 * blocks as visited. Sets the link fields of the blocks in the dominance
 * frontier to the block itself.
 */
static
void mark_iterated_dominance_frontiers(const be_dom_front_info_t *domfronts,
                                       waitq *worklist)
{
	DBG((dbg, LEVEL_3, "Dominance Frontier:"));
	while(!pdeq_empty(worklist)) {
		int i;
		ir_node *block = waitq_get(worklist);
		ir_node **domfront = be_get_dominance_frontier(domfronts, block);
		int domfront_len = ARR_LEN(domfront);

		for (i = 0; i < domfront_len; ++i) {
			ir_node *y = domfront[i];
			if(Block_block_visited(y))
				continue;

			if(!irn_visited(y)) {
				set_irn_link(y, NULL);
				waitq_put(worklist, y);
			}
			DBG((dbg, LEVEL_3, " %+F", y));
			mark_Block_block_visited(y);
		}
	}
	DBG((dbg, LEVEL_3, "\n"));
}

static
ir_node *search_def_end_of_block(be_ssa_construction_env_t *env,
                                 ir_node *block);

static
ir_node *create_phi(be_ssa_construction_env_t *env, ir_node *block,
                    ir_node *link_with)
{
	int i, n_preds = get_Block_n_cfgpreds(block);
	ir_graph *irg = get_irn_irg(block);
	ir_node *phi;
	ir_node **ins = alloca(n_preds * sizeof(ins[0]));

	assert(n_preds > 1);

	for(i = 0; i < n_preds; ++i) {
		ins[i] = new_r_Unknown(irg, env->mode);
	}
	phi = new_r_Phi(irg, block, n_preds, ins, env->mode);
	if(env->new_phis != NULL) {
		ARR_APP1(ir_node*, env->new_phis, phi);
	}

	if(env->mode != mode_M) {
		sched_add_after(block, phi);
	}

	DBG((dbg, LEVEL_2, "\tcreating phi %+F in %+F\n", phi, block));
	set_irn_link(link_with, phi);
	mark_irn_visited(block);

	for(i = 0; i < n_preds; ++i) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		ir_node *pred_def   = search_def_end_of_block(env, pred_block);

		set_irn_n(phi, i, pred_def);
	}

	return phi;
}

static
ir_node *get_def_at_idom(be_ssa_construction_env_t *env, ir_node *block)
{
	ir_node *dom = get_Block_idom(block);
	assert(dom != NULL);
	return search_def_end_of_block(env, dom);
}

static
ir_node *search_def_end_of_block(be_ssa_construction_env_t *env, ir_node *block)
{
	if(irn_visited(block)) {
		assert(get_irn_link(block) != NULL);
		return get_irn_link(block);
	} else if(Block_block_visited(block)) {
		return create_phi(env, block, block);
	} else {
		ir_node *def = get_def_at_idom(env, block);
		mark_irn_visited(block);
		set_irn_link(block, def);

		return def;
	}
}

static
ir_node *search_def(be_ssa_construction_env_t *env, ir_node *at)
{
	ir_node *block = get_nodes_block(at);
	ir_node *node;
	ir_node *def;

	DBG((dbg, LEVEL_3, "\t...searching def at %+F\n", at));

	/* no defs in the current block we can do the normal searching */
	if(!irn_visited(block) && !Block_block_visited(block)) {
		DBG((dbg, LEVEL_3, "\t...continue at idom\n"));
		return get_def_at_idom(env, block);
	}

	/* there are defs in the current block, walk the linked list to find
	   the one immediately dominating us
	 */
	node = block;
	def  = get_irn_link(node);
	while(def != NULL) {
		if(!value_dominates(at, def)) {
			DBG((dbg, LEVEL_3, "\t...found dominating def %+F\n", def));
			return def;
		}

		node = def;
		def  = get_irn_link(node);
	}

	/* block in dominance frontier? create a phi then */
	if(Block_block_visited(block)) {
		DBG((dbg, LEVEL_3, "\t...create phi at block %+F\n", block));
		assert(!is_Phi(node));
		return create_phi(env, block, node);
	}

	DBG((dbg, LEVEL_3, "\t...continue at idom (after checking block)\n"));
	return get_def_at_idom(env, block);
}

/**
 * Adds a definition into the link field of the block. The definitions are
 * sorted by dominance. A non-visited block means no definition has been
 * inserted yet.
 */
static
void introduce_def_at_block(ir_node *block, ir_node *def)
{
	if(irn_visited(block)) {
		ir_node *node = block;
		ir_node *current_def;

		while(1) {
			current_def = get_irn_link(node);
			if(current_def == def) {
				/* already in block */
				return;
			}
			if(current_def == NULL)
				break;
			if(value_dominates(current_def, def))
				break;
			node = current_def;
		}

		set_irn_link(node, def);
		set_irn_link(def, current_def);
	} else {
		set_irn_link(block, def);
		set_irn_link(def, NULL);
		mark_irn_visited(block);
	}
}

void be_ssa_construction_init(be_ssa_construction_env_t *env, be_irg_t *birg)
{
	ir_graph *irg = be_get_birg_irg(birg);
	memset(env, 0, sizeof(env[0]));

	be_assure_dom_front(birg);

	env->irg       = irg;
	env->domfronts = be_get_birg_dom_front(birg);
	env->new_phis  = NEW_ARR_F(ir_node*, 0);
	env->worklist  = new_waitq();

	set_using_visited(irg);
	set_using_block_visited(irg);
	set_using_irn_link(irg);

	/* we use the visited flag to indicate blocks in the dominance frontier
	 * and blocks that already have the relevant value at the end calculated */
	inc_irg_visited(irg);
	/* We use the block visited flag to indicate blocks in the dominance
	 * froniter of some values (and this potentially needing phis) */
	inc_irg_block_visited(irg);
}

void be_ssa_construction_destroy(be_ssa_construction_env_t *env)
{
	del_waitq(env->worklist);
	DEL_ARR_F(env->new_phis);

	clear_using_visited(env->irg);
	clear_using_block_visited(env->irg);
	clear_using_irn_link(env->irg);
}

void be_ssa_construction_add_copy(be_ssa_construction_env_t *env,
                                  ir_node *copy)
{
	ir_node *block;

	assert(env->iterated_domfront_calculated == 0);

	if(env->mode == NULL) {
		env->mode = get_irn_mode(copy);
	} else {
		assert(env->mode == get_irn_mode(copy));
	}

	block = get_nodes_block(copy);

	if(!irn_visited(block)) {
		waitq_put(env->worklist, block);
	}
	introduce_def_at_block(block, copy);
}

void be_ssa_construction_add_copies(be_ssa_construction_env_t *env,
                                    ir_node **copies, size_t copies_len)
{
	size_t i;

	assert(env->iterated_domfront_calculated == 0);

	if(env->mode == NULL) {
		env->mode = get_irn_mode(copies[0]);
	}

	for(i = 0; i < copies_len; ++i) {
		ir_node *copy = copies[i];
		ir_node *block = get_nodes_block(copy);

		assert(env->mode == get_irn_mode(copy));
		if(!irn_visited(block)) {
			waitq_put(env->worklist, block);
		}
		introduce_def_at_block(block, copy);
	}
}

void be_ssa_construction_set_ignore_uses(be_ssa_construction_env_t *env,
                                         const ir_nodeset_t *ignore_uses)
{
	env->ignore_uses = ignore_uses;
}

ir_node **be_ssa_construction_get_new_phis(be_ssa_construction_env_t *env)
{
	return env->new_phis;
}

void be_ssa_construction_fix_users(be_ssa_construction_env_t *env,
                                   ir_node *value)
{
	const ir_edge_t *edge, *next;

	if(!env->iterated_domfront_calculated) {
		mark_iterated_dominance_frontiers(env->domfronts, env->worklist);
		env->iterated_domfront_calculated = 1;
	}

	/*
	 * Search the valid def for each use and set it.
	 */
	foreach_out_edge_safe(value, edge, next) {
		ir_node *use = get_edge_src_irn(edge);
		ir_node *at  = use;
		int pos      = get_edge_src_pos(edge);
		ir_node *def;

		if(env->ignore_uses != NULL	&&
		   ir_nodeset_contains(env->ignore_uses, use))
			continue;
		if(is_Anchor(use))
			continue;

		if(is_Phi(use)) {
			ir_node *block = get_nodes_block(use);
			ir_node *predblock = get_Block_cfgpred_block(block, pos);
			at = sched_last(predblock);
		}

		def = search_def(env, at);

		if(def == NULL) {
			panic("no definition found for %+F at position %d\n", use, pos);
		}

		DBG((dbg, LEVEL_2, "\t%+F(%d) -> %+F\n", use, pos, def));
		set_irn_n(use, pos, def);
	}
}

void be_ssa_construction_fix_users_array(be_ssa_construction_env_t *env,
                                         ir_node **nodes, size_t nodes_len)
{
	size_t i;

	for(i = 0; i < nodes_len; ++i) {
		ir_node *node = nodes[i];
		be_ssa_construction_fix_users(env, node);
	}
}

void be_ssa_construction_update_liveness_phis(be_ssa_construction_env_t *env,
                                              be_lv_t *lv)
{
	int i, n;

	n = ARR_LEN(env->new_phis);
	for(i = 0; i < n; ++i) {
		ir_node *phi = env->new_phis[i];
		be_liveness_introduce(lv, phi);
	}
}

void be_init_ssaconstr(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ssaconstr");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_ssaconstr);
