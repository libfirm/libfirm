/**
 * @file
 * @brief
 * This file contains the following IRG modifications for be routines:
 * - backend dominance information
 * - SSA construction for a set of nodes
 * - insertion of Perm nodes
 * - empty block elimination
 * - a simple dead node elimination (set inputs of unreachable nodes to BAD)
 *
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date        04.05.2005
 * @version     $Id$
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#ifdef HAVE_MALLOC_H
 #include <malloc.h>
#endif
#ifdef HAVE_ALLOCA_H
 #include <alloca.h>
#endif

#include "hashptr.h"
#include "pdeq.h"
#include "pset.h"
#include "pmap.h"
#include "util.h"
#include "debug.h"
#include "error.h"

#include "irflag_t.h"
#include "ircons_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irmode_t.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irgopt.h"
#include "irprintf_t.h"
#include "irgwalk.h"

#include "be_t.h"
#include "bechordal_t.h"
#include "bearch.h"
#include "besched_t.h"
#include "belive_t.h"
#include "benode_t.h"
#include "beutil.h"
#include "beinsn_t.h"

#include "beirgmod.h"

DEBUG_ONLY(static firm_dbg_module_t *dbgssa = NULL;)
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/*
  ____ ____    _
 / ___/ ___|  / \
 \___ \___ \ / _ \
  ___) |__) / ___ \
 |____/____/_/   \_\
   ____                _                   _   _
  / ___|___  _ __  ___| |_ _ __ _   _  ___| |_(_) ___  _ __
 | |   / _ \| '_ \/ __| __| '__| | | |/ __| __| |/ _ \| '_ \
 | |__| (_) | | | \__ \ |_| |  | |_| | (__| |_| | (_) | | | |
  \____\___/|_| |_|___/\__|_|   \__,_|\___|\__|_|\___/|_| |_|

*/

/* The problem: Given a value and a set of "copies" that are known to represent
 * the same abstract value, rewire all usages of the original value to their
 * closest copy while introducing phis as necessary.
 *
 * Algorithm: Mark all blocks in the iterated dominance frontiers of the value
 * and it's copies. Link the copies ordered by dominance to the blocks.
 * The we search for each use all all definitions in the current block, if none
 * is found, then we search one in the immediate dominator. If we are in a block
 * of the dominance frontier, create a phi and search do the same search for the
 * phi arguments.
 */

/**
 * Calculates the iterated dominance frontier of a set of blocks. Marks the
 * blocks as visited. Sets the link fields of the blocks in the dominance
 * frontier to the block itself.
 */
static
void mark_iterated_dominance_frontiers(const be_dom_front_info_t *domfronts,
                                       waitq *worklist)
{
	DBG((dbgssa, LEVEL_3, "Dominance Frontier:"));
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
			DBG((dbgssa, LEVEL_3, " %+F", y));
			mark_Block_block_visited(y);
		}
	}
	DBG((dbgssa, LEVEL_3, "\n"));
}

typedef struct ssa_constr_env_t {
	ir_node **new_phis;    /**< ARR_F of newly created phis or NULL */
	ir_mode *mode;         /**< mode of the value */
} ssa_constr_env_t;

static
ir_node *search_def_end_of_block(ssa_constr_env_t *env, ir_node *block);

static
ir_node *create_phi(ssa_constr_env_t *env, ir_node *block, ir_node *link_with)
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

	DBG((dbgssa, LEVEL_2, "\tcreating phi %+F in %+F\n", phi, block));
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
ir_node *get_def_at_idom(ssa_constr_env_t *env, ir_node *block)
{
	ir_node *dom = get_Block_idom(block);
	ir_node *def = search_def_end_of_block(env, dom);

	return def;
}

static
ir_node *search_def_end_of_block(ssa_constr_env_t *env, ir_node *block)
{
	if(irn_visited(block)) {
		assert(get_irn_link(block) != NULL);
		return get_irn_link(block);
	} else if(Block_block_visited(block)) {
		return create_phi(env, block, block);
	} else {
		ir_node *def = get_def_at_idom(env, block);
#if 1
		mark_irn_visited(block);
		set_irn_link(block, def);
#endif

		return def;
	}
}

static
ir_node *search_def(ssa_constr_env_t *env, ir_node *at)
{
	ir_node *block = get_nodes_block(at);
	ir_node *node;
	ir_node *def;

	DBG((dbgssa, LEVEL_3, "\t...searching def at %+F\n", at));

	/* no defs in the current block we can do the normal searching */
	if(!irn_visited(block) && !Block_block_visited(block)) {
		DBG((dbgssa, LEVEL_3, "\t...continue at idom\n"));
		return get_def_at_idom(env, block);
	}

	/* there are defs in the current block, walk the linked list to find
	   the one immediately dominating us
	 */
	node = block;
	def  = get_irn_link(node);
	while(def != NULL) {
#if 0
		assert(get_nodes_block(def) == block);
		if(!value_dominates_intrablock(at, def)) {
			DBG((dbgssa, LEVEL_3, "\t...found dominating def %+F\n", def));
			return def;
		}
#else
		if(!value_dominates(at, def)) {
			DBG((dbgssa, LEVEL_3, "\t...found dominating def %+F\n", def));
			return def;
		}
#endif
		node = def;
		def  = get_irn_link(node);
	}

	/* block in dominance frontier? create a phi then */
	if(Block_block_visited(block)) {
		DBG((dbgssa, LEVEL_3, "\t...create phi at block %+F\n", block));
		assert(!is_Phi(node));
		return create_phi(env, block, node);
	}


	DBG((dbgssa, LEVEL_3, "\t...continue at idom (after checking block)\n"));
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

ir_node **be_ssa_construction(const be_dom_front_info_t *domfronts, be_lv_t *lv,
                         ir_node *value, int copies_len, ir_node **copies,
                         const ir_nodeset_t *ignore_uses, int need_new_phis)
{
	ir_graph *irg = get_irn_irg(value);
	const ir_edge_t *edge, *next;
	int i;
	ir_node *block;
	waitq *worklist;
	ssa_constr_env_t env;

	/* We need to collect the phi functions to compute their liveness. */
	if(lv != NULL || need_new_phis) {
		env.new_phis = NEW_ARR_F(ir_node*, 0);
	} else {
		env.new_phis = NULL;
	}
	env.mode = get_irn_mode(value);

	set_using_visited(irg);
	set_using_block_visited(irg);
	set_using_irn_link(irg);

	/* we use the visited flag to indicate blocks in the dominance frontier
	 * and blocks that already have the relevant value at the end calculated */
	inc_irg_visited(irg);
	/* We use the block visited flag to indicate blocks in the dominance
	 * froniter of some values (and this potentially needing phis) */
	inc_irg_block_visited(irg);

	DBG((dbgssa, LEVEL_1, "Introducing following copies for: %+F\n", value));
	/* compute iterated dominance frontiers and create lists in the block link
	 * fields that sort usages by dominance. Blocks in the dominance frontier
	 * are marked by links back to the block. */
	worklist = new_waitq();

	block = get_nodes_block(value);
	introduce_def_at_block(block, value);
	waitq_put(worklist, block);

	for(i = 0; i < copies_len; ++i) {
		ir_node *copy = copies[i];
		block = get_nodes_block(copy);

		if(!irn_visited(block)) {
			waitq_put(worklist, block);
		}
		introduce_def_at_block(block, copy);
		DBG((dbgssa, LEVEL_1, "\t%+F in %+F\n", copy, block));
	}

	mark_iterated_dominance_frontiers(domfronts, worklist);
	del_waitq(worklist);

	DBG((dbgssa, LEVEL_2, "New Definitions:\n"));
	/*
	 * Search the valid def for each use and set it.
	 */
	foreach_out_edge_safe(value, edge, next) {
		ir_node *use = get_edge_src_irn(edge);
		ir_node *at  = use;
		ir_node *def;
		int pos      = get_edge_src_pos(edge);

		if(ignore_uses != NULL && ir_nodeset_contains(ignore_uses, use))
			continue;

		if(is_Phi(use)) {
			ir_node *block = get_nodes_block(use);
			ir_node *predblock = get_Block_cfgpred_block(block, pos);
			at = sched_last(predblock);
		}

		def = search_def(&env, at);

		if(def == NULL) {
			panic("no definition found for %+F at position %d\n", use, pos);
		}

		DBG((dbgssa, LEVEL_2, "\t%+F(%d) -> %+F\n", use, pos, def));
		set_irn_n(use, pos, def);
	}

	/* Recompute the liveness of the original nodes, the copies and the
	 * inserted phis. */
	if(lv != NULL) {
		int n;

		be_liveness_update(lv, value);
		for(i = 0; i < copies_len; ++i) {
			ir_node *copy = copies[i];
			be_liveness_update(lv, copy);
		}

		n = ARR_LEN(env.new_phis);
		for(i = 0; i < n; ++i) {
			ir_node *phi = env.new_phis[i];
			be_liveness_introduce(lv, phi);
		}
	}

	clear_using_visited(irg);
	clear_using_block_visited(irg);
	clear_using_irn_link(irg);

	if(!need_new_phis && env.new_phis != NULL) {
		DEL_ARR_F(env.new_phis);
		return NULL;
	}
	return env.new_phis;
}

void be_ssa_constr_set_ignore(const be_dom_front_info_t *domfronts, be_lv_t *lv,
                              pset *nodes, pset *ignores)
{
	ir_graph *irg;
	const ir_edge_t *edge, *next;
	int i;
	ir_node *block;
	waitq *worklist;
	ir_node *value;
	ssa_constr_env_t env;

	foreach_pset(nodes, value) {
		pset_break(nodes);
		break;
	}
	irg = get_irn_irg(value);

	/* We need to collect the phi functions to compute their liveness. */
	if(lv != NULL) {
		env.new_phis = NEW_ARR_F(ir_node*, 0);
	}

	set_using_visited(irg);
	set_using_block_visited(irg);
	set_using_irn_link(irg);

	/* we use the visited flag to indicate blocks in the dominance frontier
	 * and blocks that already have the relevant value at the end calculated */
	inc_irg_visited(irg);
	/* We use the block visited flag to indicate blocks in the dominance
	 * froniter of some values (and this potentially needing phis) */
	inc_irg_block_visited(irg);

	DBG((dbgssa, LEVEL_1, "Introducing following copies for:\n"));

	/* compute iterated dominance frontiers and create lists in the block link
	 * fields that sort usages by dominance. Blocks in the dominance frontier
	 * are marked by links back to the block. */
	worklist = new_waitq();

	foreach_pset(nodes, value) {
		block = get_nodes_block(value);
		env.mode = get_irn_mode(value);

		if(!irn_visited(block)) {
			waitq_put(worklist, block);
		}
		introduce_def_at_block(block, value);
		DBG((dbgssa, LEVEL_1, "\t%+F in %+F\n", value, block));
	}

	mark_iterated_dominance_frontiers(domfronts, worklist);
	del_waitq(worklist);

	/*
	 * Search the valid def for each use and set it.
	 */
	foreach_pset(nodes, value) {
		foreach_out_edge_safe(value, edge, next) {
			ir_node *use = get_edge_src_irn(edge);
			ir_node *at  = use;
			ir_node *def;
			int pos      = get_edge_src_pos(edge);

			if(ignores != NULL && pset_find_ptr(ignores, use))
				continue;

			if(is_Phi(use)) {
				ir_node *block = get_nodes_block(use);
				ir_node *predblock = get_Block_cfgpred_block(block, pos);
				at = sched_last(predblock);
			}

			def = search_def(&env, at);

			if(def == NULL) {
				panic("no definition found for %+F input %d\n", use, pos);
			}

			DBG((dbgssa, LEVEL_2, "\t%+F(%d) -> %+F\n", use, pos, def));
			set_irn_n(use, pos, def);
		}
	}

	/* Recompute the liveness of the original nodes, the copies and the
	 * inserted phis. */
	if(lv != NULL) {
		int n;

		foreach_pset(nodes, value) {
			be_liveness_update(lv, value);
		}

		n = ARR_LEN(env.new_phis);
		for(i = 0; i < n; ++i) {
			ir_node *phi = env.new_phis[i];
			be_liveness_introduce(lv, phi);
		}
		DEL_ARR_F(env.new_phis);
	}

	clear_using_visited(irg);
	clear_using_block_visited(irg);
	clear_using_irn_link(irg);
}

/*
  ___                     _     ____
 |_ _|_ __  ___  ___ _ __| |_  |  _ \ ___ _ __ _ __ ___
  | || '_ \/ __|/ _ \ '__| __| | |_) / _ \ '__| '_ ` _ \
  | || | | \__ \  __/ |  | |_  |  __/  __/ |  | | | | | |
 |___|_| |_|___/\___|_|   \__| |_|   \___|_|  |_| |_| |_|

*/

ir_node *insert_Perm_after(const arch_env_t *arch_env,
						   be_lv_t *lv,
						   const arch_register_class_t *cls,
						   be_dom_front_info_t *dom_front,
						   ir_node *pos)
{
	ir_node *bl     = is_Block(pos) ? pos : get_nodes_block(pos);
	ir_graph *irg   = get_irn_irg(bl);
	pset *live      = pset_new_ptr_default();

	ir_node *curr, *irn, *perm, **nodes;
	int i, n;

	DBG((dbg, LEVEL_1, "Insert Perm after: %+F\n", pos));

	be_liveness_nodes_live_at(lv, arch_env, cls, pos, live);

	n = pset_count(live);

	if(n == 0) {
		del_pset(live);
		return NULL;
	}

	nodes = xmalloc(n * sizeof(nodes[0]));

	DBG((dbg, LEVEL_1, "live:\n"));
	for(irn = pset_first(live), i = 0; irn; irn = pset_next(live), i++) {
		DBG((dbg, LEVEL_1, "\t%+F\n", irn));
		nodes[i] = irn;
	}
	del_pset(live);

	perm = be_new_Perm(cls, irg, bl, n, nodes);
	sched_add_after(pos, perm);
	free(nodes);

	curr = perm;
	for (i = 0; i < n; ++i) {
		ir_node *copies[1];
		ir_node *perm_op = get_irn_n(perm, i);
		const arch_register_t *reg = arch_get_irn_register(arch_env, perm_op);

		ir_mode *mode = get_irn_mode(perm_op);
		ir_node *proj = new_r_Proj(irg, bl, perm, mode, i);
		arch_set_irn_register(arch_env, proj, reg);

		sched_add_after(curr, proj);
		curr = proj;

		copies[0] = proj;

		be_ssa_construction(dom_front, lv, perm_op, 1, copies, NULL, 0);
	}

	return perm;
}

/**
 * Post-block-walker: Find blocks containing only one jump and
 * remove them.
 */
static void remove_empty_block(ir_node *block, void *data) {
	const ir_edge_t *edge, *next;
	ir_node *node;
	int *changed = data;
	ir_node *jump = NULL;

	assert(is_Block(block));

	if (get_Block_n_cfgpreds(block) != 1)
		return;

	sched_foreach(block, node) {
		if (! is_Jmp(node))
			return;
		if (jump != NULL) {
			/* we should never have 2 jumps in a block */
			panic("We should never have 2 jumps in a block");
		}
		jump = node;
	}

	if (jump == NULL)
		return;

	node = get_Block_cfgpred(block, 0);
	foreach_out_edge_safe(jump, edge, next) {
		ir_node *block = get_edge_src_irn(edge);
		int     pos    = get_edge_src_pos(edge);

		set_irn_n(block, pos, node);
	}

	set_Block_cfgpred(block, 0, new_Bad());
	be_kill_node(jump);
	*changed = 1;
}

/* removes basic blocks that just contain a jump instruction */
int be_remove_empty_blocks(ir_graph *irg) {
	int changed = 0;

	irg_block_walk_graph(irg, remove_empty_block, NULL, &changed);
	if (changed) {
		/* invalidate analysis info */
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_outs_inconsistent(irg);
	}
	return changed;
}

void be_init_irgmod(void)
{
	FIRM_DBG_REGISTER(dbgssa, "firm.be.ssaconstr");
	FIRM_DBG_REGISTER(dbg, "firm.be.irgmod");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_irgmod);
