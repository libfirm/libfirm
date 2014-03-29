/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend IRG modification routines.
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date        04.05.2005
 *
 * This file contains the following IRG modifications for be routines:
 * - insertion of Perm nodes
 * - empty block elimination
 * - a simple dead node elimination (set inputs of unreachable nodes to BAD)
 */
#include <stdlib.h>

#include "../../adt/util.h"
#include "hashptr.h"
#include "debug.h"
#include "error.h"
#include "xmalloc.h"

#include "irflag_t.h"
#include "ircons_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irmode_t.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "be_t.h"
#include "bechordal_t.h"
#include "bearch.h"
#include "besched.h"
#include "belive_t.h"
#include "benode.h"
#include "beutil.h"
#include "beinsn_t.h"
#include "bessaconstr.h"
#include "beirg.h"
#include "beirgmod.h"
#include "bemodule.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static int cmp_node_nr(const void *a, const void *b)
{
	ir_node **p1 = (ir_node**)a;
	ir_node **p2 = (ir_node**)b;
	long      n1 = get_irn_node_nr(*p1);
	long      n2 = get_irn_node_nr(*p2);
	return (n1>n2) - (n1<n2);
}

/*
  ___                     _     ____
 |_ _|_ __  ___  ___ _ __| |_  |  _ \ ___ _ __ _ __ ___
  | || '_ \/ __|/ _ \ '__| __| | |_) / _ \ '__| '_ ` _ \
  | || | | \__ \  __/ |  | |_  |  __/  __/ |  | | | | | |
 |___|_| |_|___/\___|_|   \__| |_|   \___|_|  |_| |_| |_|

*/

ir_node *insert_Perm_before(ir_graph *irg, const arch_register_class_t *cls,
						   ir_node *pos)
{
	be_lv_t     *lv = be_get_irg_liveness(irg);
	ir_nodeset_t live;

	ir_node *perm, **nodes;
	size_t i, n;

	DBG((dbg, LEVEL_1, "Insert Perm before: %+F\n", pos));

	ir_nodeset_init(&live);
	be_liveness_nodes_live_before(lv, cls, pos, &live);

	n = ir_nodeset_size(&live);
	if (n == 0) {
		ir_nodeset_destroy(&live);
		return NULL;
	}

	nodes = XMALLOCN(ir_node*, n);

	DBG((dbg, LEVEL_1, "live:\n"));
	i = 0;
	foreach_ir_nodeset(&live, irn, iter) {
		DBG((dbg, LEVEL_1, "\t%+F\n", irn));
		nodes[i] = irn;
		i++;
	}
	ir_nodeset_destroy(&live);
	/* make the input order deterministic */
	QSORT(nodes, n, cmp_node_nr);

	ir_node *const bl = get_nodes_block(pos);
	perm = be_new_Perm(cls, bl, n, nodes);
	sched_add_before(pos, perm);
	free(nodes);

	for (i = 0; i < n; ++i) {
		ir_node *perm_op = get_irn_n(perm, i);
		be_ssa_construction_env_t senv;

		ir_mode *mode = get_irn_mode(perm_op);
		ir_node *proj = new_r_Proj(perm, mode, i);

		be_ssa_construction_init(&senv, irg);
		be_ssa_construction_add_copy(&senv, perm_op);
		be_ssa_construction_add_copy(&senv, proj);
		be_ssa_construction_fix_users(&senv, perm_op);
		be_ssa_construction_update_liveness_phis(&senv, lv);
		be_liveness_update(lv, perm_op);
		be_liveness_update(lv, proj);
		be_ssa_construction_destroy(&senv);
	}

	return perm;
}

static int blocks_removed;

/**
 * Post-block-walker: Find blocks containing only one jump and
 * remove them.
 */
static void remove_empty_block(ir_node *block)
{
	int        i;
	int        arity;
	ir_node   *pred;
	ir_node   *succ_block;
	ir_node   *jump = NULL;
	ir_graph  *irg = get_irn_irg(block);
	ir_entity *entity;

	if (irn_visited_else_mark(block))
		return;

	if (get_Block_n_cfgpreds(block) != 1)
		goto check_preds;

	sched_foreach(block, node) {
		if (! is_Jmp(node)
				&& !(arch_get_irn_flags(node) & arch_irn_flag_simple_jump))
			goto check_preds;
		if (jump != NULL) {
			/* we should never have 2 jumps in a block */
			panic("found 2 jumps in a block");
		}
		jump = node;
	}

	if (jump == NULL)
		goto check_preds;

	entity     = get_Block_entity(block);
	pred       = get_Block_cfgpred(block, 0);
	succ_block = NULL;
	foreach_out_edge_safe(jump, edge) {
		int pos = get_edge_src_pos(edge);

		assert(succ_block == NULL);
		succ_block = get_edge_src_irn(edge);
		if (get_Block_entity(succ_block) != NULL && entity != NULL) {
			/*
			 * Currently we can add only one label for a block.
			 * Therefore we cannot combine them if  both block already have one.
			 */
			goto check_preds;
		}

		set_irn_n(succ_block, pos, pred);
	}

	/* move the label to the successor block */
	set_Block_entity(succ_block, entity);

	/* there can be some non-scheduled Pin nodes left in the block, move them
	 * to the succ block (Pin) or pred block (Sync) */
	foreach_out_edge_safe(block, edge) {
		ir_node *const node = get_edge_src_irn(edge);

		if (node == jump)
			continue;
		/* we simply kill Pins, because there are some strange interactions
		 * between jump threading, which produce PhiMs with Pins, we simply
		 * kill the pins here, everything is scheduled anyway */
		if (is_Pin(node)) {
			exchange(node, get_Pin_op(node));
			continue;
		}
		if (is_Sync(node)) {
			set_nodes_block(node, get_nodes_block(pred));
			continue;
		}
		if (is_End(node)) { /* End-keep, reroute it to the successor */
			int pos = get_edge_src_pos(edge);
			set_irn_n(node, pos, succ_block);
			continue;
		}
		panic("Unexpected node %+F in block %+F with empty schedule", node, block);
	}

	set_Block_cfgpred(block, 0, new_r_Bad(irg, mode_X));
	kill_node(jump);
	blocks_removed = 1;

	/* check predecessor */
	remove_empty_block(get_nodes_block(pred));
	return;

check_preds:
	arity = get_Block_n_cfgpreds(block);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_Block_cfgpred_block(block, i);
		remove_empty_block(pred);
	}
}

/* removes basic blocks that just contain a jump instruction */
int be_remove_empty_blocks(ir_graph *irg)
{
	blocks_removed = 0;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	remove_empty_block(get_irg_end_block(irg));
	foreach_irn_in(get_irg_end(irg), i, pred) {
		if (!is_Block(pred))
			continue;
		remove_empty_block(pred);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

	if (blocks_removed) {
		/* invalidate analysis info */
		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	}
	return blocks_removed;
}

//---------------------------------------------------------------------------

typedef struct remove_dead_nodes_env_t_ {
	bitset_t *reachable;
	be_lv_t  *lv;
} remove_dead_nodes_env_t;

/**
 * Post-walker: remember all visited nodes in a bitset.
 */
static void mark_dead_nodes_walker(ir_node *node, void *data)
{
	remove_dead_nodes_env_t *env = (remove_dead_nodes_env_t*) data;
	bitset_set(env->reachable, get_irn_idx(node));
}

/**
 * Post-block-walker:
 * Walk through the schedule of every block and remove all dead nodes from it.
 */
static void remove_dead_nodes_walker(ir_node *block, void *data)
{
	remove_dead_nodes_env_t *env = (remove_dead_nodes_env_t*) data;

	sched_foreach_safe(block, node) {
		if (bitset_is_set(env->reachable, get_irn_idx(node)))
			continue;

		if (env->lv != NULL)
			be_liveness_remove(env->lv, node);
		sched_remove(node);

		/* kill projs */
		if (get_irn_mode(node) == mode_T) {
			foreach_out_edge_safe(node, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!is_Proj(proj))
					continue;
				if (env->lv != NULL)
					be_liveness_remove(env->lv, proj);
				kill_node(proj);
			}
		}
		kill_node(node);
	}
}

void be_remove_dead_nodes_from_schedule(ir_graph *irg)
{
	remove_dead_nodes_env_t env;
	env.reachable = bitset_alloca(get_irg_last_idx(irg));
	env.lv        = be_get_irg_liveness(irg);

	// mark all reachable nodes
	irg_walk_graph(irg, mark_dead_nodes_walker, NULL, &env);

	// walk schedule and remove non-marked nodes
	irg_block_walk_graph(irg, remove_dead_nodes_walker, NULL, &env);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_irgmod)
void be_init_irgmod(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.irgmod");
}
