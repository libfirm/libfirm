/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Contains some useful function for the backend.
 * @author      Sebastian Hack
 */
#include "beutil.h"

#include "beirg.h"
#include "belive.h"
#include "benode.h"
#include "besched.h"
#include "bessaconstr.h"
#include "ircons.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnodeset.h"
#include "iropt.h"
#include "irtools.h"
#include "util.h"
#include <stdio.h>

/**
 * Block-walker: adds the visited block to a flexible array.
 */
static void add_to_postorder(ir_node *block, void *data)
{
	ir_node ***list = (ir_node***) data;
	ARR_APP1(ir_node*, *list, block);
}

ir_node **be_get_cfgpostorder(ir_graph *irg)
{
	ir_node **list      = NEW_ARR_F(ir_node*, 0);
	ir_node  *end_block = get_irg_end_block(irg);

	/* end block may be unreachable in case of endless loops */
	if (get_Block_n_cfgpreds(end_block) == 0)
		ARR_APP1(ir_node*, list, end_block);

	/* walk blocks */
	irg_block_edges_walk(get_irg_start_block(irg), NULL, add_to_postorder,
	                     &list);

	return list;
}

static int cmp_node_nr(const void *a, const void *b)
{
	ir_node **p1 = (ir_node**)a;
	ir_node **p2 = (ir_node**)b;
	long      n1 = get_irn_node_nr(*p1);
	long      n2 = get_irn_node_nr(*p2);
	return (n1>n2) - (n1<n2);
}

ir_node *insert_Perm_before(ir_graph *irg, const arch_register_class_t *cls,
                            ir_node *const pos)
{
	ir_nodeset_t live;
	ir_nodeset_init(&live);
	be_lv_t *lv = be_get_irg_liveness(irg);
	be_liveness_nodes_live_before(lv, cls, pos, &live);

	size_t n = ir_nodeset_size(&live);
	if (n == 0) {
		ir_nodeset_destroy(&live);
		return NULL;
	}

	ir_node **nodes = XMALLOCN(ir_node*, n);
	size_t    p     = 0;
	foreach_ir_nodeset(&live, irn, iter) {
		nodes[p++] = irn;
	}
	ir_nodeset_destroy(&live);
	/* make the input order deterministic */
	QSORT(nodes, n, cmp_node_nr);

	ir_node *const block = get_nodes_block(pos);
	ir_node *const perm  = be_new_Perm(block, n, nodes);
	sched_add_before(pos, perm);
	free(nodes);

	for (size_t i = 0; i < n; ++i) {
		ir_node *const perm_op = get_irn_n(perm, i);
		ir_node *const proj    = be_new_Proj(perm, i);

		be_ssa_construction_env_t senv;
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

		if (env->lv->sets_valid)
			be_liveness_remove(env->lv, node);
		sched_remove(node);

		/* kill projs */
		if (get_irn_mode(node) == mode_T) {
			foreach_out_edge_safe(node, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!is_Proj(proj))
					continue;
				if (env->lv->sets_valid)
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

	/* mark all reachable nodes */
	irg_walk_graph(irg, mark_dead_nodes_walker, NULL, &env);

	/* walk schedule and remove non-marked nodes */
	irg_block_walk_graph(irg, remove_dead_nodes_walker, NULL, &env);
}

void be_keep_if_unused(ir_node *node)
{
	if (get_irn_n_edges(node) == 0) {
		ir_node *const keep = be_new_Keep_one(node);
		sched_add_after(node, keep);
	}
}
