/**
 * @file
 * @brief
 * This file contains the following IRG modifications for be routines:
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

#include "hashptr.h"
#include "pdeq.h"
#include "pset.h"
#include "pmap.h"
#include "util.h"
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
#include "irprintf_t.h"
#include "irgwalk.h"

#include "be_t.h"
#include "bechordal_t.h"
#include "bearch_t.h"
#include "besched_t.h"
#include "belive_t.h"
#include "benode_t.h"
#include "beutil.h"
#include "beinsn_t.h"
#include "bessaconstr.h"

#include "beirgmod.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/*
  ___                     _     ____
 |_ _|_ __  ___  ___ _ __| |_  |  _ \ ___ _ __ _ __ ___
  | || '_ \/ __|/ _ \ '__| __| | |_) / _ \ '__| '_ ` _ \
  | || | | \__ \  __/ |  | |_  |  __/  __/ |  | | | | | |
 |___|_| |_|___/\___|_|   \__| |_|   \___|_|  |_| |_| |_|

*/

ir_node *insert_Perm_after(be_irg_t *birg,
						   const arch_register_class_t *cls,
						   ir_node *pos)
{
	const arch_env_t *arch_env = birg->main_env->arch_env;
	be_lv_t *lv     = birg->lv;
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
		ir_node *perm_op = get_irn_n(perm, i);
		const arch_register_t *reg = arch_get_irn_register(arch_env, perm_op);
		be_ssa_construction_env_t senv;

		ir_mode *mode = get_irn_mode(perm_op);
		ir_node *proj = new_r_Proj(irg, bl, perm, mode, i);
		arch_set_irn_register(arch_env, proj, reg);

		sched_add_after(curr, proj);
		curr = proj;

		be_ssa_construction_init(&senv, birg);
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
	FIRM_DBG_REGISTER(dbg, "firm.be.irgmod");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_irgmod);
