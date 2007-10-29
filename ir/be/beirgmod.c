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
 * @brief       Backend IRG modification routines.
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date        04.05.2005
 * @version     $Id$
 *
 * This file contains the following IRG modifications for be routines:
 * - insertion of Perm nodes
 * - empty block elimination
 * - a simple dead node elimination (set inputs of unreachable nodes to BAD)
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
#include "beirg_t.h"
#include "beirgmod.h"
#include "bemodule.h"

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
	ir_nodeset_t          live;
	ir_nodeset_iterator_t iter;

	ir_node *curr, *irn, *perm, **nodes;
	size_t i, n;

	DBG((dbg, LEVEL_1, "Insert Perm after: %+F\n", pos));

	ir_nodeset_init(&live);
	be_liveness_nodes_live_at(lv, arch_env, cls, pos, &live);

	n = ir_nodeset_size(&live);
	if(n == 0) {
		ir_nodeset_destroy(&live);
		return NULL;
	}

	nodes = xmalloc(n * sizeof(nodes[0]));

	DBG((dbg, LEVEL_1, "live:\n"));
	i = 0;
	foreach_ir_nodeset(&live, irn, iter) {
		DBG((dbg, LEVEL_1, "\t%+F\n", irn));
		nodes[i] = irn;
		i++;
	}
	ir_nodeset_destroy(&live);

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

static int blocks_removed;

/**
 * Post-block-walker: Find blocks containing only one jump and
 * remove them.
 */
static void remove_empty_block(ir_node *block)
{
	const ir_edge_t *edge, *next;
	int      i, arity;
	ir_node *node;
	ir_node *pred;
	ir_node *succ_block;
	ir_node *jump = NULL;

	if (irn_visited(block))
		return;

	mark_irn_visited(block);
	if (get_Block_n_cfgpreds(block) != 1)
		goto check_preds;

	sched_foreach(block, node) {
		if (! is_Jmp(node))
			goto check_preds;
		if (jump != NULL) {
			/* we should never have 2 jumps in a block */
			panic("found 2 jumps in a block");
		}
		jump = node;
	}

	if (jump == NULL)
		goto check_preds;

	pred       = get_Block_cfgpred(block, 0);
	succ_block = NULL;
	foreach_out_edge_safe(jump, edge, next) {
		int pos = get_edge_src_pos(edge);

		assert(succ_block == NULL);
		succ_block = get_edge_src_irn(edge);

		set_irn_n(succ_block, pos, pred);
	}

	/* there can be some non-scheduled Pin nodes left in the block, move them
	 * to the succ block */
	foreach_out_edge_safe(block, edge, next) {
		node = get_edge_src_irn(edge);

		if(node == jump)
			continue;
		if(is_Pin(node)) {
			set_nodes_block(node, succ_block);
			continue;
		}
		panic("Unexpected node %+F in block %+F with empty schedule", node,
		      block);
	}

	set_Block_cfgpred(block, 0, new_Bad());
	be_kill_node(jump);
	blocks_removed = 1;

	/* check predecessor */
	remove_empty_block(get_nodes_block(pred));
	return;

check_preds:
	arity = get_Block_n_cfgpreds(block);
	for(i = 0; i < arity; ++i) {
		ir_node *pred = get_Block_cfgpred_block(block, i);
		remove_empty_block(pred);
	}
}

/* removes basic blocks that just contain a jump instruction */
int be_remove_empty_blocks(ir_graph *irg)
{
	ir_node *end;
	int      i, arity;

	blocks_removed = 0;

	set_using_visited(irg);
	inc_irg_visited(irg);
	remove_empty_block(get_irg_end_block(irg));
	end   = get_irg_end(irg);
	arity = get_irn_arity(end);
	for(i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(end, i);
		if(!is_Block(pred))
			continue;
		remove_empty_block(pred);
	}
	clear_using_visited(irg);

	if (blocks_removed) {
		/* invalidate analysis info */
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_outs_inconsistent(irg);
	}
	return blocks_removed;
}

void be_init_irgmod(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.irgmod");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_irgmod);
