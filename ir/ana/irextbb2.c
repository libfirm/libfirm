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
 * @brief     Alternative extended basic block computation
 * @author    Matthias Braun
 * @date      5.2005
 * @version   $Id$
 * @summary
 *  Alternative algorithm for computing extended basic blocks (using out edges
 *  and execution frequencies)
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "irextbb_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "irouts.h"
#include "xmalloc.h"
#include "irprintf.h"
#include "execfreq.h"

typedef struct _env {
  	struct obstack *obst;   /**< the obstack where allocations took place */
	ir_extblk *head;        /**< head of the list of all extended blocks */
	ir_exec_freq *execfreqs;
} env_t;

/**
 * allocate a new extended block header.
 */
static ir_extblk *allocate_extblk(ir_node *block, env_t *env)
{
	ir_extblk *extblk = obstack_alloc(env->obst, sizeof(*extblk));

	extblk->kind    = k_ir_extblk;
	extblk->visited = 1;
	extblk->blks    = (ir_node **)env->head;
	extblk->link    = block;
	env->head       = extblk;

	set_Block_extbb(block, extblk);
	set_irn_link(block, NULL);

	return extblk;
}

/**
 * add a block to an extended block
 */
static void addto_extblk(ir_extblk *extblk, ir_node *block)
{
	/* link all blocks belonging to this extended block */
	set_irn_link(block, extblk->link);

	extblk->link = block;
	extblk->visited++;

	set_Block_extbb(block, extblk);
}

/**
 * Returns the number of block successors.
 * we are interested only in 1, 2 and >2.
 */
static int get_block_n_succs(ir_node *block) {
	if (edges_activated(current_ir_graph)) {
		const ir_edge_t *edge;

		edge = get_block_succ_first(block);
		if (! edge)
			return 0;

		edge = get_block_succ_next(block, edge);
		if (! edge)
			return 1;

		edge = get_block_succ_next(block, edge);
		return edge ? 3 : 2;
	}

	return get_Block_n_cfg_outs(block);
}

static void pick_successor(ir_node *block, ir_extblk *extblk, env_t *env);

static void create_extblk(ir_node *block, env_t *env)
{
	ir_extblk *extblk;

	if (irn_visited(block))
		return;

	extblk = allocate_extblk(block, env);
	mark_irn_visited(block);

	pick_successor(block, extblk, env);
}

static void pick_successor(ir_node *block, ir_extblk *extblk, env_t *env)
{
	const ir_edge_t *edge;
	ir_node         *best_succ    = NULL;
	double          best_execfreq = -1;

 	/*
		More than two successors means we have a jump table.
		we cannot include a jump target into the current extended
		basic block, so create a new one here.
	*/
	if (get_block_n_succs(block) > 2) {
		const ir_edge_t *edge;

		foreach_block_succ(block, edge) {
			ir_node *succ = get_edge_src_irn(edge);
			create_extblk(succ, env);
		}

		return;
	}

	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		double execfreq;

		if(irn_visited(succ))
			continue;

		if(get_Block_n_cfgpreds(succ) > 1) {
			create_extblk(succ, env);
			continue;
		}

		execfreq = get_block_execfreq(env->execfreqs, succ);

		/*
			Remember best successor and make non best successor with only 1
			pred block to new extbb leaders.
		*/
		if (execfreq > best_execfreq) {
			if (best_succ != NULL) {
				create_extblk(best_succ, env);
			}

			best_execfreq = execfreq;
			best_succ = succ;
		}
		else {
			create_extblk(succ, env);
		}
	}

	/* add best successor and recursively try to pick more */
	if(best_succ != NULL) {
		addto_extblk(extblk, best_succ);
		mark_irn_visited(best_succ);
		pick_successor(best_succ, extblk, env);
	}
}

/*
 * Compute the extended basic blocks for a graph
 */
void compute_extbb_execfreqs(ir_graph *irg, ir_exec_freq *execfreqs) {
  	env_t     env;
	ir_extblk *extbb, *next;
	ir_node   *endblock;

	if (irg->extbb_obst) {
		obstack_free(irg->extbb_obst, NULL);
	}
	else {
		irg->extbb_obst = xmalloc(sizeof(*irg->extbb_obst));
	}
	obstack_init(irg->extbb_obst);

	env.obst      = irg->extbb_obst;
	env.head      = NULL;
	env.execfreqs = execfreqs;

	assure_irg_outs(irg);

	/* we must mark nodes, so increase the visited flag */
	inc_irg_visited(irg);
	create_extblk(get_irg_start_block(irg), &env);

	/* the end block needs a extbb assigned (even for endless loops) */
	endblock = get_irg_end_block(irg);
	if (! irn_visited(endblock)) {
		create_extblk(endblock, &env);
	}

	/*
		Ok, we have now the list of all extended blocks starting with env.head
		every extended block "knowns" the number of blocks in visited and
		the blocks are linked in link.
		Now we can create arrays that hold the blocks, some kind of "out" edges
		for the extended block
	*/
	for (extbb = env.head; extbb; extbb = next) {
		int i, len = (int)extbb->visited;
		ir_node *block;

		next = (ir_extblk *)extbb->blks;

		extbb->blks = NEW_ARR_D(ir_node *, env.obst, len);

		for (block = extbb->link, i = 0; i < len; ++i) {
			ir_node *nblock = get_irn_link(block);

			/* ensure that the leader is the first one */
			extbb->blks[len - 1 - i] = block;
			set_irn_link(block, NULL);
			block = nblock;
		}

#if 0
		for(i = 0; i < len; ++i) {
			if(i > 0)
				printf(", ");
			ir_printf("%+F", extbb->blks[i]);
		}
		printf("\n");
#endif

		extbb->link    = NULL;
		extbb->visited = 0;
	}

	irg->extblk_state = extblk_valid;
}
