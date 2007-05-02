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
 * @brief   Mips implementation of list scheduler selector
 * @author  Matthias Braun, Mehdi
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "mips_scheduler.h"

#include "../besched_t.h"
#include "../be.h"
#include "../beabi.h"
#include "iredges.h"
#include "ircons.h"
#include "gen_mips_regalloc_if.h"

#include "mips_new_nodes.h"

list_sched_selector_t mips_sched_selector;

typedef struct {
	const arch_env_t* arch_env;
	pset *div_set;
	/**
	 * This array holds an entry for each register that specifies how much cycles
	 * have to pass before we can access that register again
	 * (because mips will write the register value back in the WB phase of the pipeline)
	 */
	int busy_registers[N_mips_gp_REGS];
	/// current block
	ir_node* block;
	ir_node* last_nop;
} mips_sched_env_t;

/* Matze: deprecated and totally broken */
#if 0

static void *mips_scheduler_init_graph(const list_sched_selector_t *vtab, const arch_env_t *arch_env, ir_graph *irg)
{
	mips_sched_env_t *sched_env = xmalloc(sizeof(sched_env[0]));
	memset(sched_env, 0, sizeof(sched_env[0]));

	sched_env->arch_env = arch_env;
	sched_env->div_set = new_pset(pset_default_ptr_cmp, 4);

	return sched_env;
}

static void mips_scheduler_finish_graph(void* graph_env)
{
	mips_sched_env_t *sched_env = (mips_sched_env_t*) graph_env;
	del_pset(sched_env->div_set);
}

static void *mips_scheduler_init_block(void *graph_env, ir_node *block)
{
	mips_sched_env_t *sched_env = (mips_sched_env_t*) graph_env;
	assert(pset_count(sched_env->div_set) == 0);
	srand(12234);
	// TODO later we might have blocks that don't end in a jump
	memset(&sched_env->busy_registers, 0, sizeof(sched_env->busy_registers));
	sched_env->block = block;
	sched_env->last_nop = NULL;
	return sched_env;
}

static void mips_scheduler_finish_block(void* graph_env)
{
	mips_sched_env_t *sched_env = (mips_sched_env_t*) graph_env;
	// attach last nop to end node (so that firm doesn't discard it)
	if(sched_env->last_nop != NULL) {
		ir_node* end = get_irg_end(get_irn_irg(sched_env->block));
		(void) end;
		// TODO
	}
	sched_env->block = NULL;
}

static int mips_scheduler_to_appear_in_schedule(void *block_env, const ir_node *irn)
{
	return is_mips_irn(irn) && !is_mips_zero(irn) && !is_mips_reinterpret_conv(irn) && !is_mips_fallthrough(irn);
}

static void mips_collect_mflohis(pset* set, ir_node* node) {
	// construct a list of nodes that need to be scheduled before
	// we are allowed to schedule another div or mul instruction
	const ir_edge_t *edge, *edge2;

	if(is_mips_div(node)) {
		foreach_out_edge(node, edge) {
			const ir_node* node2 = get_edge_src_irn(edge);

			assert(is_Proj(node2));
			foreach_out_edge(node2, edge2) {
				const ir_node* node3 = get_edge_src_irn(edge2);
				if(is_mips_mfhi(node3) || is_mips_mflo(node3))
					pset_insert_ptr(set, node3);
			}
		}
	} else if(is_mips_mult(node)) {
		foreach_out_edge(node, edge) {
			const ir_node* node2 = get_edge_src_irn(edge);

			if(is_mips_mfhi(node2) || is_mips_mflo(node2))
				pset_insert_ptr(set, node2);
		}
	}
}

static int mips_scheduler_node_allowed(mips_sched_env_t *sched_env, ir_node* node)
{
	if(pset_count(sched_env->div_set) != 0 && (is_mips_div(node) || is_mips_mult(node))) {
		return 0;
	}

	return 1;
}

static ir_node *mips_scheduler_select(void *block_env, nodeset *ready_set, nodeset *live_set)
{
	mips_sched_env_t *sched_env = (mips_sched_env_t*) block_env;
	const arch_env_t *arch_env = (const arch_env_t*) sched_env->arch_env;
	ir_node *node = NULL;
	ir_node *block = sched_env->block;
	ir_node *condjmp = NULL;
	ir_graph *irg = get_irn_irg(block);
	int have_non_branch_nodes = 0;

	// test all nodes in the ready set and take the first non-branch that
	// is allowed
	for (node = nodeset_first(ready_set); node != NULL; node = nodeset_next(ready_set)) {
		if (arch_irn_class_is(arch_env, node, branch)) {
			if (is_irn_forking(node))
				condjmp = node;
			continue;
		}

		have_non_branch_nodes = 1;

		if (mips_scheduler_node_allowed(sched_env, node))
		{
			nodeset_break(ready_set);

			// TODO update busy_registers

			if (is_mips_div(node) || is_mips_mult(node)) {
				mips_collect_mflohis(sched_env->div_set, node);
			} else if(is_mips_mflo(node) || is_mips_mfhi(node)) {
				pset_remove_ptr(sched_env->div_set, node);
			}

			return node;
		}
	}

	// if we arrive here no non-branch node was found that we can emit

	// return a branch if there are just branches left
	if(!have_non_branch_nodes) {
		// schedule conditional branches before non-conditional ones
		if(condjmp != NULL) {
			return condjmp;
		}
		node = nodeset_first(ready_set);
		assert(arch_irn_class_is(arch_env, node, branch));
		nodeset_break(ready_set);
		return node;
	}

	// emit a nop
	node = new_rd_mips_nop(NULL, irg, block, mode_M);
	keep_alive(node);
	return node;
}

#endif

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
const list_sched_selector_t *mips_get_list_sched_selector(const void *self, list_sched_selector_t *selector)
{
#if 0
	memset(&mips_sched_selector, 0, sizeof(mips_sched_selector));
	mips_sched_selector.init_graph = mips_scheduler_init_graph;
	mips_sched_selector.init_block = mips_scheduler_init_block;
	mips_sched_selector.select = mips_scheduler_select;
	mips_sched_selector.to_appear_in_schedule = mips_scheduler_to_appear_in_schedule;
	mips_sched_selector.finish_block = mips_scheduler_finish_block;
	mips_sched_selector.finish_graph = mips_scheduler_finish_graph;
	//return &mips_sched_selector;
#endif
	return selector;
}

const ilp_sched_selector_t *mips_get_ilp_sched_selector(const void *self) {
	return NULL;
}
