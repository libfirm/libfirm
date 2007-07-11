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
 * @brief       Scheduling utilities for nodes in Blocks and Blocks.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "impl.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "firm_types.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irextbb.h"
#include "debug.h"

#include "bemodule.h"
#include "bearch_t.h"
#include "besched_t.h"
#include "beutil.h"
#include "belistsched.h"

FIRM_IMPL1(sched_get_time_step, int, const ir_node *)
FIRM_IMPL1(sched_has_next, int, const ir_node *)
FIRM_IMPL1(sched_has_prev, int, const ir_node *)
FIRM_IMPL1(sched_next, ir_node *, const ir_node *)
FIRM_IMPL1(sched_prev, ir_node *, const ir_node *)
FIRM_IMPL1(sched_is_scheduled, int, const ir_node *)
FIRM_IMPL1(sched_first, ir_node *, const ir_node *)
FIRM_IMPL1(sched_last, ir_node *, const ir_node *)
FIRM_IMPL2_VOID(sched_add_after, ir_node *, ir_node *)
FIRM_IMPL2_VOID(sched_add_before, ir_node *, ir_node *)
FIRM_IMPL1_VOID(sched_init_block, ir_node *)
FIRM_IMPL1_VOID(sched_reset, ir_node *)
FIRM_IMPL2(sched_comes_after, int, const ir_node *, const ir_node *)
FIRM_IMPL1_VOID(sched_remove, ir_node *)

size_t sched_irn_data_offset = 0;

static void block_sched_dumper(ir_node *block, void *env)
{
	FILE  *f = env;
	const ir_node *curr;

	ir_fprintf(f, "%+F:\n", block);

	sched_foreach(block, curr) {
		sched_info_t *info = get_irn_sched_info(curr);
		ir_fprintf(f, "\t%6d: %+F\n", info->time_step, curr);
	}
}

void be_sched_dump(FILE *f, ir_graph *irg)
{
	irg_block_walk_graph(irg, block_sched_dumper, NULL, f);
}

/* Init the scheduling stuff. */
void be_init_sched(void)
{
	sched_irn_data_offset = firm_register_additional_node_data(sizeof(sched_info_t));
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched);

void sched_renumber(const ir_node *block)
{
	ir_node *irn;
	sched_info_t *inf;
	sched_timestep_t step = SCHED_INITIAL_GRANULARITY;

	sched_foreach(block, irn) {
		inf = get_irn_sched_info(irn);
		inf->time_step = step;
		step += SCHED_INITIAL_GRANULARITY;
	}
}

int sched_skip_cf_predicator(const ir_node *irn, void *data) {
	arch_env_t *ae = data;
	return arch_irn_class_is(ae, irn, branch);
}

int sched_skip_phi_predicator(const ir_node *irn, void *data) {
	(void) data;
	return is_Phi(irn);
}

/* Skip nodes in a schedule. */
ir_node *sched_skip(ir_node *from, int forward, sched_predicator_t *predicator, void *data)
{
	const ir_node *bl = get_block(from);
	ir_node *curr;

	if (forward) {
		if (is_Block(from))
			from = sched_next(from);
		for (curr = from; curr != bl && predicator(curr, data); curr = sched_next(curr)) {
		}
	} else {
		if (is_Block(from))
			from = sched_prev(from);
		for (curr = from; curr != bl && predicator(curr, data); curr = sched_prev(curr)) {
		}
	}

	return curr;
}

//---------------------------------------------------------------------------

typedef struct remove_dead_nodes_env_t_ {
	bitset_t *reachable;
	ir_graph *irg;
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
	ir_node                 *node, *next;

	for (node = sched_first(block); ! sched_is_end(node); node = next) {
		/* get next node now, as after calling sched_remove it will be invalid */
		next = sched_next(node);

		if (bitset_is_set(env->reachable, get_irn_idx(node)))
			continue;

		if(env->lv)
			be_liveness_remove(env->lv, node);
		sched_remove(node);
		be_kill_node(node);
	}
}

void be_remove_dead_nodes_from_schedule(be_irg_t *birg)
{
	ir_graph *irg = be_get_birg_irg(birg);

	remove_dead_nodes_env_t env;
	env.reachable = bitset_alloca(get_irg_last_idx(irg));
	env.lv  = be_get_birg_liveness(birg);
	env.irg = irg;

	// mark all reachable nodes
	irg_walk_graph(irg, mark_dead_nodes_walker, NULL, &env);

	// walk schedule and remove non-marked nodes
	irg_block_walk_graph(irg, remove_dead_nodes_walker, NULL, &env);
}
