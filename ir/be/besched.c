/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
#include "config.h"

#include <stdlib.h>

#include "irprintf.h"
#include "irgwalk.h"
#include "firm_types.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irextbb.h"
#include "irgmod.h"
#include "debug.h"

#include "bemodule.h"
#include "bearch.h"
#include "besched.h"
#include "beutil.h"
#include "belistsched.h"
#include "belive.h"

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

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched);
void be_init_sched(void)
{
	sched_irn_data_offset = firm_register_additional_node_data(sizeof(sched_info_t));
}

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

int sched_skip_cf_predicator(const ir_node *irn, void *data)
{
	(void)data;
	return is_cfop(irn);
}

int sched_skip_phi_predicator(const ir_node *irn, void *data)
{
	(void) data;
	return is_Phi(irn);
}

/* Skip nodes in a schedule. */
ir_node *sched_skip(ir_node *from, int forward, sched_predicator_t *predicator, void *data)
{
	const ir_node *bl = get_block_const(from);
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

		if (env->lv)
			be_liveness_remove(env->lv, node);
		sched_remove(node);
		kill_node(node);
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

int (sched_get_time_step)(const ir_node *node)
{
	return _sched_get_time_step(node);
}

int (sched_has_next)(const ir_node *node)
{
	return _sched_has_next(node);
}

int (sched_has_prev)(const ir_node *node)
{
	return _sched_has_prev(node);
}

ir_node *(sched_next)(const ir_node *node)
{
	return _sched_next(node);
}

ir_node *(sched_prev)(const ir_node *node)
{
	return _sched_prev(node);
}

int (sched_is_scheduled)(const ir_node *node)
{
	return _sched_is_scheduled(node);
}

ir_node *(sched_first)(const ir_node *node)
{
	return _sched_first(node);
}

ir_node *(sched_last)(const ir_node *node)
{
	return _sched_last(node);
}

void (sched_add_after)(ir_node *after, ir_node *node)
{
	_sched_add_after(after, node);
}

void (sched_add_before)(ir_node *before, ir_node *node)
{
	_sched_add_before(before, node);
}

void (sched_init_block)(ir_node *block)
{
	_sched_init_block(block);
}

void (sched_remove)(ir_node *node)
{
	_sched_remove(node);
}

void (sched_reset)(ir_node *node)
{
	_sched_reset(node);
}

int (sched_comes_after)(const ir_node *n1, const ir_node *n2)
{
	return _sched_comes_after(n1, n2);
}
