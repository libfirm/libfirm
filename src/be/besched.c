/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Scheduling utilities for nodes in Blocks and Blocks.
 * @author      Sebastian Hack
 */
#include "besched.h"

#include "belistsched.h"
#include "belive.h"
#include "bemodule.h"
#include "firm_types.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irtools.h"
#include "lc_opts.h"
#include "lc_opts_enum.h"
#include <stdlib.h>

#define SCHED_INITIAL_GRANULARITY (1 << 14)

static void sched_renumber(ir_node *const block)
{
	sched_timestep_t step = SCHED_INITIAL_GRANULARITY;

	sched_foreach(block, irn) {
		sched_info_t *inf = get_irn_sched_info(irn);
		inf->time_step = step;
		step += SCHED_INITIAL_GRANULARITY;
	}
}

static inline void sched_set_time_stamp(const ir_node *irn)
{
	sched_info_t       *info      = get_irn_sched_info(irn);
	const sched_info_t *prev_info = get_irn_sched_info(info->prev);
	const sched_info_t *next_info = get_irn_sched_info(info->next);
	sched_timestep_t    before_ts = prev_info->time_step;
	sched_timestep_t    after_ts  = next_info->time_step;

	/*
	 * If we are the last, we can give us a big time step,
	 * else we have to compute our time step from our
	 * neighbours.
	 */
	if (before_ts >= after_ts) {
		info->time_step = before_ts + SCHED_INITIAL_GRANULARITY;
		/* overflow? */
		if (info->time_step <= before_ts) {
			sched_renumber(get_nodes_block(irn));
		}
	} else {
		sched_timestep_t ts = (before_ts + after_ts) / 2;

		/*
		 * If the resolution went out, we have to renumber
		 * this block.
		 */
		if (ts == before_ts || ts == after_ts)
			sched_renumber(get_nodes_block(irn));
		else
			info->time_step = ts;
	}
}

void sched_add_before(ir_node *before, ir_node *irn)
{
	sched_info_t *info      = get_irn_sched_info(irn);
	ir_node      *next      = before;
	sched_info_t *next_info = get_irn_sched_info(next);
	ir_node      *prev      = next_info->prev;
	sched_info_t *prev_info = get_irn_sched_info(prev);
	assert(sched_is_scheduled(before));
	assert(!sched_is_scheduled(irn));
	assert(!is_Proj(before));
	assert(!is_Proj(irn));
	assert(get_block_const(before) == get_nodes_block(irn));

	info->prev = prev;
	info->next = next;
	prev_info->next = irn;
	next_info->prev = irn;
	sched_set_time_stamp(irn);
}

void sched_add_after(ir_node *after, ir_node *irn)
{
	sched_info_t *info      = get_irn_sched_info(irn);
	ir_node      *prev      = after;
	sched_info_t *prev_info = get_irn_sched_info(prev);
	ir_node      *next      = prev_info->next;
	sched_info_t *next_info = get_irn_sched_info(next);
	assert(sched_is_scheduled(after));
	assert(!sched_is_scheduled(irn));
	assert(!is_Proj(after));
	assert(!is_Proj(irn));
	assert(get_block_const(after) == get_nodes_block(irn));

	info->prev = prev;
	info->next = next;
	prev_info->next = irn;
	next_info->prev = irn;
	sched_set_time_stamp(irn);
}

void sched_remove(ir_node *irn)
{
	sched_info_t *info      = get_irn_sched_info(irn);
	ir_node      *prev      = info->prev;
	ir_node      *next      = info->next;
	sched_info_t *prev_info = get_irn_sched_info(prev);
	sched_info_t *next_info = get_irn_sched_info(next);
	assert(sched_is_scheduled(irn));

	prev_info->next = next;
	next_info->prev = prev;
	info->next      = NULL;
	info->prev      = NULL;
}

void sched_replace(ir_node *const old, ir_node *const irn)
{
	assert(sched_is_scheduled(old));
	assert(!sched_is_scheduled(irn));

	sched_info_t *const old_info = get_irn_sched_info(old);
	sched_info_t *const irn_info = get_irn_sched_info(irn);
	*irn_info = *old_info;

	old_info->prev = NULL;
	old_info->next = NULL;

	ir_node *const prev = irn_info->prev;
	ir_node *const next = irn_info->next;
	get_irn_sched_info(prev)->next = irn;
	get_irn_sched_info(next)->prev = irn;
}

static be_module_list_entry_t *schedulers;
static schedule_func           scheduler;

void be_register_scheduler(const char *name, schedule_func func)
{
	if (scheduler == NULL)
		scheduler = func;
	be_add_module_to_list(&schedulers, name, (void*)func);
}

void be_schedule_graph(ir_graph *irg)
{
	scheduler(irg);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched)
void be_init_sched(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	be_add_module_list_opt(be_grp, "scheduler", "scheduling algorithm",
	                       &schedulers, (void**)&scheduler);
}

ir_node *be_move_after_schedule_first(ir_node *node)
{
	for (;;) {
		ir_node *const next = sched_next(node);
		if (!arch_irn_is(next, schedule_first))
			return node;
		node = next;
	}
}
