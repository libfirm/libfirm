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

#include "lc_opts.h"
#include "lc_opts_enum.h"
#include "irtools.h"

#define SCHED_INITIAL_GRANULARITY (1 << 14)

static void sched_renumber(const ir_node *block)
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
	if(before_ts >= after_ts) {
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
		if(ts == before_ts || ts == after_ts)
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



static be_module_list_entry_t *schedulers;
static schedule_func           scheduler;

void be_register_scheduler(const char *name, schedule_func func)
{
	if (scheduler == NULL)
		scheduler = func;
	be_add_module_to_list(&schedulers, name, func);
}

void be_schedule_graph(ir_graph *irg)
{
	scheduler(irg);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched);
void be_init_sched(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	be_add_module_list_opt(be_grp, "scheduler", "scheduling algorithm",
	                       &schedulers, (void**)&scheduler);
}
