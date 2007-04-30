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
 * @brief       Base routines for register allocation.
 * @author      Sebastian Hack
 * @date        22.11.2004
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "pset.h"
#include "impl.h"

#include "irnode.h"
#include "irmode.h"
#include "irdom.h"
#include "iredges.h"

#include "bera.h"
#include "beutil.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bemodule.h"

be_ra_timer_t *global_ra_timer = NULL;

static INLINE
sched_timestep_t get_time_step(const ir_node *irn)
{
	if(is_Phi(irn))
		return 0;

	return sched_get_time_step(irn);
}

int value_dominates_intrablock(const ir_node *a, const ir_node *b)
{
	sched_timestep_t as = get_time_step(a);
	sched_timestep_t bs = get_time_step(b);

	return as <= bs;
}

int value_dominates(const ir_node *a, const ir_node *b)
{
	const ir_node *block_a = get_block(a);
	const ir_node *block_b = get_block(b);

	/*
	 * a and b are not in the same block,
	 * so dominance is determined by the dominance of the blocks.
	 */
	if(block_a != block_b) {
		return block_dominates(block_a, block_b);
	}

	/*
	 * Dominance is determined by the time steps of the schedule.
	 */
	return value_dominates_intrablock(a, b);
}

/**
 * Check, if two values interfere.
 * @param a The first value.
 * @param b The second value.
 * @return 1, if a and b interfere, 0 if not.
 */
int values_interfere(const be_lv_t *lv, const ir_node *a, const ir_node *b)
{
	int a2b = value_dominates(a, b);
	int b2a = value_dominates(b, a);

	/* If there is no dominance relation, they do not interfere. */
	if((a2b | b2a) > 0) {
		const ir_edge_t *edge;
		ir_node *bb;

		/*
		 * Adjust a and b so, that a dominates b if
		 * a dominates b or vice versa.
		 */
		if(b2a) {
			const ir_node *t = a;
			a = b;
			b = t;
		}

		bb = get_nodes_block(b);

		/*
		 * If a is live end in b's block it is
		 * live at b's definition (a dominates b)
		 */
		if(be_is_live_end(lv, bb, a))
			return 1;

		/*
		 * Look at all usages of a.
		 * If there's one usage of a in the block of b, then
		 * we check, if this use is dominated by b, if that's true
		 * a and b interfere. Note that b must strictly dominate the user,
		 * since if b is the last user of in the block, b and a do not
		 * interfere.
		 * Uses of a not in b's block can be disobeyed, because the
		 * check for a being live at the end of b's block is already
		 * performed.
		 */
		foreach_out_edge(a, edge) {
			const ir_node *user = get_edge_src_irn(edge);
			if(get_nodes_block(user) == bb && !is_Phi(user) && b != user && value_dominates(b, user))
				return 1;
		}
  	}

	return 0;
}

/** The list of register allocators */
static be_module_list_entry_t *register_allocators = NULL;
static be_ra_t *selected_allocator = NULL;

void be_register_allocator(const char *name, be_ra_t *allocator)
{
	if(selected_allocator == NULL)
		selected_allocator = allocator;
	be_add_module_to_list(&register_allocators, name, allocator);
}

void be_allocate_registers(be_irg_t *birg)
{
	assert(selected_allocator != NULL);
	if(selected_allocator != NULL) {
		selected_allocator->allocate(birg);
	}
}

void be_init_ra(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");

	be_add_module_list_opt(be_grp, "regalloc", "register allocator",
	                       &register_allocators, (void**) &selected_allocator);
}
BE_REGISTER_MODULE_CONSTRUCTOR(init_be_ra);
