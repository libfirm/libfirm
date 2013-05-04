/*
 * Copyright (C) 1995-2012 University of Karlsruhe.  All right reserved.
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
 * @brief   This file implements functions to finalize the irg for emit.
 */
#include "amd64_finish.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "debug.h"
#include "error.h"
#include "amd64_nodes_attr.h"
#include "gen_amd64_new_nodes.h"
#include "irgwalk.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Returns the index of the first "same" register.
 */
static int get_first_same(arch_register_req_t const *const req)
{
	unsigned const other = req->other_same;
	int            i;

	for (i = 0; i != 32; ++i) {
		if (other & (1U << i))
			return i;
	}
	panic("same position not found");
}

/**
 * Insert copies for all amd64 nodes where the should_be_same requirement is
 * not fulfilled.
 */
static void assure_should_be_same_requirements(ir_node *const node)
{
	int const n_res = arch_get_irn_n_outs(node);
	int       i;

	/* Check all OUT requirements, if there is a should_be_same. */
	for (i = 0; i != n_res; i++) {
		arch_register_req_t const *const req = arch_get_irn_register_req_out(node, i);
		if (arch_register_req_is(req, should_be_same)) {
			int                    const same_pos = get_first_same(req);
			ir_node               *const in_node  = get_irn_n(node, same_pos);
			arch_register_t const *const in_reg   = arch_get_irn_register(in_node);
			arch_register_t const *const out_reg  = arch_get_irn_register_out(node, i);
			if (in_reg != out_reg) {
				ir_node *const block = get_nodes_block(node);
				ir_node *const copy  = be_new_Copy(block, in_node);

				/* Destination is the out register. */
				arch_set_irn_register(copy, out_reg);
				/* Insert copy before the node into the schedule. */
				sched_add_before(node, copy);
				/* Set copy as in. */
				set_irn_n(node, same_pos, copy);

				DBG((dbg, LEVEL_1, "created copy %+F for should be same argument at input %d of %+F\n", copy, same_pos, node));
			}
		}
	}
}

/**
 * Block walker: finishes a block.
 */
static void amd64_finish_irg_walker(ir_node *const block, void *const env)
{
	ir_node *irn;
	ir_node *next;
	(void) env;

	/* Insert should_be_same copies. */
	for (irn = sched_first(block); ! sched_is_end(irn); irn = next) {
		next = sched_next(irn);
		if (is_amd64_irn(irn)) {
			assure_should_be_same_requirements(irn);
		}
	}
}

/**
 * Add Copy nodes for not fulfilled should_be_same constraints.
 */
void amd64_finish_irg(ir_graph *const irg)
{
	irg_block_walk_graph(irg, 0, amd64_finish_irg_walker, 0);
}

void amd64_init_finish(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.finish");
}
