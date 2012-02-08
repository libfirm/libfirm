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
 * @brief    Special iCore Perm lowering
 * @author   Manuel Mohr
 */
#include "config.h"

#include <stdlib.h>

#include "icore_lowerperm.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irprofile.h"

#include "bearch.h"
#include "belower.h"
#include "benode.h"
#include "besched.h"
#include "belive.h"
#include "beirg.h"

#include "gen_sparc_new_nodes.h"
#include "gen_sparc_regalloc_if.h"

#include "statev.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg_icore;)

static const int MAX_PERMI_SIZE = 5;
static ir_node *sched_point;

static unsigned                max_size;
static const arch_register_t **regs;
static unsigned               *sourceof;
static unsigned               *usecount;

typedef enum perm_type_t {
	PERM_CHAIN,
	PERM_CYCLE
} perm_type_t;

typedef struct perm_op_t {
	perm_type_t  type;
	unsigned    *regs;
	unsigned     size;
} perm_op_t;

static perm_op_t **ops;


static int get_reg_index(const arch_register_t *reg)
{
	int i;

	for (i = 0; i < max_size; ++i)
		if (regs[i] == reg)
			return i;

	return -1;
}

static void init_regs(const ir_node *perm)
{
	const ir_edge_t *edge;
	const ir_edge_t *next;
	unsigned         n = 0;

	foreach_out_edge_safe(perm, edge, next) {
		const ir_node         *out     = get_edge_src_irn(edge);
		long                   pn      = get_Proj_proj(out);
		const ir_node         *in      = get_irn_n(irn, pn);
		const arch_register_t *in_reg  = arch_get_irn_register(in);
		const arch_register_t *out_reg = arch_get_irn_register(out);
		unsigned               iidx;
		unsigned               oidx;

		/* Ignore registers that are left untouched by the Perm node */
		if (in_reg == out_reg) {
			DBG((dbg_icore, LEVEL_1,
				"%+F removing equal perm register pair (%+F, %+F, %s)\n",
				perm, in, out, out_reg->name));

			exchange(out, in);
			continue;
		}

		if (get_index(out_reg) == -1)
			regs[n++] = out;
		if (get_index(in_reg)  == -1)
			regs[n++] = in;

		oidx = (unsigned) get_index(out_reg);
		iidx = (unsigned) get_index(in_reg);

		sourceof[oidx] = iidx; /* Remember the source, Luke. */
		++usecount[iidx];      /* Increment usecount of this register.*/
	}
}

static void search_chains()
{
	unsigned  oidx;
	bool     *seen = ALLOCANZ(bool, max_size);

	for (oidx = 0; oidx < n; /* empty */) {
		unsigned iidx = sourceof[oidx];

		if (iidx == oidx || usecount[oidx] > 0) {
			++oidx;
			continue;
		}

		/* We know: register with index oidx is the end of a chain. */

	}
}

static void analyze_perm(const ir_node *perm)
{
	const int arity = get_irn_arity(perm);
	unsigned i;

	max_size  = 2 * arity;
	regs      = ALLOCANZ(const arch_register_t *, max_size);
	sourceof  = ALLOCANZ(unsigned, max_size);
	n_users   = ALLOCANZ(unsigned, max_size);

	for (i = 0; i < size; ++i)
		sourceof[i] = i;

	init_regs(perm);

	search_chains();
	search_cycles();
}

static void lower_perm_node(ir_node *perm)
{
	int arity = get_irn_arity(perm);

	assert(be_is_Perm(perm) && "Non-Perm node passed to lower_perm_node");
	assert(arity == get_irn_n_edges(perm)
	       && "Perm's in and out numbers differ");

	/* Get the schedule predecessor node to the perm. */
	sched_point = sched_prev(perm);
	assert(sched_point && "Perm is not scheduled or has no predecessor");

	analyze_perm(perm);

	/* Remove the perm from schedule */
	sched_remove(perm);
	kill_node(perm);
}

/**
 * Calls the corresponding lowering function for the node.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void lower_nodes_after_ra_walker(ir_node *irn, void *walk_env)
{
	int perm_stayed;
	(void) walk_env;

	if (!be_is_Perm(irn))
		return;

	perm_stayed = push_through_perm(irn);
	if (perm_stayed)
		lower_perm_node(irn);
}

void icore_lower_nodes_after_ra(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg_icore, "firm.be.lower.icore");

	/* we will need interference */
	be_assure_live_chk(irg);

	dump_ir_graph(irg, "before_icore_lowering");
	irg_walk_graph(irg, NULL, lower_nodes_after_ra_walker, NULL);
	dump_ir_graph(irg, "after_icore_lowering");
}
