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

#define MAX_PERMI_SIZE 5
#define NUM_REGISTERS  32

DEBUG_ONLY(static firm_dbg_module_t *dbg_icore;)

static unsigned sourceof[NUM_REGISTERS];
static unsigned usecount[NUM_REGISTERS];

typedef enum perm_type_t {
	PERM_CHAIN,
	PERM_CYCLE
} perm_type_t;

typedef struct perm_op_t {
	perm_type_t type;
	unsigned    regs[NUM_REGISTERS];
	unsigned    length;
} perm_op_t;

static perm_op_t ops[NUM_REGISTERS];
static unsigned  num_ops = 0;

static ir_node               *sched_point     = NULL;
static const arch_register_class_t *reg_class = NULL;

static const arch_register_t *get_arch_register(unsigned index)
{
	return arch_register_for_index(reg_class, index);
}

static const char *get_register_name_from_index(unsigned index)
{
	return arch_register_get_name(get_arch_register(index));
}

static void print_perm_op(const perm_op_t *op)
{
	unsigned i;

	printf("%s", get_register_name_from_index(op->regs[0]));
	for (i = 1; i < op->length; ++i)
		printf(" -> %s", get_register_name_from_index(op->regs[i]));

	if (op->type == PERM_CYCLE)
		printf(" -> %s", get_register_name_from_index(op->regs[0]));

	puts("");
}

static void analyze_regs(const ir_node *perm)
{
	const ir_edge_t *edge;
	const ir_edge_t *next;

	foreach_out_edge_safe(perm, edge, next) {
		ir_node               *out     = get_edge_src_irn(edge);
		long                   pn      = get_Proj_proj(out);
		ir_node               *in      = get_irn_n(perm, pn);
		const arch_register_t *in_reg  = arch_get_irn_register(in);
		const arch_register_t *out_reg = arch_get_irn_register(out);
		unsigned               iidx;
		unsigned               oidx;

		if (reg_class == NULL)
			reg_class = arch_register_get_class(in_reg);

		/* Ignore registers that are left untouched by the Perm node. */
		if (in_reg == out_reg) {
			DBG((dbg_icore, LEVEL_1,
				"%+F removing equal perm register pair (%+F, %+F, %s)\n",
				perm, in, out, arch_register_get_name(out_reg)));

			exchange(out, in);
			continue;
		}

		oidx = arch_register_get_index(out_reg);
		iidx = arch_register_get_index(in_reg);

		sourceof[oidx] = iidx; /* Remember the source, Luke. */
		++usecount[iidx];      /* Increment usecount of source register.*/
	}
}

static void reverse_regs(perm_op_t *op)
{
	const unsigned length = op->length;
	unsigned       i;

	for (i = 0; i < length / 2; ++i) {
		const unsigned other = length - i - 1;
		const unsigned tmp   = op->regs[other];

		op->regs[other] = op->regs[i];
		op->regs[i]     = tmp;
	}
}

/* reg is the last register in the chain. */
static void create_chain(unsigned reg)
{
	perm_op_t *op     = &ops[num_ops++];
	unsigned   length = 0;

	/* reg is guaranteed to be part of our chain. */
	op->regs[length++] = reg;

	while (usecount[reg] == 0 && sourceof[reg] != reg) {
		unsigned src = sourceof[reg];

		/* Mark as done. */
		sourceof[reg] = reg;

		/* src is also part of our chain. */
		op->regs[length++] = src;

		assert(usecount[src] > 0);
		--usecount[src];

		reg = src;
	}

	/* Set all op attributes and reverse register ordering. */
	op->type   = PERM_CHAIN;
	op->length = length;
	reverse_regs(op);

	printf("Found a chain: ");
	print_perm_op(op);
}

static void search_chains()
{
	unsigned reg;

	for (reg = 0; reg < NUM_REGISTERS; /* empty */) {
		unsigned src = sourceof[reg];

		/* Skip fix points.
		 * Also, all registers with a usecount > 0 cannot be the end
		 * of a chain. */
		if (src == reg || usecount[reg] > 0) {
			++reg;
			continue;
		}

		/* We know: register reg is the end of a chain. */
		create_chain(reg);
	}
}

static void create_cycle(unsigned reg)
{
	perm_op_t *op     = &ops[num_ops++];
	unsigned   length = 0;

	while (sourceof[reg] != reg) {
		unsigned src = sourceof[reg];

		op->regs[length++] = reg;
		sourceof[reg] = reg;
		reg = src;
	}

	op->type   = PERM_CYCLE;
	op->length = length;
	reverse_regs(op);

	printf("Found a cycle: ");
	print_perm_op(op);
}

/* Precondition: all chains have already been handled. */
static void search_cycles()
{
	unsigned reg;

	for (reg = 0; reg < NUM_REGISTERS; /* empty */) {
		unsigned src = sourceof[reg];

		/* Skip fix points. */
		if (src == reg) {
			++reg;
			continue;
		}

		/* This must hold as only cycles are left. */
		assert(usecount[reg] == 1);
		create_cycle(reg);
	}
}

static void analyze_perm(const ir_node *perm)
{
	unsigned i;

	memset(usecount, 0, NUM_REGISTERS * sizeof(unsigned));
	for (i = 0; i < NUM_REGISTERS; ++i)
		sourceof[i] = i;

	analyze_regs(perm);

	search_chains();
	search_cycles();

#ifdef DEBUG_libfirm
	for (i = 0; i < NUM_REGISTERS; ++i)
		assert(sourceof[i] == i);
#endif
}

static void lower_perm_node(ir_node *perm)
{
	int arity = get_irn_arity(perm);

	/* Check preconditions. */
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
