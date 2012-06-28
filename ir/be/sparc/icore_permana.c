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
 * @brief    Analyze Perm node to identify cycles and chains
 * @author   Manuel Mohr
 */

#include "config.h"

#include <stdlib.h>

#include "icore_permana.h"

#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/* Provided by user calling analyze_perm. */
static const ir_node *perm    = NULL;
static perm_op_t     *ops     = NULL;
static unsigned       num_ops = 0;

/* Internal use. */
static unsigned sourceof[NUM_REGISTERS];
static unsigned usecount[NUM_REGISTERS];
static const arch_register_class_t *reg_class;


static const arch_register_t *get_arch_register_from_index(unsigned index)
{
	return arch_register_for_index(reg_class, index);
}

static const char *get_register_name_from_index(unsigned index)
{
	return arch_register_get_name(get_arch_register_from_index(index));
}

void print_perm_op(const perm_op_t *op)
{
	unsigned i;

	DB((dbg, LEVEL_2, "%s(%u)", get_register_name_from_index(op->regs[0]),
		op->regs[0]));
	for (i = 1; i < op->length; ++i)
		DB((dbg, LEVEL_2, " -> %s(%u)",
			get_register_name_from_index(op->regs[i]), op->regs[i]));

	if (op->type == PERM_OP_CYCLE)
		DB((dbg, LEVEL_2, " -> %s(%u)",
			get_register_name_from_index(op->regs[0]), op->regs[0]));

	DB((dbg, LEVEL_2, "\n"));
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
	op->type   = PERM_OP_CHAIN;
	op->length = length;
	reverse_regs(op);

	DB((dbg, LEVEL_2, "  Found a chain: "));
	print_perm_op(op);
}

static void search_chains(void)
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

	op->type   = PERM_OP_CYCLE;
	op->length = length;
	reverse_regs(op);

	DB((dbg, LEVEL_2, "  Found a cycle: "));
	print_perm_op(op);
}

/* Precondition: all chains have already been handled. */
static void search_cycles(void)
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

static void analyze_regs(void)
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

		assert(in_reg != out_reg
		       && "Self loops should have been removed earlier");

		oidx = arch_register_get_index(out_reg);
		iidx = arch_register_get_index(in_reg);

		sourceof[oidx] = iidx;
		++usecount[iidx]; /* Increment usecount of source register.*/
	}
}

static void init_state(const ir_node *perm_, perm_op_t *ops_)
{
	unsigned i;

	perm    = perm_;
	ops     = ops_;
	num_ops = 0;

	memset(usecount,  0, NUM_REGISTERS * sizeof(usecount[0]));
	for (i = 0; i < NUM_REGISTERS; ++i)
		sourceof[i] = i;

	if (reg_class == NULL) {
		ir_node               *in  = get_irn_n(perm, 0);
		const arch_register_t *reg = arch_get_irn_register(in);

		reg_class = arch_register_get_class(reg);
	}
}

unsigned analyze_perm(const ir_node *perm_, perm_op_t *ops_)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.lower.icore.permana");
	init_state(perm_, ops_);

	analyze_regs();
	search_chains();
	search_cycles();

#ifdef DEBUG_libfirm
	{
		unsigned i;
		for (i = 0; i < NUM_REGISTERS; ++i)
			assert(sourceof[i] == i);
	}
#endif

	return num_ops;
}
