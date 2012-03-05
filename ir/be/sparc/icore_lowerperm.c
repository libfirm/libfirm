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

#define PERMI_SIZE 5
#define NUM_REGISTERS  32

DEBUG_ONLY(static firm_dbg_module_t *dbg_icore;)

typedef enum perm_type_t {
	PERM_CHAIN,
	PERM_CYCLE
} perm_type_t;

typedef struct perm_op_t {
	perm_type_t type;
	unsigned    regs[NUM_REGISTERS];
	unsigned    length;
} perm_op_t;

static unsigned   sourceof[NUM_REGISTERS];
static unsigned   usecount[NUM_REGISTERS];
static ir_node   *in_nodes[NUM_REGISTERS];
static ir_node   *out_nodes[NUM_REGISTERS];
static perm_op_t  ops[NUM_REGISTERS];
static unsigned   num_ops     = 0;
static ir_node   *sched_point = NULL;
static ir_node   *perm        = NULL;
static ir_mode   *perm_mode   = NULL;
static const arch_register_class_t *reg_class = NULL;

static void init_state()
{
	unsigned i;

	memset(usecount,  0, NUM_REGISTERS * sizeof(unsigned));
	memset(in_nodes,  0, NUM_REGISTERS * sizeof(ir_node *));
	memset(out_nodes, 0, NUM_REGISTERS * sizeof(ir_node *));
	memset(ops,       0, NUM_REGISTERS * sizeof(perm_op_t));
	for (i = 0; i < NUM_REGISTERS; ++i)
		sourceof[i] = i;
	num_ops     = 0;
	sched_point = NULL;
	perm        = NULL;
	perm_mode   = NULL;
	reg_class   = NULL;
}

static void save_perm_info()
{
	if (reg_class == NULL) {
		ir_node               *in  = get_irn_n(perm, 0);
		const arch_register_t *reg = arch_get_irn_register(in);

		reg_class = arch_register_get_class(reg);
		perm_mode = get_irn_mode(in);
	}
}

static const arch_register_t *get_arch_register_from_index(unsigned index)
{
	return arch_register_for_index(reg_class, index);
}

static const char *get_register_name_from_index(unsigned index)
{
	return arch_register_get_name(get_arch_register_from_index(index));
}

static void print_perm_op(const perm_op_t *op)
{
	unsigned i;

	printf("%s(%u)", get_register_name_from_index(op->regs[0]),
		op->regs[0]);
	for (i = 1; i < op->length; ++i)
		printf(" -> %s(%u)", get_register_name_from_index(op->regs[i]),
			op->regs[i]);

	if (op->type == PERM_CYCLE)
		printf(" -> %s(%u)", get_register_name_from_index(op->regs[0]),
			op->regs[0]);

	puts("");
}

static void schedule_node(ir_node *irn)
{
	sched_add_after(skip_Proj(sched_point), irn);
	sched_point = irn;
}

static void analyze_regs()
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
		out_nodes[oidx] = out;
		in_nodes[iidx]  = in;

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

	printf("  Found a chain: ");
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

	printf("  Found a cycle: ");
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

static void set_Permi_reg_reqs(ir_node *irn)
{
	const arch_register_req_t *req;
	const arch_register_req_t **in_reqs;
	struct obstack *obs;
	int i;
	const int arity = get_irn_arity(irn);

	assert(is_sparc_Permi(irn) || is_sparc_Permi23(irn));
	/* Get register requirement.  Assumes all input/registers belong to the
	 * same register class and have the same requirements. */
	req = reg_class->class_req;

	/* Set in register requirements. */
	obs = be_get_be_obst(get_irn_irg(irn));
	in_reqs = OALLOCNZ(obs, const arch_register_req_t*, arity);
	for (i = 0; i < arity; ++i) {
		in_reqs[i] = req;
	}
	arch_set_irn_register_reqs_in(irn, in_reqs);

	/* Set out register requirements. */
	for (i = 0; i < arity; ++i) {
		arch_set_irn_register_req_out(irn, i, req);
	}
}

static void split_chain_into_copies(const perm_op_t *op)
{
	ir_node *bb = get_nodes_block(perm);
	unsigned i;
	assert(op->type == PERM_CHAIN);

	for (i = 2; i <= op->length; ++i) {
		unsigned  in_idx  = op->regs[op->length - i];
		unsigned  out_idx = op->regs[op->length - i + 1];
		ir_node  *in      = in_nodes[in_idx];
		ir_node  *out     = out_nodes[out_idx];

		ir_node  *cpy     = be_new_Copy(bb, in);
		arch_set_irn_register(cpy, arch_get_irn_register(out));

		exchange(out, cpy);
		schedule_node(cpy);
	}
}

/* Chains g0 -> ... can never be turned into a cycle
 * because writing to g0 does not make sense. */
static void handle_zero_chains()
{
	unsigned i;
	unsigned j;

	for (i = 0; i < num_ops; ++i)
		if (ops[i].regs[0] == 0)
			split_chain_into_copies(&ops[i]);

	/* Remove all zero chains. */
	j = 0;
	for (i = 0; i < num_ops; ++i) {
		if (ops[i].regs[0] != 0) {
			if (j != i)
				ops[j] = ops[i];
			++j;
		}
	}
	num_ops = j;
}

static ir_node *create_chain_permi(const perm_op_t *op)
{
	ir_node *args[PERMI_SIZE];
	ir_node *ress[PERMI_SIZE];
	ir_node *permi;
	ir_node *bb     = get_nodes_block(perm);
	unsigned length = op->length;
	unsigned i;

	assert(op->type == PERM_CHAIN && op->length <= PERMI_SIZE);

	for (i = 0; i < length - 1; ++i) {
		args[i] = in_nodes[op->regs[i]];
		ress[i] = out_nodes[op->regs[i + 1]];
	}

	/* TODO: Fix debuginfo. */
	permi = new_bd_sparc_Permi_chain(NULL, bb, length - 1, args, length - 1);
	set_Permi_reg_reqs(permi);

	/* Rewire Projs. */
	for (i = 0; i < length - 1; ++i) {
		const unsigned  out  = (i + 1);
		ir_node        *proj = ress[i];

		set_Proj_pred(proj, permi);
		set_Proj_proj(proj, i);
		arch_set_irn_register_out(permi, i,
			get_arch_register_from_index(op->regs[out]));
	}

	return permi;
}

static void handle_chain(const perm_op_t *op)
{
	assert(op->regs[0] != 0);

	if (op->length <= PERMI_SIZE) {
		/* TODO: Implement long pseudo cycle. */
		schedule_node(create_chain_permi(op));
	} else
		split_chain_into_copies(op);
}

static ir_node *create_permi(const perm_op_t *op)
{
	ir_node *args[PERMI_SIZE];
	ir_node *ress[PERMI_SIZE];
	ir_node *permi;
	ir_node *bb     = get_nodes_block(perm);
	unsigned length = op->length;
	unsigned i;

	assert(op->type == PERM_CYCLE && op->length <= PERMI_SIZE);

	for (i = 0; i < length; ++i) {
		const unsigned in  = i;
		const unsigned out = (in + 1) % length;

		args[i] = in_nodes[op->regs[in]];
		ress[i] = out_nodes[op->regs[out]];
	}

	/* TODO: Fix debuginfo. */
	permi = new_bd_sparc_Permi_cycle(NULL, bb, length, args, length);
	set_Permi_reg_reqs(permi);

	/* Rewire Projs. */
	for (i = 0; i < length; ++i) {
		const unsigned  out  = (i + 1) % length;
		ir_node        *proj = ress[i];

		set_Proj_pred(proj, permi);
		set_Proj_proj(proj, i);
		arch_set_irn_register_out(permi, i,
			get_arch_register_from_index(op->regs[out]));
	}

	return permi;
}

static void split_big_cycle(const perm_op_t *op)
{
	unsigned i;
	unsigned j;
	const unsigned length = op->length;

	assert(length > PERMI_SIZE);

	/* Place every permi except for the last.
	 * All permis are of the maximum size PERMI_SIZE. */
	for (i = length; i > PERMI_SIZE; i -= (PERMI_SIZE - 1)) {
		const unsigned  start = i - PERMI_SIZE;
		const unsigned  last  = PERMI_SIZE - 1;
		perm_op_t       subop;
		ir_node        *permi;
		ir_node        *proj;

		subop.type   = PERM_CYCLE;
		subop.length = PERMI_SIZE;
		for (j = 0; j < PERMI_SIZE; ++j)
			subop.regs[j] = op->regs[start + j];

		permi = create_permi(&subop);
		schedule_node(permi);

		/* Create intermediate Proj. */
		proj = new_r_Proj(permi, perm_mode, last);
		in_nodes[subop.regs[0]] = proj;
		arch_set_irn_register(proj,
			get_arch_register_from_index(subop.regs[0]));

		sched_point = proj;
	}

	/* Place the last permi. */
	{
		const unsigned  lastlen = i;
		perm_op_t       lastop;
		ir_node        *permi;

		lastop.type   = PERM_CYCLE;
		lastop.length = lastlen;
		for (j = 0; j < lastlen; ++j)
			lastop.regs[j] = op->regs[j];

		permi = create_permi(&lastop);
		schedule_node(permi);
	}
}

static void handle_cycle(const perm_op_t *op)
{
	assert(op->type == PERM_CYCLE);

	if (op->length <= PERMI_SIZE)
		schedule_node(create_permi(op));
	else
		split_big_cycle(op);
}

static void handle_op(const perm_op_t *op)
{
	if (op->type == PERM_CHAIN)
		handle_chain(op);
	else
		handle_cycle(op);
}

static void combine_small_ops(const perm_op_t *op2, const perm_op_t *op3)
{
	assert(op2->length == 2 && op3->length >= 2 && op3->length <= 3);

	if (op2->type == PERM_CYCLE && op3->type == PERM_CYCLE) {
		ir_node *bb      = get_nodes_block(perm);
		ir_node *permi23;
		ir_node *args[PERMI_SIZE];
		ir_node *ress[PERMI_SIZE];
		const unsigned length = op2->length + op3->length;
		unsigned       i;

		/* Add cycle of size 2. */
		for (i = 0; i < 2; ++i) {
			const unsigned in  = i;
			const unsigned out = (in + 1) % 2;

			args[i] = in_nodes[op2->regs[in]];
			ress[i] = out_nodes[op2->regs[out]];
		}

		/* Add cycle of size 2 or 3. */
		for (i = 0; i < op3->length; ++i) {
			const unsigned in  = i;
			const unsigned out = (in + 1) % op3->length;

			args[2 + i] = in_nodes[op3->regs[in]];
			ress[2 + i] = out_nodes[op3->regs[out]];
		}

		permi23 = new_bd_sparc_Permi23_cycle_cycle(NULL, bb, length, args, length);
		set_Permi_reg_reqs(permi23);

		/* Rewire Projs. */
		for (i = 0; i < 2; ++i) {
			const unsigned  out  = (i + 1) % 2;
			ir_node        *proj = ress[i];

			set_Proj_pred(proj, permi23);
			set_Proj_proj(proj, i);
			arch_set_irn_register_out(permi23, i,
				get_arch_register_from_index(op2->regs[out]));
		}

		for (i = 0; i < op3->length; ++i) {
			const unsigned  out  = (i + 1) % op3->length;
			ir_node        *proj = ress[2 + i];

			set_Proj_pred(proj, permi23);
			set_Proj_proj(proj, 2 + i);
			arch_set_irn_register_out(permi23, 2 + i,
				get_arch_register_from_index(op3->regs[out]));
		}

		schedule_node(permi23);
	} else {
		/* TODO: Implement all cases. */
		handle_op(op2);
		handle_op(op3);
	}
}

static void search_combinable_ops()
{
	const perm_op_t *twos[NUM_REGISTERS];
	const perm_op_t *threes[NUM_REGISTERS];
	unsigned num_twos    = 0;
	unsigned num_threes  = 0;
	unsigned used_threes = 0;
	unsigned i, j;

	/* Collect all perm ops of size 2 or 3, i.e.
	 *   - all chains of the form  x -> y  or  x -> y -> z
	 *   - all cycles of the form  x -> y -> x or  x -> y -> z -> x
	 */
	for (i = 0; i < num_ops; ++i) {
		const unsigned length = ops[i].length;

		if (length == 2)
			twos[num_twos++] = &ops[i];
		else if (length == 3)
			threes[num_threes++] = &ops[i];
	}

	/* Try to combine each op of size 2 with an op of size 3.
	 * If no op of size 3 available, try to combine it with op of size 2.
	 * If also not possible, handle it separately. */
	for (i = 0; i < num_twos; ++i) {
		if (used_threes < num_threes) {
			combine_small_ops(twos[i], threes[used_threes]);
			++used_threes;
		} else if (i + 1 < num_twos) {
			combine_small_ops(twos[i], twos[i + 1]);
			++i;
		} else {
			handle_op(twos[i]);
		}
	}

	/* Handle all leftover ops of size 3.
	 * Can happen, for example, if no op of size 2 was available. */
	for (i = used_threes; i < num_threes; ++i)
		handle_op(threes[i]);

	/* Remove all ops of size <= 3. */
	j = 0;
	for (i = 0; i < num_ops; ++i) {
		if (ops[i].length > 3) {
			if (j != i)
				ops[j] = ops[i];
			++j;
		}
	}
	num_ops = j;
}

static void analyze_perm()
{
	unsigned i;

	ir_printf("Analyzing %+F\n", perm);

	save_perm_info();
	analyze_regs();

	search_chains();
	search_cycles();

#ifdef DEBUG_libfirm
	for (i = 0; i < NUM_REGISTERS; ++i)
		assert(sourceof[i] == i);
#endif

	/* Handle all zero chains. */
	handle_zero_chains();

	/* Try to combine small ops into one instruction. */
	search_combinable_ops();

	/* Handle all remaining ops. */
	for (i = 0; i < num_ops; ++i) {
		assert(ops[i].length > 3);
		handle_op(&ops[i]);
	}

	ir_printf("Finished %+F\n", perm);
}

static void lower_perm_node(ir_node *irn)
{
	int arity = get_irn_arity(irn);

	init_state();

	/* Check preconditions. */
	assert(be_is_Perm(irn) && "Non-Perm node passed to lower_perm_node");
	assert(arity == get_irn_n_edges(irn)
	       && "Perm's in and out numbers differ");

	/* Get the schedule predecessor node to the perm. */
	sched_point = sched_prev(irn);
	assert(sched_point && "Perm is not scheduled or has no predecessor");

	perm = irn;
	analyze_perm();

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
