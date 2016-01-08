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
#include "icore_permana.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irdump.h"

#include "bearch.h"
#include "belower.h"
#include "benode.h"
#include "besched.h"
#include "belive.h"
#include "beirg.h"

#include "gen_sparc_new_nodes.h"

#include "lc_opts.h"
#include "irtools.h"
#include "execfreq_t.h"
#include "statev.h"

#define PERMI_SIZE 5
//#define USE_EXECFREQ

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static ir_node   *in_nodes[NUM_REGISTERS];
static ir_node   *out_nodes[NUM_REGISTERS];
static perm_op_t  ops[NUM_REGISTERS];
static unsigned   num_ops;
static int        num_permis;
static int        num_moves;
static ir_node   *sched_point;
static ir_node   *perm;
static ir_mode   *perm_mode;
static const arch_register_class_t *reg_class;

static int dump_graphs = 0;
static int only_cycles = 0;
static int single_movs = 0;


static void init_state(void)
{
	memset(in_nodes,  0, NUM_REGISTERS * sizeof(in_nodes[0]));
	memset(out_nodes, 0, NUM_REGISTERS * sizeof(out_nodes[0]));
	memset(ops,       0, NUM_REGISTERS * sizeof(ops[0]));
	num_ops     = 0;
	num_permis  = 0;
	num_moves   = 0;
	sched_point = NULL;
	perm        = NULL;
	perm_mode   = NULL;
	reg_class   = NULL;
}

static const arch_register_t *get_arch_register_from_index(unsigned index)
{
	return arch_register_for_index(reg_class, index);
}

static void save_perm_info(void)
{
	if (reg_class == NULL) {
		ir_node               *in  = get_irn_n(perm, 0);
		const arch_register_t *reg = arch_get_irn_register(in);

		reg_class = reg->reg_class;
		perm_mode = get_irn_mode(in);
	}
}

static void schedule_node(ir_node *irn)
{
	sched_add_after(skip_Proj(sched_point), irn);
	sched_point = irn;
}

static void analyze_regs(void)
{
	foreach_out_edge_safe(perm, edge) {
		ir_node               *out     = get_edge_src_irn(edge);
		long                   pn      = get_Proj_proj(out);
		ir_node               *in      = get_irn_n(perm, pn);
		const arch_register_t *in_reg  = arch_get_irn_register(in);
		const arch_register_t *out_reg = arch_get_irn_register(out);
		unsigned               iidx;
		unsigned               oidx;

		/* Ignore registers that are left untouched by the Perm node. */
		if (in_reg == out_reg) {
			DB((dbg, LEVEL_3,
				"%+F removing equal perm register pair (%+F, %+F, %s)\n",
				perm, in, out, out_reg->name));

			exchange(out, in);
			continue;
		}

		oidx = out_reg->index;
		iidx = in_reg->index;
		out_nodes[oidx] = out;
		in_nodes[iidx]  = in;
	}
}

static void set_Permi_reg_reqs(ir_node *irn)
{
	const arch_register_req_t *req;
	const arch_register_req_t **in_reqs;
	struct obstack *obs;
	const unsigned arity = (unsigned) get_irn_arity(irn);

	assert(is_sparc_Permi(irn) || is_sparc_Permi23(irn));
	/* Get register requirements.  Assumes all input/registers belong to the
	 * same register class and have the same requirements. */
	req = reg_class->class_req;

	/* Set in register requirements. */
	obs = be_get_be_obst(get_irn_irg(irn));
	in_reqs = OALLOCNZ(obs, const arch_register_req_t*, arity);
	for (unsigned i = 0; i < arity; ++i) {
		in_reqs[i] = req;
	}
	arch_set_irn_register_reqs_in(irn, in_reqs);

	/* Set out register requirements. */
	const unsigned outs = arch_get_irn_n_outs(irn);
	for (unsigned i = 0; i < outs; ++i) {
		arch_set_irn_register_req_out(irn, i, req);
	}

	/* We are called each time we build a Permi insn. */
	++num_permis;
}

static ir_node *create_permi(const perm_op_t *op)
{
	ir_node  *args[PERMI_SIZE];
	ir_node  *ress[PERMI_SIZE];
	ir_node  *permi;
	dbg_info *dbgi  = get_irn_dbg_info(perm);
	ir_node *bb     = get_nodes_block(perm);
	unsigned length = op->length;
	unsigned i;

	assert(op->type == PERM_OP_CYCLE && op->length <= PERMI_SIZE);

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_2, "Implementing cycle: "));
	print_perm_op(op);
	DB((dbg, LEVEL_2, "\n"));
#endif

	for (i = 0; i < length; ++i) {
		const unsigned in  = i;
		const unsigned out = (in + 1) % length;

		args[i] = in_nodes[op->regs[in]];
		ress[i] = out_nodes[op->regs[out]];
	}

	permi = new_bd_sparc_Permi_cycle(dbgi, bb, length, args, length);
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

static ir_node *create_chain_permi(const perm_op_t *op)
{
	ir_node  *args[PERMI_SIZE];
	ir_node  *ress[PERMI_SIZE];
	ir_node  *permi;
	dbg_info *dbgi  = get_irn_dbg_info(perm);
	ir_node *bb     = get_nodes_block(perm);
	unsigned size   = op->length - 1;
	unsigned i;

	assert(op->type == PERM_OP_CHAIN && op->length <= PERMI_SIZE);

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_2, "Implementing chain: "));
	print_perm_op(op);
	DB((dbg, LEVEL_2, "\n"));
#endif

	for (i = 0; i < size; ++i) {
		args[i] = in_nodes[op->regs[i]];
		ress[i] = out_nodes[op->regs[i + 1]];
	}

	permi = new_bd_sparc_Permi_chain(dbgi, bb, size, args, size + 1);
	set_Permi_reg_reqs(permi);

	/* Rewire Projs. */
	for (i = 0; i < size; ++i) {
		const unsigned  out  = (i + 1);
		ir_node        *proj = ress[i];

		set_Proj_pred(proj, permi);
		set_Proj_proj(proj, i);
		arch_set_irn_register_out(permi, i,
			get_arch_register_from_index(op->regs[out]));
	}

	/* Set additional output to signal that we also write to the
	 * first register in our chain. */
	arch_set_irn_register_out(permi, size, get_arch_register_from_index(op->regs[0]));

	return permi;
}

static ir_node *create_permi23(ir_node **args, const perm_op_t *op2,
                               const perm_op_t *op3)
{
	ir_node  *bb      = get_nodes_block(perm);
	unsigned  sz      = op2->length + op3->length;
	dbg_info *dbgi    = get_irn_dbg_info(perm);
	ir_node  *permi23;

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_2, "Combining two small ops:\n"));
	DB((dbg, LEVEL_2, "  ")); print_perm_op(op2);
	DB((dbg, LEVEL_2, "  ")); print_perm_op(op3);
	DB((dbg, LEVEL_2, "\n"));
#endif

	if (op2->type == PERM_OP_CHAIN)
		--sz;
	if (op3->type == PERM_OP_CHAIN)
		--sz;

	if (op2->type == PERM_OP_CYCLE && op3->type == PERM_OP_CYCLE) {
		permi23 = new_bd_sparc_Permi23_cycle_cycle(dbgi, bb, sz, args, sz);
	} else if (op2->type == PERM_OP_CYCLE && op3->type == PERM_OP_CHAIN) {
		permi23 = new_bd_sparc_Permi23_cycle_chain(dbgi, bb, sz, args, sz + 1);
	} else if (op2->type == PERM_OP_CHAIN && op3->type == PERM_OP_CYCLE) {
		permi23 = new_bd_sparc_Permi23_chain_cycle(dbgi, bb, sz, args, sz + 1);
	} else if (op2->type == PERM_OP_CHAIN && op3->type == PERM_OP_CHAIN) {
		permi23 = new_bd_sparc_Permi23_chain_chain(dbgi, bb, sz, args, sz + 2);
	} else
		assert(!"Unknown perm ops");

	set_Permi_reg_reqs(permi23);
	return permi23;
}

static void split_chain_into_copies(const perm_op_t *op)
{
	ir_node *bb = get_nodes_block(perm);
	unsigned i;
	assert(op->type == PERM_OP_CHAIN);

	for (i = 2; i <= op->length; ++i) {
		unsigned  in_idx  = op->regs[op->length - i];
		unsigned  out_idx = op->regs[op->length - i + 1];
		ir_node  *in      = in_nodes[in_idx];
		ir_node  *out     = out_nodes[out_idx];

		ir_node  *cpy     = be_new_Copy(bb, in);
		arch_set_irn_register(cpy, arch_get_irn_register(out));
		++num_moves;

		exchange(out, cpy);
		schedule_node(cpy);
	}
}

/* Chains g0 -> ... can never be turned into a cycle
 * because writing to g0 does not make sense. */
static void handle_zero_chains(void)
{
	unsigned i;
	unsigned j;

	for (i = 0; i < num_ops; ++i)
		if (ops[i].regs[0] == REG_GP_G0)
			split_chain_into_copies(&ops[i]);

	/* Remove all zero chains. */
	j = 0;
	for (i = 0; i < num_ops; ++i) {
		if (ops[i].regs[0] != REG_GP_G0) {
			if (j != i)
				ops[j] = ops[i];
			++j;
		}
	}
	num_ops = j;
}

static void handle_all_chains(void)
{
	unsigned i;
	unsigned j;

	assert(only_cycles);

	for (i = 0; i < num_ops; ++i)
		if (ops[i].type == PERM_OP_CHAIN)
			split_chain_into_copies(&ops[i]);

	/* Keep only cycles. */
	j = 0;
	for (i = 0; i < num_ops; ++i) {
		if (ops[i].type != PERM_OP_CHAIN) {
			if (j != i)
				ops[j] = ops[i];
			++j;
		}
	}
	num_ops = j;
}

static void split_big_cycle(perm_op_t *op)
{
	const unsigned length = op->length;
	unsigned i;
	unsigned j;

	assert(op->type == PERM_OP_CYCLE && length >= PERMI_SIZE);

	/* Place as many permis of maximum size PERMI_SIZE as possible. */
	for (i = length; i >= PERMI_SIZE; i -= (PERMI_SIZE - 1)) {
		const unsigned  start = i - PERMI_SIZE;
		const unsigned  last  = PERMI_SIZE - 1;
		perm_op_t       subop;
		ir_node        *permi;
		ir_node        *proj;

		subop.type   = PERM_OP_CYCLE;
		subop.length = PERMI_SIZE;
		for (j = 0; j < PERMI_SIZE; ++j)
			subop.regs[j] = op->regs[start + j];

		permi = create_permi(&subop);
		schedule_node(permi);

		/* Check if another permi will be placed. */
		if (i >= PERMI_SIZE + PERMI_SIZE - 1) {
			/* Create intermediate Proj. */
			proj = new_r_Proj(permi, perm_mode, last);
			in_nodes[subop.regs[0]] = proj;
			arch_set_irn_register(proj,
				get_arch_register_from_index(subop.regs[0]));

			sched_point = proj;
		}
	}

	/* If there's something left, it must be of size < PERMI_SIZE.
	 * Remember it for combination with other small ops later. */
	if (i > 1) {
		const unsigned lastlen = i;

		op->type   = PERM_OP_CYCLE;
		op->length = lastlen;
	}
}

static void split_big_chain(perm_op_t *op)
{
	const unsigned length = op->length;
	unsigned i;
	unsigned j;

	assert(op->type == PERM_OP_CHAIN && length >= PERMI_SIZE);

	/* Place as many chain permis of maximum size PERMI_SIZE as possible. */
	for (i = length; i >= PERMI_SIZE; i -= (PERMI_SIZE - 1)) {
		const unsigned  start = i - PERMI_SIZE;
		perm_op_t       subop;
		ir_node        *permi;

		subop.type   = PERM_OP_CHAIN;
		subop.length = PERMI_SIZE;
		for (j = 0; j < PERMI_SIZE; ++j)
			subop.regs[j] = op->regs[start + j];

		permi = create_chain_permi(&subop);
		schedule_node(permi);
	}

	/* If there's something left, it must be of size < PERMI_SIZE.
	 * Remember it for combination with other small ops later. */
	if (i > 1) {
		const unsigned lastlen = i;

		op->type   = PERM_OP_CHAIN;
		op->length = lastlen;
	}
}

static void handle_big_ops(void)
{
	unsigned i;
	unsigned j;

	for (i = 0; i < num_ops; ++i) {
		perm_op_t *op = &ops[i];

		if (op->length >= PERMI_SIZE) {
			if (op->type == PERM_OP_CYCLE)
				split_big_cycle(op);
			else
				split_big_chain(op);
		}
	}

	/* Keep all small ops. */
	j = 0;
	for (i = 0; i < num_ops; ++i) {
		if (ops[i].length < PERMI_SIZE) {
			if (j != i)
				ops[j] = ops[i];
			++j;
		}
	}
	num_ops = j;
}

static void combine_small_ops(const perm_op_t *op2, const perm_op_t *op3)
{
	ir_node *args[PERMI_SIZE];
	ir_node *ress[PERMI_SIZE];
	ir_node *permi23;
	unsigned pos     = 0;
	unsigned i       = 0;

	assert(op2->length == 2 && op3->length >= 2 && op3->length <= 3);

	if (op2->type == PERM_OP_CYCLE) {
		for (i = 0; i < 2; ++i) {
			const unsigned in  = i;
			const unsigned out = (in + 1) % 2;

			args[i] = in_nodes[op2->regs[in]];
			ress[i] = out_nodes[op2->regs[out]];
		}
		pos = 2;
	} else if (op2->type == PERM_OP_CHAIN) {
		args[0] = in_nodes[op2->regs[0]];
		ress[0] = out_nodes[op2->regs[1]];
		pos = 1;
	} else
		assert(!"Unknown perm op type");

	if (op3->type == PERM_OP_CYCLE) {
		for (i = 0; i < op3->length; ++i) {
			const unsigned in  = i;
			const unsigned out = (in + 1) % op3->length;

			args[pos + i] = in_nodes[op3->regs[in]];
			ress[pos + i] = out_nodes[op3->regs[out]];
		}
	} else if (op3->type == PERM_OP_CHAIN) {
		for (i = 0; i + 1 < op3->length; ++i) {
			const unsigned in  = i;
			const unsigned out = i + 1;

			args[pos + i] = in_nodes[op3->regs[in]];
			ress[pos + i] = out_nodes[op3->regs[out]];
		}
	} else
		assert(!"Unknown perm op type");

	permi23   = create_permi23(args, op2, op3);
	int arity = get_irn_arity(permi23);

	/* Rewire Projs. */
	if (op2->type == PERM_OP_CYCLE) {
		for (i = 0; i < 2; ++i) {
			const unsigned  out  = (i + 1) % 2;
			ir_node        *proj = ress[i];

			set_Proj_pred(proj, permi23);
			set_Proj_proj(proj, i);
			arch_set_irn_register_out(permi23, i,
				get_arch_register_from_index(op2->regs[out]));
		}
	} else if (op2->type == PERM_OP_CHAIN) {
		ir_node *proj = ress[0];

		set_Proj_pred(proj, permi23);
		set_Proj_proj(proj, 0);
		arch_set_irn_register_out(permi23, 0,
			get_arch_register_from_index(op2->regs[1]));

		/* Set an additional output register (that noone will read) to
		 * preserve the information that we also write to the first
		 * register in the chain. */
		arch_set_irn_register_out(permi23, arity,
			get_arch_register_from_index(op2->regs[0]));
	} else
		assert(!"Unknown perm op type");

	if (op3->type == PERM_OP_CYCLE) {
		for (i = 0; i < op3->length; ++i) {
			const unsigned  out  = (i + 1) % op3->length;
			ir_node        *proj = ress[pos + i];

			set_Proj_pred(proj, permi23);
			set_Proj_proj(proj, pos + i);
			arch_set_irn_register_out(permi23, pos + i,
				get_arch_register_from_index(op3->regs[out]));
		}
	} else if (op3->type == PERM_OP_CHAIN) {
		for (i = 0; i + 1 < op3->length; ++i) {
			const unsigned  out  = (i + 1);
			ir_node        *proj = ress[pos + i];

			set_Proj_pred(proj, permi23);
			set_Proj_proj(proj, pos + i);
			arch_set_irn_register_out(permi23, pos + i,
				get_arch_register_from_index(op3->regs[out]));
		}

		/* Set an additional output register (that noone will read) to
		 * preserve the information that we also write to the first
		 * register in the chain. */
		const int pos = arity + (op2->type == PERM_OP_CHAIN ? 1 : 0);
		arch_set_irn_register_out(permi23, pos,
			get_arch_register_from_index(op3->regs[0]));
	} else
		assert(!"Unknown perm op type");

	schedule_node(permi23);
}

static void handle_small_op(const perm_op_t *op)
{
	assert(op->length < PERMI_SIZE);

	if (op->type == PERM_OP_CYCLE)
		schedule_node(create_permi(op));
	else if (single_movs || op->length >= 3)
		schedule_node(create_chain_permi(op));
	else /* op->type == PERM_OP_CHAIN && !single_movs && op->length == 2 */
		split_chain_into_copies(op);
}

static void shave_off_op(perm_op_t *op, perm_op_t *part, unsigned len)
{
	unsigned i;

	assert(op->length >= 3 && op->length < PERMI_SIZE && op->length > len);

	part->type   = op->type;
	part->length = len;
	for (i = 0; i < len; ++i) {
		part->regs[i] = op->regs[op->length - len + i];
	}

	op->length -= (len - 1);
}

static void handle_small_ops(void)
{
	perm_op_t *twos  [NUM_REGISTERS];
	perm_op_t *threes[NUM_REGISTERS];
	perm_op_t *fours [NUM_REGISTERS];
	unsigned num_twos    = 0;
	unsigned num_threes  = 0;
	unsigned num_fours   = 0;
	unsigned used_twos   = 0;
	unsigned used_threes = 0;
	unsigned used_fours  = 0;
	unsigned i;

	/* Scan remaining ops. */
	for (i = 0; i < num_ops; ++i) {
		assert(ops[i].length < PERMI_SIZE);

		switch (ops[i].length) {
		case 2: twos  [num_twos++]   = ops + i; break;
		case 3: threes[num_threes++] = ops + i; break;
		case 4: fours [num_fours++]  = ops + i; break;
		default: assert(!"Invalid op size");
		}
	}

	while (used_twos < num_twos || used_threes < num_threes
	       || used_fours < num_fours) {

		/* Check if there's a three op remaining. */
		if (used_threes < num_threes) {
			if (used_twos < num_twos) {
				/* Perfect match, combine the two. */
				combine_small_ops(twos[used_twos++], threes[used_threes++]);
			} else if (used_fours < num_fours) {
				perm_op_t  swap;
				perm_op_t *shave = fours[used_fours++];

				/* Shave off a swap from a four op. */
				shave_off_op(shave, &swap, 2);
				combine_small_ops(&swap, threes[used_threes++]);

				/* Save remainder. */
				threes[num_threes++] = shave;
			} else if (used_threes + 1 < num_threes) {
				perm_op_t  swap;
				perm_op_t *shave = threes[used_threes++];

				/* Shave off a swap from a three op. */
				shave_off_op(shave, &swap, 2);
				combine_small_ops(&swap, threes[used_threes++]);

				/* Save remainder. */
				twos[num_twos++] = shave;
			} else {
				/* Single remaining three op. */
				handle_small_op(threes[used_threes++]);
			}
		} else if (used_twos < num_twos) {
			/* At this point: No three ops remaining. */

			if (used_fours < num_fours) {
				perm_op_t  three;
				perm_op_t *shave = fours[used_fours++];

				/* Shave off a three op from a four op. */
				shave_off_op(shave, &three, 3);
				combine_small_ops(twos[used_twos++], &three);

				/* Save remainder. */
				twos[num_twos++] = shave;
			} else if (used_twos + 1 < num_twos) {
				combine_small_ops(twos[used_twos], twos[used_twos + 1]);
				used_twos += 2;
			} else {
				/* Single remaining two op. */
				handle_small_op(twos[used_twos++]);
			}
		} else {
			/* Only four ops remaining. */
			for (; used_fours < num_fours; ++used_fours) {
				handle_small_op(fours[used_fours]);
			}
		}
	}
}

#ifdef DEBUG_libfirm
static void emit_stat_events(void)
{
	ir_node *block = get_nodes_block(perm);
	stat_ev_int("perm_block_nr", get_irn_node_nr(block));

#ifdef USE_EXECFREQ
	ir_graph *irg = get_irn_irg(block);
	ir_execfreq_int_factors ef_factors;
	ir_calculate_execfreq_int_factors(&ef_factors, irg);
	int count = get_block_execfreq_int(&ef_factors, block);
	stat_ev_int("perm_exec_count", count);
#endif

	unsigned rtg_nodes = 0;
	for (unsigned i = 0; i < num_ops; ++i) {
		rtg_nodes += ops[i].length;
		if (ops[i].type == PERM_OP_CYCLE)
			stat_ev_int("perm_cycle_size", ops[i].length);
		else if (ops[i].type == PERM_OP_CHAIN)
			stat_ev_int("perm_chain_size", ops[i].length);
		else
			assert(!"Invalid perm op type");
	}
	stat_ev_int("perm_num_rtg_nodes", (int)rtg_nodes);
}
#endif

static void lower_perm(void)
{
	assert(perm && "No Perm node has been set");
	DB((dbg, LEVEL_2, "Lowering %+F\n", perm));

	save_perm_info();
	analyze_regs();

	num_ops = analyze_perm(perm, ops);

	/* Handle all zero chains. */
	handle_zero_chains();

	/* For non-empty register transfer graphs, emit statistic events. */
	const bool emit_stats = num_ops > 0;
#ifdef DEBUG_libfirm
	if (emit_stats) {
		num_permis = 0;
		stat_ev_ctx_push_fmt("perm_stats", "%ld", get_irn_node_nr(perm));
		emit_stat_events();
	}
#endif

	if (only_cycles)
		handle_all_chains();

	/* Place as many permis of maximum size as possible. */
	handle_big_ops();

#ifdef DEBUG_libfirm
	for (unsigned i = 0; i < num_ops; ++i)
		assert(ops[i].length >= 2 && ops[i].length < PERMI_SIZE);
#endif

	/* Try to combine small ops efficiently. */
	handle_small_ops();

#ifdef DEBUG_libfirm
	if (emit_stats) {
		const int num_insns = num_permis + num_moves;
		if (num_insns > 0) {
			stat_ev_int("perm_num_insns", num_insns);
			stat_ev_int("perm_icore_num_permis", num_permis);
		}
		stat_ev_ctx_pop("perm_stats");
	}

	DB((dbg, LEVEL_2, "Finished %+F\n", perm));
#endif
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
	lower_perm();

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
//	int perm_stayed;
	(void) walk_env;

	if (!be_is_Perm(irn))
		return;

//	perm_stayed = push_through_perm(irn);
//	if (perm_stayed)
		lower_perm_node(irn);
}

void icore_lower_nodes_after_ra(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.lower.icore");

	/* we will need interference */
	be_assure_live_chk(irg);

	if (dump_graphs)
		dump_ir_graph(irg, "before_icore_lowering");

	irg_walk_graph(irg, NULL, lower_nodes_after_ra_walker, NULL);

	if (dump_graphs)
		dump_ir_graph(irg, "after_icore_lowering");
}

static const lc_opt_table_entry_t icore_lowerperm_options[] = {
	LC_OPT_ENT_BOOL("dump_lower", "dump graphs before/after icore lowering", &dump_graphs),
	LC_OPT_ENT_BOOL("only_cycles", "only turn real cycles into permis", &only_cycles),
	LC_OPT_ENT_BOOL("single_movs", "generate permi for single movs", &single_movs),
	LC_OPT_LAST
};

void icore_init_lowerperm(void)
{
	lc_opt_entry_t *be_grp;
	lc_opt_entry_t *sparc_grp;
	lc_opt_entry_t *icore_grp;

	be_grp    = lc_opt_get_grp(firm_opt_get_root(), "be");
	sparc_grp = lc_opt_get_grp(be_grp, "sparc");
	icore_grp = lc_opt_get_grp(sparc_grp, "icore");
	lc_opt_add_table(icore_grp, icore_lowerperm_options);
}
