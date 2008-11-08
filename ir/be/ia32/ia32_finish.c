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
 * @brief   This file implements functions to finalize the irg for emit.
 * @author  Christian Wuerdig
 * @version $Id$
 */
#include "config.h"

#include "irnode.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "iredges.h"
#include "irprintf.h"
#include "pdeq.h"
#include "error.h"

#include "../bearch_t.h"
#include "../besched_t.h"
#include "../benode_t.h"

#include "bearch_ia32_t.h"
#include "ia32_finish.h"
#include "ia32_new_nodes.h"
#include "ia32_map_regs.h"
#include "ia32_common_transform.h"
#include "ia32_transform.h"
#include "ia32_dbg_stat.h"
#include "ia32_optimize.h"
#include "gen_ia32_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Transforms a Sub or xSub into Neg--Add iff OUT_REG == SRC2_REG.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
static void ia32_transform_sub_to_neg_add(ir_node *irn, ia32_code_gen_t *cg)
{
	ir_graph *irg;
	ir_node *in1, *in2, *noreg, *nomem, *res;
	ir_node *noreg_fp, *block;
	dbg_info *dbg;
	const arch_register_t *in1_reg, *in2_reg, *out_reg;

	/* fix_am will solve this for AddressMode variants */
	if (get_ia32_op_type(irn) != ia32_Normal)
		return;

	noreg    = ia32_new_NoReg_gp(cg);
	noreg_fp = ia32_new_NoReg_xmm(cg);
	nomem    = new_NoMem();
	in1      = get_irn_n(irn, n_ia32_binary_left);
	in2      = get_irn_n(irn, n_ia32_binary_right);
	in1_reg  = arch_get_irn_register(in1);
	in2_reg  = arch_get_irn_register(in2);
	out_reg  = arch_irn_get_register(irn, 0);

	irg     = cg->irg;
	block   = get_nodes_block(irn);

	/* in case of sub and OUT == SRC2 we can transform the sequence into neg src2 -- add */
	if (out_reg != in2_reg)
		return;

	dbg = get_irn_dbg_info(irn);

	/* generate the neg src2 */
	if (is_ia32_xSub(irn)) {
		int size;
		ir_entity *entity;
		ir_mode *op_mode = get_ia32_ls_mode(irn);

		assert(get_irn_mode(irn) != mode_T);

		res = new_rd_ia32_xXor(dbg, irg, block, noreg, noreg, nomem, in2, noreg_fp);
		size = get_mode_size_bits(op_mode);
		entity = ia32_gen_fp_known_const(size == 32 ? ia32_SSIGN : ia32_DSIGN);
		set_ia32_am_sc(res, entity);
		set_ia32_op_type(res, ia32_AddrModeS);
		set_ia32_ls_mode(res, op_mode);

		arch_set_irn_register(res, in2_reg);

		/* add to schedule */
		sched_add_before(irn, res);

		/* generate the add */
		res = new_rd_ia32_xAdd(dbg, irg, block, noreg, noreg, nomem, res, in1);
		set_ia32_ls_mode(res, get_ia32_ls_mode(irn));

		/* exchange the add and the sub */
		edges_reroute(irn, res, irg);

		/* add to schedule */
		sched_add_before(irn, res);
	} else {
		ir_node         *res_proj   = NULL;
		ir_node         *flags_proj = NULL;
		const ir_edge_t *edge;

		if (get_irn_mode(irn) == mode_T) {
			/* collect the Proj uses */
			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				long     pn   = get_Proj_proj(proj);
				if (pn == pn_ia32_Sub_res) {
					assert(res_proj == NULL);
					res_proj = proj;
				} else {
					assert(pn == pn_ia32_Sub_flags);
					assert(flags_proj == NULL);
					flags_proj = proj;
				}
			}
		}

		if (flags_proj == NULL) {
			res = new_rd_ia32_Neg(dbg, irg, block, in2);
			arch_set_irn_register(res, in2_reg);

			/* add to schedule */
			sched_add_before(irn, res);

			/* generate the add */
			res = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, nomem, res, in1);
			arch_set_irn_register(res, out_reg);
			set_ia32_commutative(res);

			/* exchange the add and the sub */
			edges_reroute(irn, res, irg);

			/* add to schedule */
			sched_add_before(irn, res);
		} else {
			ir_node *stc, *cmc, *not, *adc;
			ir_node *adc_flags;

			/*
			 * ARG, the above technique does NOT set the flags right.
			 * So, we must produce the following code:
			 * t1 = ~b
			 * t2 = a + ~b + Carry
			 * Complement Carry
			 *
			 * a + -b = a + (~b + 1)  would set the carry flag IF a == b ...
			 */
			not = new_rd_ia32_Not(dbg, irg, block, in2);
			arch_set_irn_register(not, in2_reg);
			sched_add_before(irn, not);

			stc = new_rd_ia32_Stc(dbg, irg, block);
			arch_set_irn_register(stc, &ia32_flags_regs[REG_EFLAGS]);
			sched_add_before(irn, stc);

			adc = new_rd_ia32_Adc(dbg, irg, block, noreg, noreg, nomem, not,
			                      in1, stc);
			arch_set_irn_register(adc, out_reg);
			sched_add_before(irn, adc);

			set_irn_mode(adc, mode_T);
			adc_flags = new_r_Proj(irg, block, adc, mode_Iu, pn_ia32_Adc_flags);
			arch_set_irn_register(adc_flags, &ia32_flags_regs[REG_EFLAGS]);

			cmc = new_rd_ia32_Cmc(dbg, irg, block, adc_flags);
			arch_set_irn_register(cmc, &ia32_flags_regs[REG_EFLAGS]);
			sched_add_before(irn, cmc);

			exchange(flags_proj, cmc);
			if (res_proj != NULL) {
				set_Proj_pred(res_proj, adc);
				set_Proj_proj(res_proj, pn_ia32_Adc_res);
			}

			res = adc;
		}
	}

	set_irn_mode(res, get_irn_mode(irn));

	SET_IA32_ORIG_NODE(res, irn);

	/* remove the old sub */
	sched_remove(irn);
	kill_node(irn);

	DBG_OPT_SUB2NEGADD(irn, res);
}

static inline int need_constraint_copy(ir_node *irn)
{
	/* TODO this should be determined from the node specification */
	switch (get_ia32_irn_opcode(irn)) {
		case iro_ia32_IMul: {
			/* the 3 operand form of IMul needs no constraint copy */
			ir_node *right = get_irn_n(irn, n_ia32_IMul_right);
			return !is_ia32_Immediate(right);
		}

		case iro_ia32_Lea:
		case iro_ia32_Conv_I2I:
		case iro_ia32_Conv_I2I8Bit:
		case iro_ia32_CMov:
			return 0;

		default:
			return 1;
	}
}

/**
 * Returns the index of the "same" register.
 * On the x86, we should have only one.
 */
static int get_first_same(const arch_register_req_t* req)
{
	const unsigned other = req->other_same;
	int i;

	for (i = 0; i < 32; ++i) {
		if (other & (1U << i)) return i;
	}
	assert(! "same position not found");
	return 32;
}

/**
 * Insert copies for all ia32 nodes where the should_be_same requirement
 * is not fulfilled.
 * Transform Sub into Neg -- Add if IN2 == OUT
 */
static void assure_should_be_same_requirements(ia32_code_gen_t *cg,
                                               ir_node *node)
{
	ir_graph                   *irg      = cg->irg;
	const arch_register_req_t **reqs;
	const arch_register_t      *out_reg, *in_reg;
	int                         n_res, i;
	ir_node                    *in_node, *block;

	reqs  = get_ia32_out_req_all(node);
	n_res = arch_irn_get_n_outs(node);
	block = get_nodes_block(node);

	/* check all OUT requirements, if there is a should_be_same */
	for (i = 0; i < n_res; i++) {
		int                          i2, arity;
		int                          same_pos;
		ir_node                     *perm;
		ir_node                     *in[2];
		ir_node                     *perm_proj0;
		ir_node                     *perm_proj1;
		ir_node                     *uses_out_reg;
		const arch_register_req_t   *req = reqs[i];
		const arch_register_class_t *cls;
		int                         uses_out_reg_pos;

		if (!arch_register_req_is(req, should_be_same))
			continue;

		same_pos = get_first_same(req);

		/* get in and out register */
		out_reg  = arch_irn_get_register(node, i);
		in_node  = get_irn_n(node, same_pos);
		in_reg   = arch_get_irn_register(in_node);

		/* requirement already fulfilled? */
		if (in_reg == out_reg)
			continue;
		/* unknowns can be changed to any register we want on emitting */
		if (is_unknown_reg(in_reg))
			continue;
		cls = arch_register_get_class(in_reg);
		assert(cls == arch_register_get_class(out_reg));

		/* check if any other input operands uses the out register */
		arity = get_irn_arity(node);
		uses_out_reg     = NULL;
		uses_out_reg_pos = -1;
		for (i2 = 0; i2 < arity; ++i2) {
			ir_node               *in     = get_irn_n(node, i2);
			const arch_register_t *in_reg;

			if (!mode_is_data(get_irn_mode(in)))
				continue;

			in_reg = arch_get_irn_register(in);

			if (in_reg != out_reg)
				continue;

			if (uses_out_reg != NULL && in != uses_out_reg) {
				panic("invalid register allocation");
			}
			uses_out_reg = in;
			if (uses_out_reg_pos >= 0)
				uses_out_reg_pos = -1; /* multiple inputs... */
			else
				uses_out_reg_pos = i2;
		}

		/* no-one else is using the out reg, we can simply copy it
		 * (the register can't be live since the operation will override it
		 *  anyway) */
		if (uses_out_reg == NULL) {
			ir_node *copy = be_new_Copy(cls, irg, block, in_node);
			DBG_OPT_2ADDRCPY(copy);

			/* destination is the out register */
			arch_set_irn_register(copy, out_reg);

			/* insert copy before the node into the schedule */
			sched_add_before(node, copy);

			/* set copy as in */
			set_irn_n(node, same_pos, copy);

			DBG((dbg, LEVEL_1,
				"created copy %+F for should be same argument at input %d of %+F\n",
				copy, same_pos, node));
			continue;
		}

		/* for commutative nodes we can simply swap the left/right */
		if (uses_out_reg_pos == n_ia32_binary_right && is_ia32_commutative(node)) {
			ia32_swap_left_right(node);
			DBG((dbg, LEVEL_1,
				"swapped left/right input of %+F to resolve should be same constraint\n",
				node));
			continue;
		}

#ifdef DEBUG_libfirm
		ir_fprintf(stderr, "Note: need perm to resolve should_be_same constraint at %+F (this is unsafe and should not happen in theory...)\n", node);
#endif
		/* the out reg is used as node input: we need to permutate our input
		 * and the other (this is allowed, since the other node can't be live
		 * after! the operation as we will override the register. */
		in[0] = in_node;
		in[1] = uses_out_reg;
		perm  = be_new_Perm(cls, irg, block, 2, in);

		perm_proj0 = new_r_Proj(irg, block, perm, get_irn_mode(in[0]), 0);
		perm_proj1 = new_r_Proj(irg, block, perm, get_irn_mode(in[1]), 1);

		arch_set_irn_register(perm_proj0, out_reg);
		arch_set_irn_register(perm_proj1, in_reg);

		sched_add_before(node, perm);

		DBG((dbg, LEVEL_1,
			"created perm %+F for should be same argument at input %d of %+F (need permutate with %+F)\n",
			perm, same_pos, node, uses_out_reg));

		/* use the perm results */
		for (i2 = 0; i2 < arity; ++i2) {
			ir_node *in = get_irn_n(node, i2);

			if (in == in_node) {
				set_irn_n(node, i2, perm_proj0);
			} else if (in == uses_out_reg) {
				set_irn_n(node, i2, perm_proj1);
			}
		}
	}
}

/**
 * Following Problem:
 * We have a source address mode node with base or index register equal to
 * result register and unfulfilled should_be_same requirement. The constraint
 * handler will insert a copy from the remaining input operand to the result
 * register -> base or index is broken then.
 * Solution: Turn back this address mode into explicit Load + Operation.
 */
static void fix_am_source(ir_node *irn)
{
	const arch_register_req_t **reqs;
	int                         n_res, i;

	/* check only ia32 nodes with source address mode */
	if (!is_ia32_irn(irn) || get_ia32_op_type(irn) != ia32_AddrModeS)
		return;
	/* only need to fix binary operations */
	if (get_ia32_am_support(irn) != ia32_am_binary)
		return;

	reqs  = get_ia32_out_req_all(irn);
	n_res = arch_irn_get_n_outs(irn);

	for (i = 0; i < n_res; i++) {
		const arch_register_t *out_reg;
		int                    same_pos;
		ir_node               *same_node;
		const arch_register_t *same_reg;
		ir_node               *load_res;

		if (!arch_register_req_is(reqs[i], should_be_same))
			continue;

		/* get in and out register */
		out_reg   = arch_irn_get_register(irn, i);
		same_pos  = get_first_same(reqs[i]);
		same_node = get_irn_n(irn, same_pos);
		same_reg  = arch_get_irn_register(same_node);

		/* should_be same constraint is fullfilled, nothing to do */
		if (out_reg == same_reg)
			continue;

		/* we only need to do something if the out reg is the same as base
			 or index register */
		if (out_reg != arch_get_irn_register(get_irn_n(irn, n_ia32_base)) &&
				out_reg != arch_get_irn_register(get_irn_n(irn, n_ia32_index)))
			continue;

		load_res = turn_back_am(irn);
		arch_set_irn_register(load_res, out_reg);

		DBG((dbg, LEVEL_3,
			"irg %+F: build back AM source for node %+F, inserted load %+F\n",
			get_irn_irg(irn), irn, get_Proj_pred(load_res)));
		break;
	}
}

/**
 * Block walker: finishes a block
 */
static void ia32_finish_irg_walker(ir_node *block, void *env)
{
	ia32_code_gen_t *cg = env;
	ir_node *irn, *next;

	/* first: turn back AM source if necessary */
	for (irn = sched_first(block); ! sched_is_end(irn); irn = next) {
		next = sched_next(irn);
		fix_am_source(irn);
	}

	for (irn = sched_first(block); ! sched_is_end(irn); irn = next) {
		ia32_code_gen_t *cg = env;

		next = sched_next(irn);

		/* check if there is a sub which need to be transformed */
		if (is_ia32_Sub(irn) || is_ia32_xSub(irn)) {
			ia32_transform_sub_to_neg_add(irn, cg);
		}
	}

	/* second: insert copies and finish irg */
	for (irn = sched_first(block); ! sched_is_end(irn); irn = next) {
		next = sched_next(irn);
		if (is_ia32_irn(irn)) {
			/* some nodes are just a bit less efficient, but need no fixing if the
			 * should be same requirement is not fulfilled */
			if (need_constraint_copy(irn))
				assure_should_be_same_requirements(cg, irn);
		}
	}
}

/**
 * Block walker: pushes all blocks on a wait queue
 */
static void ia32_push_on_queue_walker(ir_node *block, void *env)
{
	waitq *wq = env;
	waitq_put(wq, block);
}


/**
 * Add Copy nodes for not fulfilled should_be_equal constraints
 */
void ia32_finish_irg(ir_graph *irg, ia32_code_gen_t *cg)
{
	waitq *wq = new_waitq();

	/* Push the blocks on the waitq because ia32_finish_irg_walker starts more walks ... */
	irg_block_walk_graph(irg, NULL, ia32_push_on_queue_walker, wq);

	while (! waitq_empty(wq)) {
		ir_node *block = waitq_get(wq);
		ia32_finish_irg_walker(block, cg);
	}
	del_waitq(wq);
}

void ia32_init_finish(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.finish");
}
