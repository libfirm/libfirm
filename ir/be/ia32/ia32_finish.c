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
 * @brief   This file implements functions to finalize the irg for emit.
 * @author  Christian Wuerdig
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "iredges.h"
#include "pdeq.h"
#include "error.h"

#include "../bearch_t.h"
#include "../besched_t.h"
#include "../benode_t.h"

#include "bearch_ia32_t.h"
#include "ia32_finish.h"
#include "ia32_new_nodes.h"
#include "ia32_map_regs.h"
#include "ia32_transform.h"
#include "ia32_dbg_stat.h"
#include "ia32_optimize.h"
#include "gen_ia32_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Transforms a Sub or xSub into Neg--Add iff OUT_REG == SRC2_REG.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
static void ia32_transform_sub_to_neg_add(ir_node *irn, ia32_code_gen_t *cg) {
	ir_graph *irg;
	ir_node *in1, *in2, *noreg, *nomem, *res;
	ir_node *noreg_fp, *block;
	ir_mode *mode = get_irn_mode(irn);
	dbg_info *dbg = get_irn_dbg_info(irn);
	const arch_register_t *in1_reg, *in2_reg, *out_reg, **slots;
	int i, arity;

	/* Return if AM node or not a Sub or xSub */
	if (!(is_ia32_Sub(irn) || is_ia32_xSub(irn)) || get_ia32_op_type(irn) != ia32_Normal)
		return;

	noreg   = ia32_new_NoReg_gp(cg);
	noreg_fp = ia32_new_NoReg_fp(cg);
	nomem   = new_rd_NoMem(cg->irg);
	in1     = get_irn_n(irn, 2);
	in2     = get_irn_n(irn, 3);
	in1_reg = arch_get_irn_register(cg->arch_env, in1);
	in2_reg = arch_get_irn_register(cg->arch_env, in2);
	out_reg = get_ia32_out_reg(irn, 0);

	irg     = cg->irg;
	block   = get_nodes_block(irn);

	/* in case of sub and OUT == SRC2 we can transform the sequence into neg src2 -- add */
	if (!REGS_ARE_EQUAL(out_reg, in2_reg))
		return;

	/* generate the neg src2 */
	if(mode_is_float(mode)) {
		int size;
		ir_entity *entity;

		res = new_rd_ia32_xXor(dbg, irg, block, noreg, noreg, in2, noreg_fp, nomem);
		size = get_mode_size_bits(mode);
		entity = ia32_gen_fp_known_const(size == 32 ? ia32_SSIGN : ia32_DSIGN);
		set_ia32_am_sc(res, entity);
		set_ia32_op_type(res, ia32_AddrModeS);
		set_ia32_ls_mode(res, get_ia32_ls_mode(irn));
	} else {
		res = new_rd_ia32_Neg(dbg, irg, block, noreg, noreg, in2, nomem);
	}
	arch_set_irn_register(cg->arch_env, res, in2_reg);

	/* add to schedule */
	sched_add_before(irn, res);

	/* generate the add */
	if (mode_is_float(mode)) {
		res = new_rd_ia32_xAdd(dbg, irg, block, noreg, noreg, res, in1, nomem);
		set_ia32_am_support(res, ia32_am_Source);
		set_ia32_ls_mode(res, get_ia32_ls_mode(irn));
	}
	else {
		res = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, res, in1, nomem);
		set_ia32_am_support(res, ia32_am_Full);
		set_ia32_commutative(res);
	}

	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(cg, irn));
	/* copy register */
	slots    = get_ia32_slots(res);
	slots[0] = in2_reg;

	/* exchange the add and the sub */
	edges_reroute(irn, res, irg);

	/* add to schedule */
	sched_add_before(irn, res);

	/* remove the old sub */
	sched_remove(irn);
	arity = get_irn_arity(irn);
	for(i = 0; i < arity; ++i) {
		set_irn_n(irn, i, new_Bad());
	}

	DBG_OPT_SUB2NEGADD(irn, res);
}

/**
 * Transforms a LEA into an Add or SHL if possible.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
static void ia32_transform_lea_to_add_or_shl(ir_node *irn, ia32_code_gen_t *cg) {
	ia32_am_flavour_t am_flav;
	int               imm = 0;
	dbg_info         *dbg = get_irn_dbg_info(irn);
	ir_graph         *irg;
	ir_node          *res = NULL;
	ir_node          *nomem, *noreg, *base, *index, *op1, *op2;
	ir_node          *block;
	int              offs = 0;
	const arch_register_t *out_reg, *base_reg, *index_reg;

	/* must be a LEA */
	if (! is_ia32_Lea(irn))
		return;

	am_flav = get_ia32_am_flavour(irn);

	/* mustn't have a symconst */
	if (get_ia32_am_sc(irn) != NULL || get_ia32_frame_ent(irn) != NULL)
		return;

	if (am_flav == ia32_am_IS) {
		tarval *tv;

		/* Create a SHL */
		noreg     = ia32_new_NoReg_gp(cg);
		nomem     = new_rd_NoMem(cg->irg);
		index     = get_irn_n(irn, 1);
		index_reg = arch_get_irn_register(cg->arch_env, index);
		out_reg   = arch_get_irn_register(cg->arch_env, irn);

		if (! REGS_ARE_EQUAL(out_reg, index_reg))
			return;

		/* ok, we can transform it */
		irg = cg->irg;
		block = get_nodes_block(irn);

		res  = new_rd_ia32_Shl(dbg, irg, block, noreg, noreg, index, noreg, nomem);
		offs = get_ia32_am_scale(irn);
		tv = new_tarval_from_long(offs, mode_Iu);
		set_ia32_Immop_tarval(res, tv);
		arch_set_irn_register(cg->arch_env, res, out_reg);
	} else {
		/* only some LEAs can be transformed to an Add */
		if (am_flav != ia32_am_B && am_flav != ia32_am_OB && am_flav != ia32_am_BI)
			return;

		noreg = ia32_new_NoReg_gp(cg);
		nomem = new_rd_NoMem(cg->irg);
		op1   = noreg;
		op2   = noreg;
		base  = get_irn_n(irn, 0);
		index = get_irn_n(irn, 1);

		if (am_flav & ia32_O) {
			offs  = get_ia32_am_offs_int(irn);
		}

		out_reg   = arch_get_irn_register(cg->arch_env, irn);
		base_reg  = arch_get_irn_register(cg->arch_env, base);
		index_reg = arch_get_irn_register(cg->arch_env, index);

		irg = cg->irg;
		block = get_nodes_block(irn);

		switch(get_ia32_am_flavour(irn)) {
			case ia32_am_B:
				/* out register must be same as base register */
				if (! REGS_ARE_EQUAL(out_reg, base_reg))
					return;

				op1 = base;
				break;
			case ia32_am_OB:
				/* out register must be same as base register */
				if (! REGS_ARE_EQUAL(out_reg, base_reg))
					return;

				op1 = base;
				imm = 1;
				break;
			case ia32_am_BI:
				/* out register must be same as one in register */
				if (REGS_ARE_EQUAL(out_reg, base_reg)) {
					op1 = base;
					op2 = index;
				}
				else if (REGS_ARE_EQUAL(out_reg, index_reg)) {
					op1 = index;
					op2 = base;
				}
				else {
					/* in registers a different from out -> no Add possible */
					return;
				}
				break;

			default:
				assert(0);
				break;
		}

		res = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, op1, op2, nomem);
		arch_set_irn_register(cg->arch_env, res, out_reg);
		set_ia32_op_type(res, ia32_Normal);
		set_ia32_commutative(res);

		if (imm) {
			tarval *tv = new_tarval_from_long(offs, mode_Iu);
			set_ia32_Immop_tarval(res, tv);
		}
	}

	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(cg, irn));

	/* add new ADD/SHL to schedule */
	sched_add_before(irn, res);

	DBG_OPT_LEA2ADD(irn, res);

	/* remove the old LEA */
	sched_remove(irn);

	/* exchange the Add and the LEA */
	exchange(irn, res);
}

static INLINE int need_constraint_copy(ir_node *irn) {
	return	! is_ia32_Lea(irn)          &&
		! is_ia32_Conv_I2I(irn)     &&
		! is_ia32_Conv_I2I8Bit(irn) &&
		! is_ia32_CmpCMov(irn)      &&
		! is_ia32_PsiCondCMov(irn)  &&
		! is_ia32_CmpSet(irn);
}

/**
 * Insert copies for all ia32 nodes where the should_be_same requirement
 * is not fulfilled.
 * Transform Sub into Neg -- Add if IN2 == OUT
 */
static void ia32_finish_node(ir_node *irn, void *env) {
	ia32_code_gen_t            *cg = env;
	const arch_register_req_t **reqs;
	const arch_register_t      *out_reg, *in_reg, *in2_reg;
	int                         n_res, i;
	ir_node                    *copy, *in_node, *block, *in2_node;
	ia32_op_type_t              op_tp;

	if (is_ia32_irn(irn)) {
		/* AM Dest nodes don't produce any values  */
		op_tp = get_ia32_op_type(irn);
		if (op_tp == ia32_AddrModeD)
			goto end;

		reqs  = get_ia32_out_req_all(irn);
		n_res = get_ia32_n_res(irn);
		block = get_nodes_block(irn);

		/* check all OUT requirements, if there is a should_be_same */
		if ((op_tp == ia32_Normal || op_tp == ia32_AddrModeS) && need_constraint_copy(irn))
		{
			for (i = 0; i < n_res; i++) {
				if (arch_register_req_is(reqs[i], should_be_same)) {
					int same_pos = reqs[i]->other_same;

					/* get in and out register */
					out_reg  = get_ia32_out_reg(irn, i);
					in_node  = get_irn_n(irn, same_pos);
					in_reg   = arch_get_irn_register(cg->arch_env, in_node);

					/* don't copy ignore nodes */
					if (arch_irn_is(cg->arch_env, in_node, ignore) && is_Proj(in_node))
						continue;

					/* check if in and out register are equal */
					if (! REGS_ARE_EQUAL(out_reg, in_reg)) {
						/* in case of a commutative op: just exchange the in's */
						/* beware: the current op could be everything, so test for ia32 */
						/*         commutativity first before getting the second in     */
						if (is_ia32_commutative(irn)) {
							in2_node = get_irn_n(irn, same_pos ^ 1);
							in2_reg  = arch_get_irn_register(cg->arch_env, in2_node);

							if (REGS_ARE_EQUAL(out_reg, in2_reg)) {
								set_irn_n(irn, same_pos, in2_node);
								set_irn_n(irn, same_pos ^ 1, in_node);
							}
							else
								goto insert_copy;
						}
						else {
insert_copy:
							DBG((dbg, LEVEL_1, "inserting copy for %+F in_pos %d\n", irn, same_pos));
							/* create copy from in register */
							copy = be_new_Copy(arch_register_get_class(in_reg), cg->irg, block, in_node);

							DBG_OPT_2ADDRCPY(copy);

							/* destination is the out register */
							arch_set_irn_register(cg->arch_env, copy, out_reg);

							/* insert copy before the node into the schedule */
							sched_add_before(irn, copy);

							/* set copy as in */
							set_irn_n(irn, same_pos, copy);
						}
					}
				}
			}
		}

		/* check xCmp: try to avoid unordered cmp */
		if ((is_ia32_xCmp(irn) || is_ia32_xCmpCMov(irn) || is_ia32_xCmpSet(irn)) &&
			op_tp == ia32_Normal    &&
			! is_ia32_ImmConst(irn) && ! is_ia32_ImmSymConst(irn))
		{
			long pnc = get_ia32_pncode(irn);

			if (pnc & pn_Cmp_Uo) {
				ir_node *tmp;
				int idx1 = 2, idx2 = 3;

				if (is_ia32_xCmpCMov(irn)) {
					idx1 = 0;
					idx2 = 1;
				}

				tmp = get_irn_n(irn, idx1);
				set_irn_n(irn, idx1, get_irn_n(irn, idx2));
				set_irn_n(irn, idx2, tmp);

				set_ia32_pncode(irn, get_negated_pnc(pnc, mode_E));
			}
		}
	}
end: ;
}

/**
 * Following Problem:
 * We have a source address mode node with base or index register equal to
 * result register. The constraint handler will insert a copy from the
 * remaining input operand to the result register -> base or index is
 * broken then.
 * Solution: Turn back this address mode into explicit Load + Operation.
 */
static void fix_am_source(ir_node *irn, void *env) {
	ia32_code_gen_t *cg = env;
	ir_node *base, *index, *noreg;
	const arch_register_t *reg_base, *reg_index;
	const arch_register_req_t **reqs;
	int n_res, i;

	/* check only ia32 nodes with source address mode */
	if (! is_ia32_irn(irn) || get_ia32_op_type(irn) != ia32_AddrModeS)
		return;
	/* no need to fix unary operations */
	if (get_irn_arity(irn) == 4)
		return;

	base  = get_irn_n(irn, 0);
	index = get_irn_n(irn, 1);

	reg_base  = arch_get_irn_register(cg->arch_env, base);
	reg_index = arch_get_irn_register(cg->arch_env, index);
	reqs      = get_ia32_out_req_all(irn);

	noreg = ia32_new_NoReg_gp(cg);

	n_res = get_ia32_n_res(irn);

	for (i = 0; i < n_res; i++) {
		if (arch_register_req_is(reqs[i], should_be_same)) {
			/* get in and out register */
			const arch_register_t *out_reg  = get_ia32_out_reg(irn, i);
			int same_pos = reqs[i]->other_same;

			/*
				there is a constraint for the remaining operand
				and the result register is equal to base or index register
			*/
			if (same_pos == 2 &&
				(REGS_ARE_EQUAL(out_reg, reg_base) || REGS_ARE_EQUAL(out_reg, reg_index)))
			{
				/* turn back address mode */
				ir_node               *in_node = get_irn_n(irn, 2);
				const arch_register_t *in_reg  = arch_get_irn_register(cg->arch_env, in_node);
				ir_node               *block   = get_nodes_block(irn);
				ir_mode               *ls_mode = get_ia32_ls_mode(irn);
				ir_node *load;
				int pnres;

				if (arch_register_get_class(in_reg) == &ia32_reg_classes[CLASS_ia32_gp]) {
					load  = new_rd_ia32_Load(NULL, cg->irg, block, base, index, get_irn_n(irn, 4));
					pnres = pn_ia32_Load_res;
				}
				else if (arch_register_get_class(in_reg) == &ia32_reg_classes[CLASS_ia32_xmm]) {
					load  = new_rd_ia32_xLoad(NULL, cg->irg, block, base, index, get_irn_n(irn, 4));
					pnres = pn_ia32_xLoad_res;
				}
				else {
					panic("cannot turn back address mode for this register class");
				}

				/* copy address mode information to load */
				set_ia32_ls_mode(load, ls_mode);
				set_ia32_am_flavour(load, get_ia32_am_flavour(irn));
				set_ia32_op_type(load, ia32_AddrModeS);
				set_ia32_am_support(load, ia32_am_Source);
				set_ia32_am_scale(load, get_ia32_am_scale(irn));
				set_ia32_am_sc(load, get_ia32_am_sc(irn));
				add_ia32_am_offs_int(load, get_ia32_am_offs_int(irn));
				set_ia32_frame_ent(load, get_ia32_frame_ent(irn));

				if (is_ia32_use_frame(irn))
					set_ia32_use_frame(load);

				/* insert the load into schedule */
				sched_add_before(irn, load);

				DBG((dbg, LEVEL_3, "irg %+F: build back AM source for node %+F, inserted load %+F\n", cg->irg, irn, load));

				load = new_r_Proj(cg->irg, block, load, ls_mode, pnres);
				arch_set_irn_register(cg->arch_env, load, out_reg);

				/* insert the load result proj into schedule */
				sched_add_before(irn, load);

				/* set the new input operand */
				set_irn_n(irn, 3, load);

				/* this is a normal node now */
				set_irn_n(irn, 0, noreg);
				set_irn_n(irn, 1, noreg);
				set_ia32_op_type(irn, ia32_Normal);

				break;
			}
		}
	}
}

/**
 * Block walker: finishes a block
 */
static void ia32_finish_irg_walker(ir_node *block, void *env) {
	ir_node *irn, *next;

	/* first: turn back AM source if necessary */
	for (irn = sched_first(block); ! sched_is_end(irn); irn = next) {
		next = sched_next(irn);
		fix_am_source(irn, env);
	}

	for (irn = sched_first(block); ! sched_is_end(irn); irn = next) {
		ia32_code_gen_t *cg = env;

		next = sched_next(irn);

		/* check if there is a sub which need to be transformed */
		ia32_transform_sub_to_neg_add(irn, cg);

		/* transform a LEA into an Add if possible */
		ia32_transform_lea_to_add_or_shl(irn, cg);
	}

	/* second: insert copies and finish irg */
	for (irn = sched_first(block); ! sched_is_end(irn); irn = next) {
		next = sched_next(irn);
		ia32_finish_node(irn, env);
	}
}

/**
 * Block walker: pushes all blocks on a wait queue
 */
static void ia32_push_on_queue_walker(ir_node *block, void *env) {
	waitq *wq = env;
	waitq_put(wq, block);
}


/**
 * Add Copy nodes for not fulfilled should_be_equal constraints
 */
void ia32_finish_irg(ir_graph *irg, ia32_code_gen_t *cg) {
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
