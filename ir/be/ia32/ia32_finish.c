/**
 * This file implements functions to finalize the irg for emit.
 * @author Christian Wuerdig
 * $Id$
 */

#include "irnode.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "pdeq.h"

#include "../bearch.h"
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

/**
 * Transforms a Sub or xSub into Neg--Add iff OUT_REG == SRC2_REG.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
static void ia32_transform_sub_to_neg_add(ir_node *irn, ia32_code_gen_t *cg) {
	ia32_transform_env_t tenv;
	ir_node *in1, *in2, *noreg, *nomem, *res;
	const arch_register_t *in1_reg, *in2_reg, *out_reg, **slots;

	/* Return if AM node or not a Sub or xSub */
	if (get_ia32_op_type(irn) != ia32_Normal || !(is_ia32_Sub(irn) || is_ia32_xSub(irn)))
		return;

	noreg   = ia32_new_NoReg_gp(cg);
	nomem   = new_rd_NoMem(cg->irg);
	in1     = get_irn_n(irn, 2);
	in2     = get_irn_n(irn, 3);
	in1_reg = arch_get_irn_register(cg->arch_env, in1);
	in2_reg = arch_get_irn_register(cg->arch_env, in2);
	out_reg = get_ia32_out_reg(irn, 0);

	tenv.block    = get_nodes_block(irn);
	tenv.dbg      = get_irn_dbg_info(irn);
	tenv.irg      = cg->irg;
	tenv.irn      = irn;
	tenv.mode     = get_ia32_res_mode(irn);
	tenv.cg       = cg;
	DEBUG_ONLY(tenv.mod      = cg->mod;)

	/* in case of sub and OUT == SRC2 we can transform the sequence into neg src2 -- add */
	if (REGS_ARE_EQUAL(out_reg, in2_reg)) {
		/* generate the neg src2 */
		res = gen_Minus_ex(&tenv, in2);
		arch_set_irn_register(cg->arch_env, res, in2_reg);

		/* add to schedule */
		sched_add_before(irn, get_Proj_pred(res));
		sched_add_before(irn, res);

		/* generate the add */
		if (mode_is_float(tenv.mode)) {
			res = new_rd_ia32_xAdd(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, res, in1, nomem);
			set_ia32_am_support(res, ia32_am_Source);
		}
		else {
			res = new_rd_ia32_Add(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, res, in1, nomem);
			set_ia32_am_support(res, ia32_am_Full);
			set_ia32_commutative(res);
		}
	    set_ia32_res_mode(res, tenv.mode);

		SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(tenv.cg, irn));
		/* copy register */
		slots    = get_ia32_slots(res);
		slots[0] = in2_reg;

		/* add to schedule */
		sched_add_before(irn, res);

		/* remove the old sub */
		sched_remove(irn);

		DBG_OPT_SUB2NEGADD(irn, res);

		/* exchange the add and the sub */
		exchange(irn, res);
	}
}

/**
 * Transforms a LEA into an Add if possible
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
static void ia32_transform_lea_to_add(ir_node *irn, ia32_code_gen_t *cg) {
	ia32_am_flavour_t am_flav;
	int               imm = 0;
	ir_node          *res = NULL;
	ir_node          *nomem, *noreg, *base, *index, *op1, *op2;
	char             *offs;
	ia32_transform_env_t tenv;
	const arch_register_t *out_reg, *base_reg, *index_reg;

	/* must be a LEA */
	if (! is_ia32_Lea(irn))
		return;

	am_flav = get_ia32_am_flavour(irn);

	if (get_ia32_am_sc(irn))
		return;

	/* only some LEAs can be transformed to an Add */
	if (am_flav != ia32_am_B && am_flav != ia32_am_OB && am_flav != ia32_am_OI && am_flav != ia32_am_BI)
		return;

	noreg = ia32_new_NoReg_gp(cg);
	nomem = new_rd_NoMem(cg->irg);
	op1   = noreg;
	op2   = noreg;
	base  = get_irn_n(irn, 0);
	index = get_irn_n(irn,1);

	offs  = get_ia32_am_offs(irn);

	/* offset has a explicit sign -> we need to skip + */
	if (offs && offs[0] == '+')
		offs++;

	out_reg   = arch_get_irn_register(cg->arch_env, irn);
	base_reg  = arch_get_irn_register(cg->arch_env, base);
	index_reg = arch_get_irn_register(cg->arch_env, index);

	tenv.block = get_nodes_block(irn);
	tenv.dbg   = get_irn_dbg_info(irn);
	tenv.irg   = cg->irg;
	tenv.irn   = irn;
	DEBUG_ONLY(tenv.mod   = cg->mod;)
	tenv.mode  = get_irn_mode(irn);
	tenv.cg    = cg;

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
		case ia32_am_OI:
			/* out register must be same as index register */
			if (! REGS_ARE_EQUAL(out_reg, index_reg))
				return;

			op1 = index;
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
		default:
			break;
	}

	res = new_rd_ia32_Add(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, op1, op2, nomem);
	arch_set_irn_register(cg->arch_env, res, out_reg);
	set_ia32_op_type(res, ia32_Normal);
	set_ia32_commutative(res);
	set_ia32_res_mode(res, tenv.mode);

	if (imm) {
		set_ia32_cnst(res, offs);
		set_ia32_immop_type(res, ia32_ImmConst);
	}

	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(cg, irn));

	/* add Add to schedule */
	sched_add_before(irn, res);

	DBG_OPT_LEA2ADD(irn, res);

	res = new_rd_Proj(tenv.dbg, tenv.irg, tenv.block, res, tenv.mode, pn_ia32_Add_res);

	/* add result Proj to schedule */
	sched_add_before(irn, res);

	/* remove the old LEA */
	sched_remove(irn);

	/* exchange the Add and the LEA */
	exchange(irn, res);
}

static INLINE int need_constraint_copy(ir_node *irn) {
	return \
		! is_ia32_Lea(irn)          && \
		! is_ia32_Conv_I2I(irn)     && \
		! is_ia32_Conv_I2I8Bit(irn) && \
		! is_ia32_CmpCMov(irn)      && \
		! is_ia32_PsiCondCMov(irn)  && \
		! is_ia32_CmpSet(irn);
}

/**
 * Insert copies for all ia32 nodes where the should_be_same requirement
 * is not fulfilled.
 * Transform Sub into Neg -- Add if IN2 == OUT
 */
static void ia32_finish_node(ir_node *irn, void *env) {
	ia32_code_gen_t            *cg = env;
	const ia32_register_req_t **reqs;
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
				if (arch_register_req_is(&(reqs[i]->req), should_be_same)) {
					/* get in and out register */
					out_reg  = get_ia32_out_reg(irn, i);
					in_node  = get_irn_n(irn, reqs[i]->same_pos);
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
							in2_node = get_irn_n(irn, reqs[i]->same_pos ^ 1);
							in2_reg  = arch_get_irn_register(cg->arch_env, in2_node);

							if (REGS_ARE_EQUAL(out_reg, in2_reg)) {
								set_irn_n(irn, reqs[i]->same_pos, in2_node);
								set_irn_n(irn, reqs[i]->same_pos ^ 1, in_node);
							}
							else
								goto insert_copy;
						}
						else {
insert_copy:
							DBG((cg->mod, LEVEL_1, "inserting copy for %+F in_pos %d\n", irn, reqs[i]->same_pos));
							/* create copy from in register */
							copy = be_new_Copy(arch_register_get_class(in_reg), cg->irg, block, in_node);

							DBG_OPT_2ADDRCPY(copy);

							/* destination is the out register */
							arch_set_irn_register(cg->arch_env, copy, out_reg);

							/* insert copy before the node into the schedule */
							sched_add_before(irn, copy);

							/* set copy as in */
							set_irn_n(irn, reqs[i]->same_pos, copy);
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

				set_ia32_pncode(irn, get_negated_pnc(pnc, mode_D));
			}
		}

		/*
			If we have a CondJmp/CmpSet/xCmpSet with immediate,
			we need to check if it's the right operand, otherwise
			we have to change it, as CMP doesn't support immediate
			as left operands.
		*/
#if 0
		if ((is_ia32_CondJmp(irn) || is_ia32_CmpSet(irn) || is_ia32_xCmpSet(irn)) &&
			(is_ia32_ImmConst(irn) || is_ia32_ImmSymConst(irn))                   &&
			op_tp == ia32_AddrModeS)
		{
			set_ia32_op_type(irn, ia32_AddrModeD);
			set_ia32_pncode(irn, get_inversed_pnc(get_ia32_pncode(irn)));
		}
#endif
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
	const ia32_register_req_t **reqs;
	int n_res, i;

	/* check only ia32 nodes with source address mode */
	if (! is_ia32_irn(irn) || get_ia32_op_type(irn) != ia32_AddrModeS)
		return;

	base  = get_irn_n(irn, 0);
	index = get_irn_n(irn, 1);

	reg_base  = arch_get_irn_register(cg->arch_env, base);
	reg_index = arch_get_irn_register(cg->arch_env, index);
	reqs      = get_ia32_out_req_all(irn);

	noreg = ia32_new_NoReg_gp(cg);

	n_res = get_ia32_n_res(irn);

	for (i = 0; i < n_res; i++) {
		if (arch_register_req_is(&(reqs[i]->req), should_be_same)) {
			/* get in and out register */
			const arch_register_t *out_reg  = get_ia32_out_reg(irn, i);

			/*
				there is a constraint for the remaining operand
				and the result register is equal to base or index register
			*/
			if (reqs[i]->same_pos == 2 &&
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
					assert(0 && "cannot turn back address mode for this register class");
				}

				/* copy address mode information to load */
				set_ia32_ls_mode(load, ls_mode);
				set_ia32_am_flavour(load, get_ia32_am_flavour(irn));
				set_ia32_op_type(load, ia32_AddrModeS);
				set_ia32_am_support(load, ia32_am_Source);
				set_ia32_am_scale(load, get_ia32_am_scale(irn));
				set_ia32_am_sc(load, get_ia32_am_sc(irn));
				add_ia32_am_offs(load, get_ia32_am_offs(irn));
				set_ia32_frame_ent(load, get_ia32_frame_ent(irn));

				if (is_ia32_use_frame(irn))
					set_ia32_use_frame(load);

				/* insert the load into schedule */
				sched_add_before(irn, load);

				DBG((cg->mod, LEVEL_3, "irg %+F: build back AM source for node %+F, inserted load %+F\n", cg->irg, irn, load));

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

		if (is_ia32_irn(irn)) {
			/* check if there is a sub which need to be transformed */
			ia32_transform_sub_to_neg_add(irn, cg);

			/* transform a LEA into an Add if possible */
			ia32_transform_lea_to_add(irn, cg);

			/* check for peephole optimization */
			ia32_peephole_optimization(irn, cg);
		}
	}

	/* second: insert copies and finish irg */
	for (irn = sched_first(block); ! sched_is_end(irn); irn = next) {
		next = sched_next(irn);
		ia32_finish_node(irn, env);
	}
}

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
