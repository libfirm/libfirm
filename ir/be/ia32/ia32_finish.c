/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   This file implements functions to finalize the irg for emit.
 * @author  Christian Wuerdig
 */
#include "irnode.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "iredges.h"
#include "pdeq.h"
#include "panic.h"
#include "firmstat_t.h"

#include "bearch.h"
#include "besched.h"
#include "benode.h"

#include "bearch_ia32_t.h"
#include "ia32_finish.h"
#include "ia32_new_nodes.h"
#include "ia32_common_transform.h"
#include "ia32_transform.h"
#include "ia32_dbg_stat.h"
#include "ia32_optimize.h"
#include "gen_ia32_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static bool reads_carry(x86_condition_code_t code)
{
	x86_condition_code_t c2 = code & ~x86_cc_negated;
	return c2 == x86_cc_below || c2 == x86_cc_below_equal
	    || c2 == x86_cc_float_below || c2 == x86_cc_float_below_equal
	    || c2 == x86_cc_float_unordered_below_equal
	    || c2 == x86_cc_float_unordered_below;
}

/**
 * Transforms a Sub or xSub into Neg--Add iff OUT_REG != SRC1_REG && OUT_REG == SRC2_REG.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
static void ia32_transform_sub_to_neg_add(ir_node *irn)
{
	/* fix_am will solve this for AddressMode variants */
	if (get_ia32_op_type(irn) != ia32_Normal)
		return;

	ir_graph              *irg      = get_irn_irg(irn);
	ir_node               *noreg    = ia32_new_NoReg_gp(irg);
	ir_node               *noreg_fp = ia32_new_NoReg_xmm(irg);
	ir_node               *nomem    = get_irg_no_mem(irg);
	ir_node               *in1      = get_irn_n(irn, n_ia32_binary_left);
	ir_node               *in2      = get_irn_n(irn, n_ia32_binary_right);
	const arch_register_t *in1_reg  = arch_get_irn_register(in1);
	const arch_register_t *in2_reg  = arch_get_irn_register(in2);
	const arch_register_t *out_reg  = arch_get_irn_register_out(irn, 0);

	if (out_reg == in1_reg)
		return;

	/* in case of sub and OUT == SRC2 we can transform the sequence into neg src2 -- add */
	if (out_reg != in2_reg)
		return;

	ir_node  *block = get_nodes_block(irn);
	dbg_info *dbgi  = get_irn_dbg_info(irn);

	/* generate the neg src2 */
	ir_node *res;
	if (is_ia32_xSub(irn)) {
		ir_mode *op_mode = get_ia32_ls_mode(irn);
		assert(get_irn_mode(irn) != mode_T);

		res = new_bd_ia32_xXor(dbgi, block, noreg, noreg, nomem, in2, noreg_fp);
		int        size   = get_mode_size_bits(op_mode);
		ir_entity *entity = ia32_gen_fp_known_const(irg, size == 32 ? ia32_SSIGN : ia32_DSIGN);
		set_ia32_am_ent(res, entity);
		set_ia32_op_type(res, ia32_AddrModeS);
		set_ia32_ls_mode(res, op_mode);

		arch_set_irn_register(res, in2_reg);

		/* add to schedule */
		sched_add_before(irn, res);

		/* generate the add */
		res = new_bd_ia32_xAdd(dbgi, block, noreg, noreg, nomem, res, in1);
		set_ia32_ls_mode(res, get_ia32_ls_mode(irn));
	} else {
		ir_node *flags_proj  = NULL;
		bool     needs_carry = false;
		/** See if someone is interested in a correctly set carry flag */
		if (get_irn_mode(irn) == mode_T) {
			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				long     pn   = get_Proj_proj(proj);
				if (pn == pn_ia32_flags) {
					assert(flags_proj == NULL);
					flags_proj = proj;
					foreach_out_edge(flags_proj, edge) {
						ir_node *user = get_edge_src_irn(edge);
						x86_condition_code_t cc = get_ia32_condcode(user);
						if (reads_carry(cc)) {
							needs_carry = true;
							break;
						}
					}
					break;
				}
			}
		}

		ir_node *carry;
		if (is_ia32_Sbb(irn)) {
			/* Feed borrow (in CF) as carry (via CMC) into NOT+ADC. */
			carry = get_irn_n(irn, n_ia32_Sbb_eflags);
			carry = new_bd_ia32_Cmc(dbgi, block, carry);
			goto carry;
		} else if (flags_proj != NULL && needs_carry) {
			/*
			 * ARG, the above technique does NOT set the flags right.
			 * So, we must produce the following code:
			 * t1 = ~b
			 * t2 = a + ~b + Carry
			 * Complement Carry
			 *
			 * a + -b = a + (~b + 1)  would set the carry flag wrong IFF both a and b are zero.
			 */
			carry = new_bd_ia32_Stc(dbgi, block);

carry:;
			ir_node *nnot = new_bd_ia32_Not(dbgi, block, in2);
			arch_set_irn_register(nnot, in2_reg);
			sched_add_before(irn, nnot);

			arch_set_irn_register(carry, &ia32_registers[REG_EFLAGS]);
			sched_add_before(irn, carry);

			ir_node *adc = new_bd_ia32_Adc(dbgi, block, noreg, noreg, nomem, nnot, in1, carry);
			arch_set_irn_register(adc, out_reg);
			set_ia32_commutative(adc);

			if (flags_proj != NULL) {
				set_irn_mode(adc, mode_T);
				ir_node *adc_flags = new_r_Proj(adc, ia32_mode_flags, pn_ia32_Adc_flags);
				arch_set_irn_register(adc_flags, &ia32_registers[REG_EFLAGS]);

				ir_node *cmc = new_bd_ia32_Cmc(dbgi, block, adc_flags);
				arch_set_irn_register(cmc, &ia32_registers[REG_EFLAGS]);
				sched_add_after(irn, cmc);
				exchange(flags_proj, cmc);
			}

			res = adc;
		} else {
			res = new_bd_ia32_Neg(dbgi, block, in2);
			arch_set_irn_register(res, in2_reg);

			/* add to schedule */
			sched_add_before(irn, res);

			/* generate the add */
			res = new_bd_ia32_Add(dbgi, block, noreg, noreg, nomem, res, in1);
			if (flags_proj != NULL) {
				arch_set_irn_register_out(res, pn_ia32_res, out_reg);
				arch_set_irn_register_out(res, pn_ia32_flags, &ia32_registers[REG_EFLAGS]);
			} else {
				arch_set_irn_register(res, out_reg);
			}
			set_ia32_commutative(res);
		}
	}

	/* exchange the add and the sub */
	edges_reroute(irn, res);
	sched_replace(irn, res);

	set_irn_mode(res, get_irn_mode(irn));

	SET_IA32_ORIG_NODE(res, irn);

	/* remove the old sub */
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
		case iro_ia32_CMovcc:
		case iro_ia32_Minus64:
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
	for (int i = 0; i < 32; ++i) {
		if (other & (1U << i))
			return i;
	}
	panic("same position not found");
}

/**
 * Insert copies for all ia32 nodes where the should_be_same requirement
 * is not fulfilled.
 * Transform Sub into Neg -- Add if IN2 == OUT
 */
static void assure_should_be_same_requirements(ir_node *node)
{
	ir_node *block = get_nodes_block(node);

	/* check all OUT requirements, if there is a should_be_same */
	be_foreach_out(node, i) {
		const arch_register_req_t *req = arch_get_irn_register_req_out(node, i);
		if (!arch_register_req_is(req, should_be_same))
			continue;

		int same_pos = get_first_same(req);

		/* get in and out register */
		const arch_register_t *out_reg = arch_get_irn_register_out(node, i);
		ir_node               *in_node = get_irn_n(node, same_pos);
		const arch_register_t *in_reg  = arch_get_irn_register(in_node);

		/* requirement already fulfilled? */
		if (in_reg == out_reg)
			continue;
		assert(in_reg->reg_class == out_reg->reg_class);

		/* check if any other input operands uses the out register */
		ir_node *uses_out_reg     = NULL;
		int      uses_out_reg_pos = -1;
		foreach_irn_in(node, i2, in) {
			if (!mode_is_data(get_irn_mode(in)))
				continue;

			const arch_register_t *other_in_reg = arch_get_irn_register(in);
			if (other_in_reg != out_reg)
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
			ir_node *copy = be_new_Copy(block, in_node);
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

		panic("Unresolved should_be_same constraint");
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
	/* check only ia32 nodes with source address mode */
	if (!is_ia32_irn(irn) || get_ia32_op_type(irn) != ia32_AddrModeS)
		return;
	/* only need to fix binary operations */
	if (get_ia32_am_support(irn) != ia32_am_binary)
		return;

	be_foreach_out(irn, i) {
		const arch_register_req_t *req = arch_get_irn_register_req_out(irn, i);
		if (!arch_register_req_is(req, should_be_same))
			continue;

		/* get in and out register */
		const arch_register_t *out_reg   = arch_get_irn_register_out(irn, i);
		int                    same_pos  = get_first_same(req);
		ir_node               *same_node = get_irn_n(irn, same_pos);
		const arch_register_t *same_reg  = arch_get_irn_register(same_node);

		/* should_be same constraint is fullfilled, nothing to do */
		if (out_reg == same_reg)
			continue;

		/* we only need to do something if the out reg is the same as base
			 or index register */
		if (out_reg != arch_get_irn_register(get_irn_n(irn, n_ia32_base)) &&
				out_reg != arch_get_irn_register(get_irn_n(irn, n_ia32_index)))
			continue;

		ir_node *load_res = ia32_turn_back_am(irn);
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
	(void) env;

	/* first: turn back AM source if necessary */
	sched_foreach_safe(block, irn) {
		fix_am_source(irn);
	}

	sched_foreach_safe(block, irn) {
		/* check if there is a sub which need to be transformed */
		if (is_ia32_Sub(irn) || is_ia32_Sbb(irn) || is_ia32_xSub(irn)) {
			ia32_transform_sub_to_neg_add(irn);
		}
	}

	/* second: insert copies and finish irg */
	sched_foreach_safe(block, irn) {
		if (is_ia32_irn(irn)) {
			/* some nodes are just a bit less efficient, but need no fixing if the
			 * should be same requirement is not fulfilled */
			if (need_constraint_copy(irn))
				assure_should_be_same_requirements(irn);
		}
	}
}

/**
 * Block walker: pushes all blocks on a wait queue
 */
static void ia32_push_on_queue_walker(ir_node *block, void *env)
{
	waitq *wq = (waitq*)env;
	waitq_put(wq, block);
}


/**
 * Add Copy nodes for not fulfilled should_be_equal constraints
 */
void ia32_finish_irg(ir_graph *irg)
{
	waitq *wq = new_waitq();

	/* Push the blocks on the waitq because ia32_finish_irg_walker starts more
	 * walks ... */
	irg_block_walk_graph(irg, NULL, ia32_push_on_queue_walker, wq);

	while (! waitq_empty(wq)) {
		ir_node *block = (ir_node*)waitq_get(wq);
		ia32_finish_irg_walker(block, NULL);
	}
	del_waitq(wq);
}

void ia32_init_finish(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.finish");
}
