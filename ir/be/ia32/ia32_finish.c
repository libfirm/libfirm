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

	/* Return if not a Sub or xSub */
	if (!is_ia32_Sub(irn) && !is_ia32_xSub(irn))
		return;
	/* fix_am will solve this for AddressMode variants */
	if(get_ia32_op_type(irn) != ia32_Normal)
		return;

	noreg   = ia32_new_NoReg_gp(cg);
	noreg_fp = ia32_new_NoReg_fp(cg);
	nomem   = new_rd_NoMem(cg->irg);
	in1     = get_irn_n(irn, 2);
	in2     = get_irn_n(irn, 3);
	in1_reg = arch_get_irn_register(cg->arch_env, in1);
	in2_reg = arch_get_irn_register(cg->arch_env, in2);
	out_reg = get_ia32_out_reg(irn, 0);

	assert(get_irn_mode(irn) != mode_T);

	irg     = cg->irg;
	block   = get_nodes_block(irn);

	/* in case of sub and OUT == SRC2 we can transform the sequence into neg src2 -- add */
	if (out_reg != in2_reg)
		return;

	/* generate the neg src2 */
	if(mode_is_float(mode)) {
		int size;
		ir_entity *entity;
		ir_mode *op_mode = get_ia32_ls_mode(irn);

		res = new_rd_ia32_xXor(dbg, irg, block, noreg, noreg, in2, noreg_fp, nomem);
		size = get_mode_size_bits(op_mode);
		entity = ia32_gen_fp_known_const(size == 32 ? ia32_SSIGN : ia32_DSIGN);
		set_ia32_am_sc(res, entity);
		set_ia32_op_type(res, ia32_AddrModeS);
		set_ia32_ls_mode(res, op_mode);
	} else {
		res = new_rd_ia32_Neg(dbg, irg, block, in2);
	}
	arch_set_irn_register(cg->arch_env, res, in2_reg);

	/* add to schedule */
	sched_add_before(irn, res);

	/* generate the add */
	if (mode_is_float(mode)) {
		res = new_rd_ia32_xAdd(dbg, irg, block, noreg, noreg, res, in1, nomem);
		set_ia32_am_support(res, ia32_am_Source, ia32_am_binary);
		set_ia32_ls_mode(res, get_ia32_ls_mode(irn));
	} else {
		res = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, res, in1, nomem);
		set_ia32_am_support(res, ia32_am_Full, ia32_am_binary);
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

static INLINE int is_noreg(ia32_code_gen_t *cg, const ir_node *node)
{
	return node == cg->noreg_gp;
}

static ir_node *create_immediate_from_int(ia32_code_gen_t *cg, int val)
{
	ir_graph *irg         = current_ir_graph;
	ir_node  *start_block = get_irg_start_block(irg);
	ir_node  *immediate   = new_rd_ia32_Immediate(NULL, irg, start_block, NULL,
	                                              0, val);
	arch_set_irn_register(cg->arch_env, immediate, &ia32_gp_regs[REG_GP_NOREG]);

	return immediate;
}

static ir_node *create_immediate_from_am(ia32_code_gen_t *cg,
                                         const ir_node *node)
{
	ir_graph  *irg     = get_irn_irg(node);
	ir_node   *block   = get_nodes_block(node);
	int        offset  = get_ia32_am_offs_int(node);
	int        sc_sign = is_ia32_am_sc_sign(node);
	ir_entity *entity  = get_ia32_am_sc(node);
	ir_node   *res;

	res = new_rd_ia32_Immediate(NULL, irg, block, entity, sc_sign, offset);
	arch_set_irn_register(cg->arch_env, res, &ia32_gp_regs[REG_GP_NOREG]);
	return res;
}

static int is_am_one(const ir_node *node)
{
	int        offset  = get_ia32_am_offs_int(node);
	ir_entity *entity  = get_ia32_am_sc(node);

	return offset == 1 && entity == NULL;
}

static int is_am_minus_one(const ir_node *node)
{
	int        offset  = get_ia32_am_offs_int(node);
	ir_entity *entity  = get_ia32_am_sc(node);

	return offset == -1 && entity == NULL;
}

/**
 * Transforms a LEA into an Add or SHL if possible.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
static void ia32_transform_lea_to_add_or_shl(ir_node *node, ia32_code_gen_t *cg)
{
	const arch_env_t      *arch_env = cg->arch_env;
	ir_graph              *irg      = current_ir_graph;
	ir_node               *base;
	ir_node               *index;
	const arch_register_t *base_reg;
	const arch_register_t *index_reg;
	const arch_register_t *out_reg;
	int                    scale;
	int                    has_immediates;
	ir_node               *op1;
	ir_node               *op2;
	dbg_info              *dbgi;
	ir_node               *block;
	ir_node               *res;
	ir_node               *noreg;
	ir_node               *nomem;

	if(!is_ia32_Lea(node))
		return;

	base  = get_irn_n(node, n_ia32_Lea_base);
	index = get_irn_n(node, n_ia32_Lea_index);

	if(is_noreg(cg, base)) {
		base     = NULL;
		base_reg = NULL;
	} else {
		base_reg = arch_get_irn_register(arch_env, base);
	}
	if(is_noreg(cg, index)) {
		index     = NULL;
		index_reg = NULL;
	} else {
		index_reg = arch_get_irn_register(arch_env, index);
	}

	if(base == NULL && index == NULL) {
		/* we shouldn't construct these in the first place... */
#ifdef DEBUG_libfirm
		ir_fprintf(stderr, "Optimisation warning: found immediate only lea\n");
#endif
		return;
	}

	out_reg = arch_get_irn_register(arch_env, node);
	scale   = get_ia32_am_scale(node);
	assert(!is_ia32_need_stackent(node) || get_ia32_frame_ent(node) != NULL);
	/* check if we have immediates values (frame entities should already be
	 * expressed in the offsets) */
	if(get_ia32_am_offs_int(node) != 0 || get_ia32_am_sc(node) != NULL) {
		has_immediates = 1;
	} else {
		has_immediates = 0;
	}

	/* we can transform leas where the out register is the same as either the
	 * base or index register back to an Add or Shl */
	if(out_reg == base_reg) {
		if(index == NULL) {
#ifdef DEBUG_libfirm
			if(!has_immediates) {
				ir_fprintf(stderr, "Optimisation warning: found lea which is "
				           "just a copy\n");
			}
#endif
			op1 = base;
			goto make_add_immediate;
		}
		if(scale == 0 && !has_immediates) {
			op1 = base;
			op2 = index;
			goto make_add;
		}
		/* can't create an add */
		return;
	} else if(out_reg == index_reg) {
		if(base == NULL) {
			if(has_immediates && scale == 0) {
				op1 = index;
				goto make_add_immediate;
			} else if(!has_immediates && scale > 0) {
				op1 = index;
				op2 = create_immediate_from_int(cg, scale);
				goto make_shl;
			} else if(!has_immediates) {
#ifdef DEBUG_libfirm
				ir_fprintf(stderr, "Optimisation warning: found lea which is "
				           "just a copy\n");
#endif
			}
		} else if(scale == 0 && !has_immediates) {
			op1 = index;
			op2 = base;
			goto make_add;
		}
		/* can't create an add */
		return;
	} else {
		/* can't create an add */
		return;
	}

make_add_immediate:
	if(cg->isa->opt & IA32_OPT_INCDEC) {
		if(is_am_one(node)) {
			dbgi  = get_irn_dbg_info(node);
			block = get_nodes_block(node);
			res   = new_rd_ia32_Inc(dbgi, irg, block, op1);
			arch_set_irn_register(arch_env, res, out_reg);
			goto exchange;
		}
		if(is_am_minus_one(node)) {
			dbgi  = get_irn_dbg_info(node);
			block = get_nodes_block(node);
			res   = new_rd_ia32_Dec(dbgi, irg, block, op1);
			arch_set_irn_register(arch_env, res, out_reg);
			goto exchange;
		}
	}
	op2 = create_immediate_from_am(cg, node);

make_add:
	dbgi  = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	noreg = ia32_new_NoReg_gp(cg);
	nomem = new_NoMem();
	res   = new_rd_ia32_Add(dbgi, irg, block, noreg, noreg, op1, op2, nomem);
	arch_set_irn_register(arch_env, res, out_reg);
	set_ia32_commutative(res);
	goto exchange;

make_shl:
	dbgi  = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	noreg = ia32_new_NoReg_gp(cg);
	nomem = new_NoMem();
	res   = new_rd_ia32_Shl(dbgi, irg, block, op1, op2);
	arch_set_irn_register(arch_env, res, out_reg);
	goto exchange;

exchange:
	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(cg, node));

	/* add new ADD/SHL to schedule */
	sched_add_before(node, res);

	DBG_OPT_LEA2ADD(node, res);

	/* remove the old LEA */
	sched_remove(node);

	/* exchange the Add and the LEA */
	exchange(node, res);
}

static INLINE int need_constraint_copy(ir_node *irn) {
	return	! is_ia32_Lea(irn)      &&
		! is_ia32_Conv_I2I(irn)     &&
		! is_ia32_Conv_I2I8Bit(irn) &&
		! is_ia32_TestCMov(irn)     &&
		! is_ia32_CmpCMov(irn);
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
	const arch_env_t           *arch_env = cg->arch_env;
	const arch_register_req_t **reqs;
	const arch_register_t      *out_reg, *in_reg;
	int                         n_res, i;
	ir_node                    *in_node, *block;
	ia32_op_type_t              op_tp;

	if(!is_ia32_irn(node))
		return;

	/* some nodes are just a bit less efficient, but need no fixing if the
	 * should be same requirement is not fulfilled */
	if(!need_constraint_copy(node))
		return;

	op_tp = get_ia32_op_type(node);
	reqs  = get_ia32_out_req_all(node);
	n_res = get_ia32_n_res(node);
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
		const arch_register_class_t *class;
		int                         uses_out_reg_pos;

		if (!arch_register_req_is(req, should_be_same))
			continue;

		same_pos = req->other_same;

		/* get in and out register */
		out_reg  = get_ia32_out_reg(node, i);
		in_node  = get_irn_n(node, same_pos);
		in_reg   = arch_get_irn_register(arch_env, in_node);

		/* requirement already fulfilled? */
		if (in_reg == out_reg)
			continue;
		/* unknowns can be changed to any register we want on emitting */
		if (is_unknown_reg(in_reg))
			continue;
		class = arch_register_get_class(in_reg);
		assert(class == arch_register_get_class(out_reg));

		/* check if any other input operands uses the out register */
		arity = get_irn_arity(node);
		uses_out_reg     = NULL;
		uses_out_reg_pos = -1;
		for(i2 = 0; i2 < arity; ++i2) {
			ir_node               *in     = get_irn_n(node, i2);
			const arch_register_t *in_reg = arch_get_irn_register(arch_env, in);

			if(in_reg != out_reg)
				continue;

			if(uses_out_reg != NULL && in != uses_out_reg) {
				panic("invalid register allocation");
			}
			uses_out_reg = in;
			if(uses_out_reg_pos >= 0)
				uses_out_reg_pos = -1; /* multiple inputs... */
			else
				uses_out_reg_pos = i2;
		}

		/* no-one else is using the out reg, we can simply copy it
		 * (the register can't be live since the operation will override it
		 *  anyway) */
		if(uses_out_reg == NULL) {
			ir_node *copy = be_new_Copy(class, irg, block, in_node);
			DBG_OPT_2ADDRCPY(copy);

			/* destination is the out register */
			arch_set_irn_register(arch_env, copy, out_reg);

			/* insert copy before the node into the schedule */
			sched_add_before(node, copy);

			/* set copy as in */
			set_irn_n(node, same_pos, copy);

			DBG((dbg, LEVEL_1, "created copy %+F for should be same argument "
			     "at input %d of %+F\n", copy, same_pos, node));
			continue;
		}

		/* for commutative nodes we can simply swap the left/right */
		if(is_ia32_commutative(node) && uses_out_reg_pos == 3) {
			ia32_swap_left_right(node);
			DBG((dbg, LEVEL_1, "swapped left/right input of %+F to resolve "
	   		     "should be same constraint\n", node));
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
		perm  = be_new_Perm(class, irg, block, 2, in);

		perm_proj0 = new_r_Proj(irg, block, perm, get_irn_mode(in[0]), 0);
		perm_proj1 = new_r_Proj(irg, block, perm, get_irn_mode(in[1]), 1);

		arch_set_irn_register(arch_env, perm_proj0, out_reg);
		arch_set_irn_register(arch_env, perm_proj1, in_reg);

		sched_add_before(node, perm);

		DBG((dbg, LEVEL_1, "created perm %+F for should be same argument "
		     "at input %d of %+F (need permutate with %+F)\n", perm, same_pos,
		     node, uses_out_reg));

		/* use the perm results */
		for(i2 = 0; i2 < arity; ++i2) {
			ir_node *in = get_irn_n(node, i2);

			if(in == in_node) {
				set_irn_n(node, i2, perm_proj0);
			} else if(in == uses_out_reg) {
				set_irn_n(node, i2, perm_proj1);
			}
		}
	}

	/* check xCmp: try to avoid unordered cmp */
	if ((is_ia32_xCmp(node) || is_ia32_xCmpCMov(node) || is_ia32_xCmpSet(node)) &&
		op_tp == ia32_Normal)
	{
		long pnc = get_ia32_pncode(node);

		if (pnc & pn_Cmp_Uo) {
			ir_node *tmp;
			int idx1 = 2, idx2 = 3;

			if (is_ia32_xCmpCMov(node)) {
				idx1 = 0;
				idx2 = 1;
			}

			/** Matze: TODO this looks wrong, I assume we should exchange
			 * the proj numbers and not the inputs... */

			tmp = get_irn_n(node, idx1);
			set_irn_n(node, idx1, get_irn_n(node, idx2));
			set_irn_n(node, idx2, tmp);

			set_ia32_pncode(node, get_negated_pnc(pnc, mode_E));
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
static void fix_am_source(ir_node *irn, void *env) {
	ia32_code_gen_t            *cg = env;
	const arch_env_t           *arch_env = cg->arch_env;
	ir_node                    *base;
	ir_node                    *index;
	ir_node                    *noreg;
	const arch_register_t      *reg_base;
	const arch_register_t      *reg_index;
	const arch_register_req_t **reqs;
	int                         n_res, i;

	/* check only ia32 nodes with source address mode */
	if (! is_ia32_irn(irn) || get_ia32_op_type(irn) != ia32_AddrModeS)
		return;
	/* only need to fix binary operations */
	if (get_ia32_am_arity(irn) != ia32_am_binary)
		return;

	base  = get_irn_n(irn, 0);
	index = get_irn_n(irn, 1);

	reg_base  = arch_get_irn_register(arch_env, base);
	reg_index = arch_get_irn_register(arch_env, index);
	reqs      = get_ia32_out_req_all(irn);

	noreg = ia32_new_NoReg_gp(cg);

	n_res = get_ia32_n_res(irn);

	for (i = 0; i < n_res; i++) {
		if (arch_register_req_is(reqs[i], should_be_same)) {
			/* get in and out register */
			const arch_register_t *out_reg   = get_ia32_out_reg(irn, i);
			int                    same_pos  = reqs[i]->other_same;
			ir_node               *same_node = get_irn_n(irn, same_pos);
			const arch_register_t *same_reg
				= arch_get_irn_register(arch_env, same_node);
			const arch_register_class_t *same_cls;
			ir_graph              *irg   = cg->irg;
			dbg_info              *dbgi  = get_irn_dbg_info(irn);
			ir_node               *block = get_nodes_block(irn);
			ir_mode               *proj_mode;
			ir_node               *load;
			ir_node               *load_res;
			int                    pnres;

			/* should_be same constraint is fullfilled, nothing to do */
			if(out_reg == same_reg)
				continue;

			/* we only need to do something if the out reg is the same as base
			   or index register */
			if (out_reg != reg_base && out_reg != reg_index)
				continue;

			/* turn back address mode */
			same_cls = arch_register_get_class(same_reg);
			if (same_cls == &ia32_reg_classes[CLASS_ia32_gp]) {
				load  = new_rd_ia32_Load(dbgi, irg, block, base, index,
				                         get_irn_n(irn, 4));
				assert(get_irn_mode(get_irn_n(irn,4)) == mode_M);
				pnres     = pn_ia32_Load_res;
				proj_mode = mode_Iu;
			} else if (same_cls == &ia32_reg_classes[CLASS_ia32_xmm]) {
				load  = new_rd_ia32_xLoad(dbgi, irg, block, base, index,
				                          get_irn_n(irn, 4),
				                          get_ia32_ls_mode(irn));
				assert(get_irn_mode(get_irn_n(irn,4)) == mode_M);
				pnres     = pn_ia32_xLoad_res;
				proj_mode = mode_E;
			} else {
				panic("cannot turn back address mode for this register class");
			}

			/* copy address mode information to load */
			set_ia32_ls_mode(load, get_ia32_ls_mode(irn));
			set_ia32_op_type(load, ia32_AddrModeS);
			set_ia32_am_scale(load, get_ia32_am_scale(irn));
			set_ia32_am_sc(load, get_ia32_am_sc(irn));
			if(is_ia32_am_sc_sign(irn))
				set_ia32_am_sc_sign(load);
			add_ia32_am_offs_int(load, get_ia32_am_offs_int(irn));
			set_ia32_frame_ent(load, get_ia32_frame_ent(irn));
			if (is_ia32_use_frame(irn))
				set_ia32_use_frame(load);

			/* insert the load into schedule */
			sched_add_before(irn, load);

			DBG((dbg, LEVEL_3, "irg %+F: build back AM source for node %+F, inserted load %+F\n", cg->irg, irn, load));

			load_res = new_r_Proj(cg->irg, block, load, proj_mode, pnres);
			arch_set_irn_register(cg->arch_env, load_res, out_reg);

			/* set the new input operand */
			set_irn_n(irn, 3, load_res);
			if(get_irn_mode(irn) == mode_T) {
				const ir_edge_t *edge, *next;
				foreach_out_edge_safe(irn, edge, next) {
					ir_node *node = get_edge_src_irn(edge);
					int      pn   = get_Proj_proj(node);
					if(pn == 0) {
						exchange(node, irn);
					} else {
						assert(pn == 1);
						set_Proj_pred(node, load);
					}
				}
				set_irn_mode(irn, mode_Iu);
			}

			/* this is a normal node now */
			set_irn_n(irn, 0, noreg);
			set_irn_n(irn, 1, noreg);
			set_ia32_op_type(irn, ia32_Normal);
			break;
		}
	}
}

/**
 * Block walker: finishes a block
 */
static void ia32_finish_irg_walker(ir_node *block, void *env) {
	ia32_code_gen_t *cg = env;
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
		assure_should_be_same_requirements(cg, irn);
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
