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
 * @brief   The codegenerator (transform FIRM into arm FIRM)
 * @author  Oliver Richter, Tobias Gneist, Michael Beck
 * @version $Id$
 */
#include "config.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "irvrfy.h"
#include "ircons.h"
#include "irprintf.h"
#include "dbginfo.h"
#include "iropt_t.h"
#include "debug.h"
#include "error.h"

#include "../benode.h"
#include "../beirg.h"
#include "../beutil.h"
#include "../betranshlp.h"
#include "bearch_arm_t.h"

#include "arm_nodes_attr.h"
#include "arm_transform.h"
#include "arm_optimize.h"
#include "arm_new_nodes.h"
#include "arm_map_regs.h"

#include "gen_arm_regalloc_if.h"

#include <limits.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/** hold the current code generator during transformation */
static arm_code_gen_t *env_cg;

static inline int mode_needs_gp_reg(ir_mode *mode)
{
	return mode_is_int(mode) || mode_is_reference(mode);
}

/**
 * Creates a possible DAG for an constant.
 */
static ir_node *create_const_graph_value(dbg_info *dbgi, ir_node *block,
                                         unsigned int value)
{
	ir_node *result;
	arm_vals v, vn;
	int cnt;

	arm_gen_vals_from_word(value, &v);
	arm_gen_vals_from_word(~value, &vn);

	if (vn.ops < v.ops) {
		/* remove bits */
		result = new_bd_arm_Mvn_imm(dbgi, block, vn.values[0], vn.rors[0]);
		be_dep_on_frame(result);

		for (cnt = 1; cnt < vn.ops; ++cnt) {
			result = new_bd_arm_Bic_imm(dbgi, block, result,
			                            vn.values[cnt], vn.rors[cnt]);
		}
	} else {
		/* add bits */
		result = new_bd_arm_Mov_imm(dbgi, block, v.values[0], v.rors[0]);
		be_dep_on_frame(result);

		for (cnt = 1; cnt < v.ops; ++cnt) {
			result = new_bd_arm_Or_imm(dbgi, block, result,
			                           v.values[cnt], v.rors[cnt]);
		}
	}
	return result;
}

/**
 * Create a DAG constructing a given Const.
 *
 * @param irn  a Firm const
 */
static ir_node *create_const_graph(ir_node *irn, ir_node *block)
{
	tarval  *tv = get_Const_tarval(irn);
	ir_mode *mode = get_tarval_mode(tv);
	unsigned value;

	if (mode_is_reference(mode)) {
		/* ARM is 32bit, so we can safely convert a reference tarval into Iu */
		assert(get_mode_size_bits(mode) == get_mode_size_bits(mode_Iu));
		tv = tarval_convert_to(tv, mode_Iu);
	}
	value = get_tarval_long(tv);
	return create_const_graph_value(get_irn_dbg_info(irn), block, value);
}

/**
 * Create an And that will zero out upper bits.
 *
 * @param dbgi     debug info
 * @param block    the basic block
 * @param op       the original node
 * param src_bits  number of lower bits that will remain
 */
static ir_node *gen_zero_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                                   int src_bits)
{
	if (src_bits == 8) {
		return new_bd_arm_And_imm(dbgi, block, op, 0xFF, 0);
	} else if (src_bits == 16) {
		ir_node *lshift = new_bd_arm_Mov_reg_shift_imm(dbgi, block, op, ARM_SHF_LSL_IMM, 16);
		ir_node *rshift = new_bd_arm_Mov_reg_shift_imm(dbgi, block, lshift, ARM_SHF_LSR_IMM, 16);
		return rshift;
	} else {
		panic("zero extension only supported for 8 and 16 bits");
	}
}

/**
 * Generate code for a sign extension.
 */
static ir_node *gen_sign_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                                   int src_bits)
{
	int shift_width = 32 - src_bits;
	ir_node *lshift_node = new_bd_arm_Mov_reg_shift_imm(dbgi, block, op, ARM_SHF_LSL_IMM, shift_width);
	ir_node *rshift_node = new_bd_arm_Mov_reg_shift_imm(dbgi, block, lshift_node, ARM_SHF_ASR_IMM, shift_width);
	return rshift_node;
}

static ir_node *gen_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                              ir_mode *orig_mode)
{
	int bits = get_mode_size_bits(orig_mode);
	if (bits == 32)
		return op;

	if (mode_is_signed(orig_mode)) {
		return gen_sign_extension(dbgi, block, op, bits);
	} else {
		return gen_zero_extension(dbgi, block, op, bits);
	}
}

/**
 * returns true if it is assured, that the upper bits of a node are "clean"
 * which means for a 16 or 8 bit value, that the upper bits in the register
 * are 0 for unsigned and a copy of the last significant bit for signed
 * numbers.
 */
static bool upper_bits_clean(ir_node *transformed_node, ir_mode *mode)
{
	(void) transformed_node;
	(void) mode;
	/* TODO */
	return false;
}

/**
 * Transforms a Conv node.
 *
 * @return The created ia32 Conv node
 */
static ir_node *gen_Conv(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op       = get_Conv_op(node);
	ir_node  *new_op   = be_transform_node(op);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);
	dbg_info *dbg      = get_irn_dbg_info(node);

	if (src_mode == dst_mode)
		return new_op;

	if (mode_is_float(src_mode) || mode_is_float(dst_mode)) {
		env_cg->have_fp_insn = 1;

		if (USE_FPA(env_cg->isa)) {
			if (mode_is_float(src_mode)) {
				if (mode_is_float(dst_mode)) {
					/* from float to float */
					return new_bd_arm_fpaMvf(dbg, block, new_op, dst_mode);
				}
				else {
					/* from float to int */
					return new_bd_arm_fpaFix(dbg, block, new_op, dst_mode);
				}
			}
			else {
				/* from int to float */
				return new_bd_arm_fpaFlt(dbg, block, new_op, dst_mode);
			}
		} else if (USE_VFP(env_cg->isa)) {
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	} else { /* complete in gp registers */
		int src_bits = get_mode_size_bits(src_mode);
		int dst_bits = get_mode_size_bits(dst_mode);
		int min_bits;
		ir_mode *min_mode;

		if (src_bits == dst_bits) {
			/* kill unneccessary conv */
			return new_op;
		}

		if (src_bits < dst_bits) {
			min_bits = src_bits;
			min_mode = src_mode;
		} else {
			min_bits = dst_bits;
			min_mode = dst_mode;
		}

		if (upper_bits_clean(new_op, min_mode)) {
			return new_op;
		}

		if (mode_is_signed(min_mode)) {
			return gen_sign_extension(dbg, block, new_op, min_bits);
		} else {
			return gen_zero_extension(dbg, block, new_op, min_bits);
		}
	}
}

typedef struct {
	unsigned char  imm_8;
	unsigned char  rot;
} arm_immediate_t;

static bool try_encode_as_immediate(const ir_node *node, arm_immediate_t *res)
{
	unsigned val, low_pos, high_pos;

	if (!is_Const(node))
		return false;

	val = get_tarval_long(get_Const_tarval(node));

	if (val == 0) {
		res->imm_8 = 0;
		res->rot   = 0;
		return true;
	}
	if (val <= 0xff) {
		res->imm_8 = val;
		res->rot   = 0;
		return true;
	}
	/* arm allows to use to rotate an 8bit immediate value by a multiple of 2
	   (= 0, 2, 4, 6, ...).
	   So we determine the smallest even position with a bit set
	   and the highest even position with no bit set anymore.
	   If the difference between these 2 is <= 8, then we can encode the value
	   as immediate.
	 */
	low_pos  = ntz(val) & ~1u;
	high_pos = (32-nlz(val)+1) & ~1u;

	if (high_pos - low_pos <= 8) {
		res->imm_8 = val >> low_pos;
		res->rot   = 32 - low_pos;
		return true;
	}

	if (high_pos > 24) {
		res->rot = 34 - high_pos;
		val      = val >> (32-res->rot) | val << (res->rot);
		if (val <= 0xff) {
			res->imm_8 = val;
			return true;
		}
	}

	return false;
}

static bool is_downconv(const ir_node *node)
{
	ir_mode *src_mode;
	ir_mode *dest_mode;

	if (!is_Conv(node))
		return false;

	/* we only want to skip the conv when we're the only user
	 * (not optimal but for now...)
	 */
	if (get_irn_n_edges(node) > 1)
		return false;

	src_mode  = get_irn_mode(get_Conv_op(node));
	dest_mode = get_irn_mode(node);
	return
		mode_needs_gp_reg(src_mode)  &&
		mode_needs_gp_reg(dest_mode) &&
		get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode);
}

static ir_node *arm_skip_downconv(ir_node *node)
{
	while (is_downconv(node))
		node = get_Conv_op(node);
	return node;
}

typedef enum {
	MATCH_NONE         = 0,
	MATCH_COMMUTATIVE  = 1 << 0,
	MATCH_SIZE_NEUTRAL = 1 << 1,
} match_flags_t;

typedef ir_node* (*new_binop_reg_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_node *op2);
typedef ir_node* (*new_binop_imm_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, unsigned char imm8, unsigned char imm_rot);

static ir_node *gen_int_binop(ir_node *node, match_flags_t flags,
		new_binop_reg_func new_reg, new_binop_imm_func new_imm)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_binop_left(node);
	ir_node  *new_op1;
	ir_node  *op2     = get_binop_right(node);
	ir_node  *new_op2;
	dbg_info *dbgi    = get_irn_dbg_info(node);
	arm_immediate_t imm;

	if (flags & MATCH_SIZE_NEUTRAL) {
		op1 = arm_skip_downconv(op1);
		op2 = arm_skip_downconv(op2);
	} else {
		assert(get_mode_size_bits(get_irn_mode(node)) == 32);
	}

	if (try_encode_as_immediate(op2, &imm)) {
		ir_node *new_op1 = be_transform_node(op1);
		return new_imm(dbgi, block, new_op1, imm.imm_8, imm.rot);
	}
	new_op2 = be_transform_node(op2);
    if ((flags & MATCH_COMMUTATIVE) && try_encode_as_immediate(op1, &imm)) {
		return new_imm(dbgi, block, new_op2, imm.imm_8, imm.rot);
	}
	new_op1 = be_transform_node(op1);

	return new_reg(dbgi, block, new_op1, new_op2);
}

/**
 * Creates an ARM Add.
 *
 * @return the created arm Add node
 */
static ir_node *gen_Add(ir_node *node)
{
	ir_mode  *mode    = get_irn_mode(node);

	if (mode_is_float(mode)) {
		ir_node  *block   = be_transform_node(get_nodes_block(node));
		ir_node  *op1     = get_Add_left(node);
		ir_node  *op2     = get_Add_right(node);
		dbg_info *dbgi    = get_irn_dbg_info(node);
		ir_node  *new_op1 = be_transform_node(op1);
		ir_node  *new_op2 = be_transform_node(op2);
		env_cg->have_fp_insn = 1;
		if (USE_FPA(env_cg->isa)) {
#if 0
			if (is_arm_fpaMvf_i(new_op1))
				return new_bd_arm_fpaAdf_i(dbgi, block, new_op2, mode, get_arm_imm_value(new_op1));
			if (is_arm_fpaMvf_i(new_op2))
				return new_bd_arm_fpaAdf_i(dbgi, block, new_op1, mode, get_arm_imm_value(new_op2));
#endif
			return new_bd_arm_fpaAdf(dbgi, block, new_op1, new_op2, mode);
		} else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		}
		else {
			panic("Softfloat not supported yet");
		}
	} else {
#if 0
		/* check for MLA */
		if (is_arm_Mul(new_op1) && get_irn_n_edges(op1) == 1) {
			new_op3 = new_op2;
			new_op2 = get_irn_n(new_op1, 1);
			new_op1 = get_irn_n(new_op1, 0);

			return new_bd_arm_Mla(dbgi, block, new_op1, new_op2, new_op3);
		}
		if (is_arm_Mul(new_op2) && get_irn_n_edges(op2) == 1) {
			new_op3 = new_op1;
			new_op1 = get_irn_n(new_op2, 0);
			new_op2 = get_irn_n(new_op2, 1);

			return new_bd_arm_Mla(dbgi, block, new_op1, new_op2, new_op3);
		}
#endif

		return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL,
				new_bd_arm_Add_reg, new_bd_arm_Add_imm);
	}
}

/**
 * Creates an ARM Mul.
 *
 * @return the created arm Mul node
 */
static ir_node *gen_Mul(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_Mul_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_Mul_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	ir_mode  *mode    = get_irn_mode(node);
	dbg_info *dbg     = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		env_cg->have_fp_insn = 1;
		if (USE_FPA(env_cg->isa)) {
#if 0
			if (is_arm_Mov_i(new_op1))
				return new_bd_arm_fpaMuf_i(dbg, block, new_op2, mode, get_arm_imm_value(new_op1));
			if (is_arm_Mov_i(new_op2))
				return new_bd_arm_fpaMuf_i(dbg, block, new_op1, mode, get_arm_imm_value(new_op2));
#endif
			return new_bd_arm_fpaMuf(dbg, block, new_op1, new_op2, mode);
		}
		else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		}
		else {
			panic("Softfloat not supported yet");
		}
	}
	assert(mode_is_data(mode));
	return new_bd_arm_Mul(dbg, block, new_op1, new_op2);
}

/**
 * Creates an ARM floating point Div.
 *
 * @param env   The transformation environment
 * @return the created arm fDiv node
 */
static ir_node *gen_Quot(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_Quot_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_Quot_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	ir_mode  *mode    = get_irn_mode(node);
	dbg_info *dbg     = get_irn_dbg_info(node);

	assert(mode != mode_E && "IEEE Extended FP not supported");

	env_cg->have_fp_insn = 1;
	if (USE_FPA(env_cg->isa)) {
#if 0
		if (is_arm_Mov_i(new_op1))
			return new_bd_arm_fpaRdf_i(dbg, block, new_op2, mode, get_arm_imm_value(new_op1));
		if (is_arm_Mov_i(new_op2))
			return new_bd_arm_fpaDvf_i(dbg, block, new_op1, mode, get_arm_imm_value(new_op2));
#endif
		return new_bd_arm_fpaDvf(dbg, block, new_op1, new_op2, mode);
	} else if (USE_VFP(env_cg->isa)) {
		assert(mode != mode_E && "IEEE Extended FP not supported");
		panic("VFP not supported yet");
	}
	else {
		panic("Softfloat not supported yet");
	}
}

/**
 * Creates an ARM And.
 *
 * @return the created arm And node
 */
static ir_node *gen_And(ir_node *node)
{
	return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL,
			new_bd_arm_And_reg, new_bd_arm_And_imm);
}

/**
 * Creates an ARM Orr.
 *
 * @param env   The transformation environment
 * @return the created arm Or node
 */
static ir_node *gen_Or(ir_node *node)
{
	return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL,
			new_bd_arm_Or_reg, new_bd_arm_Or_imm);
}

/**
 * Creates an ARM Eor.
 *
 * @return the created arm Eor node
 */
static ir_node *gen_Eor(ir_node *node)
{
	return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL,
			new_bd_arm_Eor_reg, new_bd_arm_Eor_imm);
}

/**
 * Creates an ARM Sub.
 *
 * @return the created arm Sub node
 */
static ir_node *gen_Sub(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_Sub_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_Sub_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	ir_mode  *mode    = get_irn_mode(node);
	dbg_info *dbgi    = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		env_cg->have_fp_insn = 1;
		if (USE_FPA(env_cg->isa)) {
#if 0
			if (is_arm_Mov_i(new_op1))
				return new_bd_arm_fpaRsf_i(dbgi, block, new_op2, mode, get_arm_imm_value(new_op1));
			if (is_arm_Mov_i(new_op2))
				return new_bd_arm_fpaSuf_i(dbgi, block, new_op1, mode, get_arm_imm_value(new_op2));
#endif
			return new_bd_arm_fpaSuf(dbgi, block, new_op1, new_op2, mode);
		} else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	} else {
		return gen_int_binop(node, MATCH_SIZE_NEUTRAL,
				new_bd_arm_Sub_reg, new_bd_arm_Sub_imm);
	}
}

static bool can_use_shift_constant(unsigned int val,
                                   arm_shift_modifier_t modifier)
{
	if (val <= 31)
		return true;
	if (val == 32 && modifier != ARM_SHF_LSL_REG && modifier != ARM_SHF_ROR_REG)
		return true;
	return false;
}

static ir_node *make_shift(ir_node *node, match_flags_t flags,
		arm_shift_modifier_t shift_modifier)
{
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *op1   = get_binop_left(node);
	ir_node  *op2   = get_binop_right(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *new_op1;
	ir_node  *new_op2;

	if (flags & MATCH_SIZE_NEUTRAL) {
		op1 = arm_skip_downconv(op1);
		op2 = arm_skip_downconv(op2);
	}

	new_op1 = be_transform_node(op1);
	if (is_Const(op2)) {
		tarval      *tv  = get_Const_tarval(op2);
		unsigned int val = get_tarval_long(tv);
		assert(tarval_is_long(tv));
		if (can_use_shift_constant(val, shift_modifier)) {
			switch (shift_modifier) {
			case ARM_SHF_LSL_REG: shift_modifier = ARM_SHF_LSL_IMM; break;
			case ARM_SHF_LSR_REG: shift_modifier = ARM_SHF_LSR_IMM; break;
			case ARM_SHF_ASR_REG: shift_modifier = ARM_SHF_ASR_IMM; break;
			case ARM_SHF_ROR_REG: shift_modifier = ARM_SHF_ROR_IMM; break;
			default: panic("unexpected shift modifier");
			}
			return new_bd_arm_Mov_reg_shift_imm(dbgi, block, new_op1,
			                                    shift_modifier, val);
		}
	}

	new_op2 = be_transform_node(op2);
	return new_bd_arm_Mov_reg_shift_reg(dbgi, block, new_op1, new_op2,
	                                    shift_modifier);
}

/**
 * Creates an ARM Shl.
 *
 * @return the created ARM Shl node
 */
static ir_node *gen_Shl(ir_node *node)
{
	return make_shift(node, MATCH_SIZE_NEUTRAL, ARM_SHF_LSL_REG);
}

/**
 * Creates an ARM Shr.
 *
 * @return the created ARM Shr node
 */
static ir_node *gen_Shr(ir_node *node)
{
	return make_shift(node, MATCH_NONE, ARM_SHF_LSR_REG);
}

/**
 * Creates an ARM Shrs.
 *
 * @return the created ARM Shrs node
 */
static ir_node *gen_Shrs(ir_node *node)
{
	return make_shift(node, MATCH_NONE, ARM_SHF_ASR_REG);
}

/**
 * Creates an ARM Ror.
 *
 * @return the created ARM Ror node
 */
static ir_node *gen_Ror(ir_node *node, ir_node *op1, ir_node *op2)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *new_op1 = be_transform_node(op1);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *new_op2 = be_transform_node(op2);

	return new_bd_arm_Mov_reg_shift_reg(dbgi, block, new_op1, new_op2,
	                                    ARM_SHF_ROR_REG);
}

/**
 * Creates an ARM Rol.
 *
 * @return the created ARM Rol node
 *
 * Note: there is no Rol on arm, we have to use Ror
 */
static ir_node *gen_Rol(ir_node *node, ir_node *op1, ir_node *op2)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *new_op1 = be_transform_node(op1);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *new_op2 = be_transform_node(op2);

	new_op2 = new_bd_arm_Rsb_imm(dbgi, block, new_op2, 32, 0);
	return new_bd_arm_Mov_reg_shift_reg(dbgi, block, new_op1, new_op2,
	                                    ARM_SHF_ROR_REG);
}

/**
 * Creates an ARM ROR from a Firm Rotl.
 *
 * @return the created ARM Ror node
 */
static ir_node *gen_Rotl(ir_node *node)
{
	ir_node *rotate = NULL;
	ir_node *op1    = get_Rotl_left(node);
	ir_node *op2    = get_Rotl_right(node);

	/* Firm has only RotL, so we are looking for a right (op2)
	   operand "-e+mode_size_bits" (it's an already modified "mode_size_bits-e",
	   that means we can create a RotR. */

	if (is_Add(op2)) {
		ir_node *right = get_Add_right(op2);
		if (is_Const(right)) {
			tarval  *tv   = get_Const_tarval(right);
			ir_mode *mode = get_irn_mode(node);
			long     bits = get_mode_size_bits(mode);
			ir_node *left = get_Add_left(op2);

			if (is_Minus(left) &&
			    tarval_is_long(tv)          &&
			    get_tarval_long(tv) == bits &&
			    bits                == 32)
				rotate = gen_Ror(node, op1, get_Minus_op(left));
		}
	} else if (is_Sub(op2)) {
		ir_node *left = get_Sub_left(op2);
		if (is_Const(left)) {
			tarval  *tv   = get_Const_tarval(left);
			ir_mode *mode = get_irn_mode(node);
			long     bits = get_mode_size_bits(mode);
			ir_node *right = get_Sub_right(op2);

			if (tarval_is_long(tv)          &&
			    get_tarval_long(tv) == bits &&
			    bits                == 32)
				rotate = gen_Ror(node, op1, right);
		}
	} else if (is_Const(op2)) {
		tarval  *tv   = get_Const_tarval(op2);
		ir_mode *mode = get_irn_mode(node);
		long     bits = get_mode_size_bits(mode);

		if (tarval_is_long(tv) && bits == 32) {
			ir_node  *block   = be_transform_node(get_nodes_block(node));
			ir_node  *new_op1 = be_transform_node(op1);
			dbg_info *dbgi    = get_irn_dbg_info(node);

			bits = (bits - get_tarval_long(tv)) & 31;
			rotate = new_bd_arm_Mov_reg_shift_imm(dbgi, block, new_op1, ARM_SHF_ROR_IMM, bits);
		}
	}

	if (rotate == NULL) {
		rotate = gen_Rol(node, op1, op2);
	}

	return rotate;
}

/**
 * Transforms a Not node.
 *
 * @return the created ARM Not node
 */
static ir_node *gen_Not(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op      = get_Not_op(node);
	ir_node  *new_op  = be_transform_node(op);
	dbg_info *dbgi    = get_irn_dbg_info(node);

	/* TODO: we could do alot more here with all the Mvn variations */

	return new_bd_arm_Mvn_reg(dbgi, block, new_op);
}

/**
 * Transforms an Abs node.
 *
 * @param env   The transformation environment
 * @return the created ARM Abs node
 */
static ir_node *gen_Abs(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op      = get_Abs_op(node);
	ir_node  *new_op  = be_transform_node(op);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_mode  *mode    = get_irn_mode(node);

	if (mode_is_float(mode)) {
		env_cg->have_fp_insn = 1;
		if (USE_FPA(env_cg->isa))
			return new_bd_arm_fpaAbs(dbgi, block, new_op, mode);
		else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		}
		else {
			panic("Softfloat not supported yet");
		}
	}
	assert(mode_is_data(mode));
	return new_bd_arm_Abs(dbgi, block, new_op);
}

/**
 * Transforms a Minus node.
 *
 * @return the created ARM Minus node
 */
static ir_node *gen_Minus(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op      = get_Minus_op(node);
	ir_node  *new_op  = be_transform_node(op);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_mode  *mode    = get_irn_mode(node);

	if (mode_is_float(mode)) {
		env_cg->have_fp_insn = 1;
		if (USE_FPA(env_cg->isa))
			return new_bd_arm_fpaMvf(dbgi, block, op, mode);
		else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		}
		else {
			panic("Softfloat not supported yet");
		}
	}
	assert(mode_is_data(mode));
	return new_bd_arm_Rsb_imm(dbgi, block, new_op, 0, 0);
}

/**
 * Transforms a Load.
 *
 * @return the created ARM Load node
 */
static ir_node *gen_Load(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Load_ptr(node);
	ir_node  *new_ptr  = be_transform_node(ptr);
	ir_node  *mem      = get_Load_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_mode  *mode     = get_Load_mode(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_load = NULL;

	if (mode_is_float(mode)) {
		env_cg->have_fp_insn = 1;
		if (USE_FPA(env_cg->isa))
			new_load = new_bd_arm_fpaLdf(dbgi, block, new_ptr, new_mem, mode);
		else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	} else {
		assert(mode_is_data(mode) && "unsupported mode for Load");

		new_load = new_bd_arm_Ldr(dbgi, block, new_ptr, new_mem, mode, NULL, 0, 0, false);
	}
	set_irn_pinned(new_load, get_irn_pinned(node));

	/* check for special case: the loaded value might not be used */
	if (be_get_Proj_for_pn(node, pn_Load_res) == NULL) {
		/* add a result proj and a Keep to produce a pseudo use */
		ir_node *proj = new_r_Proj(new_load, mode_Iu, pn_arm_Ldr_res);
		be_new_Keep(block, 1, &proj);
	}

	return new_load;
}

/**
 * Transforms a Store.
 *
 * @return the created ARM Store node
 */
static ir_node *gen_Store(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Store_ptr(node);
	ir_node  *new_ptr  = be_transform_node(ptr);
	ir_node  *mem      = get_Store_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_node  *val      = get_Store_value(node);
	ir_node  *new_val  = be_transform_node(val);
	ir_mode  *mode     = get_irn_mode(val);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node *new_store = NULL;

	if (mode_is_float(mode)) {
		env_cg->have_fp_insn = 1;
		if (USE_FPA(env_cg->isa))
			new_store = new_bd_arm_fpaStf(dbgi, block, new_ptr, new_val,
			                              new_mem, mode);
		else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	} else {
		assert(mode_is_data(mode) && "unsupported mode for Store");
		new_store = new_bd_arm_Str(dbgi, block, new_ptr, new_val, new_mem, mode,
		                           NULL, 0, 0, false);
	}
	set_irn_pinned(new_store, get_irn_pinned(node));
	return new_store;
}

static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	return new_bd_arm_Jmp(dbgi, new_block);
}

static ir_node *gen_be_Call(ir_node *node)
{
	ir_node *res = be_duplicate_node(node);
	arch_irn_add_flags(res, arch_irn_flags_modify_flags);

	return res;
}

static ir_node *gen_SwitchJmp(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *selector = get_Cond_selector(node);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node *new_op = be_transform_node(selector);
	ir_node *const_graph;
	ir_node *sub;

	ir_node *proj;
	const ir_edge_t *edge;
	int min = INT_MAX;
	int max = INT_MIN;
	int translation;
	int pn;
	int n_projs;

	foreach_out_edge(node, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		pn = get_Proj_proj(proj);

		min = pn<min ? pn : min;
		max = pn>max ? pn : max;
	}
	translation = min;
	n_projs = max - translation + 1;

	foreach_out_edge(node, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		pn = get_Proj_proj(proj) - translation;
		set_Proj_proj(proj, pn);
	}

	const_graph = create_const_graph_value(dbgi, block, translation);
	sub = new_bd_arm_Sub_reg(dbgi, block, new_op, const_graph);
	return new_bd_arm_SwitchJmp(dbgi, block, sub, n_projs, get_Cond_default_proj(node) - translation);
}

static ir_node *gen_Cmp(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op1      = get_Cmp_left(node);
	ir_node  *op2      = get_Cmp_right(node);
	ir_mode  *cmp_mode = get_irn_mode(op1);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *new_op1;
	ir_node  *new_op2;
	bool      is_unsigned;

	if (mode_is_float(cmp_mode)) {
		/* TODO: revivie this code */
		panic("FloatCmp NIY");
#if 0
		ir_node *new_op2  = be_transform_node(op2);
		/* floating point compare */
		pn_Cmp pnc = get_Proj_proj(selector);

		if (pnc & pn_Cmp_Uo) {
			/* check for unordered, need cmf */
			return new_bd_arm_fpaCmfBra(dbgi, block, new_op1, new_op2, pnc);
		}
		/* Hmm: use need cmfe */
		return new_bd_arm_fpaCmfeBra(dbgi, block, new_op1, new_op2, pnc);
#endif
	}

	assert(get_irn_mode(op2) == cmp_mode);
	is_unsigned = !mode_is_signed(cmp_mode);

	/* compare with 0 can be done with Tst */
	if (is_Const(op2) && tarval_is_null(get_Const_tarval(op2))) {
		new_op1 = be_transform_node(op1);
		new_op1 = gen_extension(dbgi, block, new_op1, cmp_mode);
		return new_bd_arm_Tst_reg(dbgi, block, new_op1, new_op1, false,
		                          is_unsigned);
	}
	if (is_Const(op1) && tarval_is_null(get_Const_tarval(op1))) {
		new_op2 = be_transform_node(op2);
		new_op2 = gen_extension(dbgi, block, new_op2, cmp_mode);
		return new_bd_arm_Tst_reg(dbgi, block, new_op2, new_op2, true,
		                          is_unsigned);
	}

	/* integer compare, TODO: use shifer_op in all its combinations */
	new_op1 = be_transform_node(op1);
	new_op1 = gen_extension(dbgi, block, new_op1, cmp_mode);
	new_op2 = be_transform_node(op2);
	new_op2 = gen_extension(dbgi, block, new_op2, cmp_mode);
	return new_bd_arm_Cmp_reg(dbgi, block, new_op1, new_op2, false,
	                          is_unsigned);
}

/**
 * Transforms a Cond.
 *
 * @return the created ARM Cond node
 */
static ir_node *gen_Cond(ir_node *node)
{
	ir_node  *selector = get_Cond_selector(node);
	ir_mode  *mode     = get_irn_mode(selector);
	ir_node  *block;
	ir_node  *flag_node;
	dbg_info *dbgi;

	if (mode != mode_b) {
		return gen_SwitchJmp(node);
	}
	assert(is_Proj(selector));

	block     = be_transform_node(get_nodes_block(node));
	dbgi      = get_irn_dbg_info(node);
	flag_node = be_transform_node(get_Proj_pred(selector));

	return new_bd_arm_B(dbgi, block, flag_node, get_Proj_proj(selector));
}

static tarval *fpa_imm[3][fpa_max];

#if 0
/**
 * Check, if a floating point tarval is an fpa immediate, i.e.
 * one of 0, 1, 2, 3, 4, 5, 10, or 0.5.
 */
static int is_fpa_immediate(tarval *tv)
{
	ir_mode *mode = get_tarval_mode(tv);
	int i, j, res = 1;

	switch (get_mode_size_bits(mode)) {
	case 32:
		i = 0;
		break;
	case 64:
		i = 1;
		break;
	default:
		i = 2;
	}

	if (tarval_is_negative(tv)) {
		tv = tarval_neg(tv);
		res = -1;
	}

	for (j = 0; j < fpa_max; ++j) {
		if (tv == fpa_imm[i][j])
			return res * j;
	}
	return fpa_max;
}
#endif

/**
 * Transforms a Const node.
 *
 * @return The transformed ARM node.
 */
static ir_node *gen_Const(ir_node *node)
{
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_mode *mode = get_irn_mode(node);
	dbg_info *dbg = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		env_cg->have_fp_insn = 1;
		if (USE_FPA(env_cg->isa)) {
			tarval *tv = get_Const_tarval(node);
#if 0
			int imm = is_fpa_immediate(tv);

			if (imm != fpa_max) {
				if (imm > 0)
					node = new_bd_arm_fpaMvf_i(dbg, block, mode, imm);
				else
					node = new_bd_arm_fpaMnf_i(dbg, block, mode, -imm);
			} else {
#endif
			{
				node = new_bd_arm_fpaConst(dbg, block, tv);
			}
			be_dep_on_frame(node);
			return node;
		}
		else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		}
		else {
			panic("Softfloat not supported yet");
		}
	}
	return create_const_graph(node, block);
}

/**
 * Transforms a SymConst node.
 *
 * @return The transformed ARM node.
 */
static ir_node *gen_SymConst(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	ir_entity *entity = get_SymConst_entity(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *new_node;

	new_node = new_bd_arm_SymConst(dbgi, block, entity, 0);
	be_dep_on_frame(new_node);
	return new_node;
}

/**
 * Transforms a CopyB node.
 *
 * @return The transformed ARM node.
 */
static ir_node *gen_CopyB(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *src      = get_CopyB_src(node);
	ir_node  *new_src  = be_transform_node(src);
	ir_node  *dst      = get_CopyB_dst(node);
	ir_node  *new_dst  = be_transform_node(dst);
	ir_node  *mem      = get_CopyB_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	dbg_info *dbg      = get_irn_dbg_info(node);
	int      size      = get_type_size_bytes(get_CopyB_type(node));
	ir_node  *src_copy;
	ir_node  *dst_copy;

	src_copy = be_new_Copy(&arm_reg_classes[CLASS_arm_gp], block, new_src);
	dst_copy = be_new_Copy(&arm_reg_classes[CLASS_arm_gp], block, new_dst);

 	return new_bd_arm_CopyB(dbg, block, dst_copy, src_copy,
			new_bd_arm_EmptyReg(dbg, block),
			new_bd_arm_EmptyReg(dbg, block),
			new_bd_arm_EmptyReg(dbg, block),
			new_mem, size);
}

/**
 * Transforms a FrameAddr into an ARM Add.
 */
static ir_node *gen_be_FrameAddr(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	ir_entity *ent    = be_get_frame_entity(node);
	ir_node   *fp     = be_get_FrameAddr_frame(node);
	ir_node   *new_fp = be_transform_node(fp);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *new_node;

	new_node = new_bd_arm_FrameAddr(dbgi, block, new_fp, ent, 0);
	return new_node;
}

/**
 * Transform a be_AddSP into an arm_AddSP. Eat up const sizes.
 */
static ir_node *gen_be_AddSP(ir_node *node)
{
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	ir_node  *sz     = get_irn_n(node, be_pos_AddSP_size);
	ir_node  *new_sz = be_transform_node(sz);
	ir_node  *sp     = get_irn_n(node, be_pos_AddSP_old_sp);
	ir_node  *new_sp = be_transform_node(sp);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *nomem  = new_NoMem();
	ir_node  *new_op;

	/* ARM stack grows in reverse direction, make a SubSPandCopy */
	new_op = new_bd_arm_SubSPandCopy(dbgi, block, new_sp, new_sz, nomem);

	return new_op;
}

/**
 * Transform a be_SubSP into an arm_SubSP. Eat up const sizes.
 */
static ir_node *gen_be_SubSP(ir_node *node)
{
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	ir_node  *sz     = get_irn_n(node, be_pos_SubSP_size);
	ir_node  *new_sz = be_transform_node(sz);
	ir_node  *sp     = get_irn_n(node, be_pos_SubSP_old_sp);
	ir_node  *new_sp = be_transform_node(sp);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *nomem  = new_NoMem();
	ir_node  *new_op;

	/* ARM stack grows in reverse direction, make an AddSP */
	new_op = new_bd_arm_AddSP(dbgi, block, new_sp, new_sz, nomem);

	return new_op;
}

/**
 * Transform a be_Copy.
 */
static ir_node *gen_be_Copy(ir_node *node)
{
	ir_node *result = be_duplicate_node(node);
	ir_mode *mode   = get_irn_mode(result);

	if (mode_needs_gp_reg(mode)) {
		set_irn_mode(node, mode_Iu);
	}

	return result;
}

/**
 * Transform a Proj from a Load.
 */
static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *load     = get_Proj_pred(node);
	ir_node  *new_load = be_transform_node(load);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long     proj      = get_Proj_proj(node);

	/* renumber the proj */
	switch (get_arm_irn_opcode(new_load)) {
	case iro_arm_Ldr:
		/* handle all gp loads equal: they have the same proj numbers. */
		if (proj == pn_Load_res) {
			return new_rd_Proj(dbgi, new_load, mode_Iu, pn_arm_Ldr_res);
		} else if (proj == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_arm_Ldr_M);
		}
		break;
	case iro_arm_fpaLdf:
		if (proj == pn_Load_res) {
			ir_mode *mode = get_Load_mode(load);
			return new_rd_Proj(dbgi, new_load, mode, pn_arm_fpaLdf_res);
		} else if (proj == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_arm_fpaLdf_M);
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from Load");
}

/**
 * Transform and renumber the Projs from a CopyB.
 */
static ir_node *gen_Proj_CopyB(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long     proj      = get_Proj_proj(node);

	switch (proj) {
	case pn_CopyB_M_regular:
		if (is_arm_CopyB(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_CopyB_M);
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from CopyB");
}

/**
 * Transform and renumber the Projs from a Quot.
 */
static ir_node *gen_Proj_Quot(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_mode  *mode     = get_irn_mode(node);
	long     proj      = get_Proj_proj(node);

	switch (proj) {
	case pn_Quot_M:
		if (is_arm_fpaDvf(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_fpaDvf_M);
		} else if (is_arm_fpaRdf(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_fpaRdf_M);
		} else if (is_arm_fpaFdv(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_fpaFdv_M);
		} else if (is_arm_fpaFrd(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_fpaFrd_M);
		}
		break;
	case pn_Quot_res:
		if (is_arm_fpaDvf(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode, pn_arm_fpaDvf_res);
		} else if (is_arm_fpaRdf(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode, pn_arm_fpaRdf_res);
		} else if (is_arm_fpaFdv(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode, pn_arm_fpaFdv_res);
		} else if (is_arm_fpaFrd(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode, pn_arm_fpaFrd_res);
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from Quot");
}

/**
 * Transform the Projs of a be_AddSP.
 */
static ir_node *gen_Proj_be_AddSP(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long     proj      = get_Proj_proj(node);

	if (proj == pn_be_AddSP_sp) {
		ir_node *res = new_rd_Proj(dbgi, new_pred, mode_Iu,
		                           pn_arm_SubSPandCopy_stack);
		arch_set_irn_register(res, &arm_gp_regs[REG_SP]);
		return res;
	} else if (proj == pn_be_AddSP_res) {
		return new_rd_Proj(dbgi, new_pred, mode_Iu, pn_arm_SubSPandCopy_addr);
	} else if (proj == pn_be_AddSP_M) {
		return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_SubSPandCopy_M);
	}
	panic("Unsupported Proj from AddSP");
}

/**
 * Transform the Projs of a be_SubSP.
 */
static ir_node *gen_Proj_be_SubSP(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long     proj      = get_Proj_proj(node);

	if (proj == pn_be_SubSP_sp) {
		ir_node *res = new_rd_Proj(dbgi, new_pred, mode_Iu,
		                           pn_arm_AddSP_stack);
		arch_set_irn_register(res, &arm_gp_regs[REG_SP]);
		return res;
	} else if (proj == pn_be_SubSP_M) {
		return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_AddSP_M);
	}
	panic("Unsupported Proj from SubSP");
}

/**
 * Transform the Projs from a Cmp.
 */
static ir_node *gen_Proj_Cmp(ir_node *node)
{
	(void) node;
	panic("Mux NYI");
}


/**
 * Transform the Thread Local Storage Proj.
 */
static ir_node *gen_Proj_tls(ir_node *node)
{
	ir_node *block = be_transform_node(get_nodes_block(node));

	return new_bd_arm_LdTls(NULL, block);
}

/**
 * Transform a Proj node.
 */
static ir_node *gen_Proj(ir_node *node)
{
	ir_graph *irg  = current_ir_graph;
	dbg_info *dbgi = get_irn_dbg_info(node);
	ir_node  *pred = get_Proj_pred(node);
	long     proj  = get_Proj_proj(node);

	if (is_Store(pred)) {
		if (proj == pn_Store_M) {
			return be_transform_node(pred);
		} else {
			panic("Unsupported Proj from Store");
		}
	} else if (is_Load(pred)) {
		return gen_Proj_Load(node);
	} else if (is_CopyB(pred)) {
		return gen_Proj_CopyB(node);
	} else if (is_Quot(pred)) {
		return gen_Proj_Quot(node);
	} else if (be_is_SubSP(pred)) {
		return gen_Proj_be_SubSP(node);
	} else if (be_is_AddSP(pred)) {
		return gen_Proj_be_AddSP(node);
	} else if (is_Cmp(pred)) {
		return gen_Proj_Cmp(node);
	} else if (is_Start(pred)) {
		if (proj == pn_Start_X_initial_exec) {
			ir_node *block = get_nodes_block(pred);
			ir_node *jump;

			/* we exchange the ProjX with a jump */
			block = be_transform_node(block);
			jump  = new_rd_Jmp(dbgi, block);
			return jump;
		}
		if (node == get_irg_anchor(irg, anchor_tls)) {
			return gen_Proj_tls(node);
		}
	} else {
		ir_node *new_pred = be_transform_node(pred);
		ir_mode *mode     = get_irn_mode(node);
		if (mode_needs_gp_reg(mode)) {
			ir_node *new_proj = new_r_Proj(new_pred, mode_Iu,
			                               get_Proj_proj(node));
			new_proj->node_nr = node->node_nr;
			return new_proj;
		}
	}

	return be_duplicate_node(node);
}

typedef ir_node *(*create_const_node_func)(dbg_info *db, ir_node *block);

static inline ir_node *create_const(ir_node **place,
                                    create_const_node_func func,
                                    const arch_register_t* reg)
{
	ir_node *block, *res;

	if (*place != NULL)
		return *place;

	block = get_irg_start_block(env_cg->irg);
	res = func(NULL, block);
	arch_set_irn_register(res, reg);
	*place = res;
	return res;
}

static ir_node *gen_Unknown(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	/* just produce a 0 */
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		tarval *tv = get_mode_null(mode);
		ir_node *node = new_bd_arm_fpaConst(dbgi, new_block, tv);
		be_dep_on_frame(node);
		return node;
	} else if (mode_needs_gp_reg(mode)) {
		return create_const_graph_value(dbgi, new_block, 0);
	}

	panic("Unexpected Unknown mode");
}

/**
 * Change some phi modes
 */
static ir_node *gen_Phi(ir_node *node)
{
	const arch_register_req_t *req;
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *phi;

	if (mode_needs_gp_reg(mode)) {
		/* we shouldn't have any 64bit stuff around anymore */
		assert(get_mode_size_bits(mode) <= 32);
		/* all integer operations are on 32bit registers now */
		mode = mode_Iu;
		req  = arm_reg_classes[CLASS_arm_gp].class_req;
	} else {
		req = arch_no_register_req;
	}

	/* phi nodes allow loops, so we use the old arguments for now
	 * and fix this later */
	phi = new_ir_node(dbgi, irg, block, op_Phi, mode, get_irn_arity(node),
	                  get_irn_in(node) + 1);
	copy_node_attr(irg, node, phi);
	be_duplicate_deps(node, phi);

	arch_set_out_register_req(phi, 0, req);

	be_enqueue_preds(node);

	return phi;
}

/**
 * Enters all transform functions into the generic pointer
 */
static void arm_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Abs,          gen_Abs);
	be_set_transform_function(op_Add,          gen_Add);
	be_set_transform_function(op_And,          gen_And);
	be_set_transform_function(op_be_AddSP,     gen_be_AddSP);
	be_set_transform_function(op_be_Call,      gen_be_Call);
	be_set_transform_function(op_be_Copy,      gen_be_Copy);
	be_set_transform_function(op_be_FrameAddr, gen_be_FrameAddr);
	be_set_transform_function(op_be_SubSP,     gen_be_SubSP);
	be_set_transform_function(op_Cmp,          gen_Cmp);
	be_set_transform_function(op_Cond,         gen_Cond);
	be_set_transform_function(op_Const,        gen_Const);
	be_set_transform_function(op_Conv,         gen_Conv);
	be_set_transform_function(op_CopyB,        gen_CopyB);
	be_set_transform_function(op_Eor,          gen_Eor);
	be_set_transform_function(op_Jmp,          gen_Jmp);
	be_set_transform_function(op_Load,         gen_Load);
	be_set_transform_function(op_Minus,        gen_Minus);
	be_set_transform_function(op_Mul,          gen_Mul);
	be_set_transform_function(op_Not,          gen_Not);
	be_set_transform_function(op_Or,           gen_Or);
	be_set_transform_function(op_Phi,          gen_Phi);
	be_set_transform_function(op_Proj,         gen_Proj);
	be_set_transform_function(op_Quot,         gen_Quot);
	be_set_transform_function(op_Rotl,         gen_Rotl);
	be_set_transform_function(op_Shl,          gen_Shl);
	be_set_transform_function(op_Shr,          gen_Shr);
	be_set_transform_function(op_Shrs,         gen_Shrs);
	be_set_transform_function(op_Store,        gen_Store);
	be_set_transform_function(op_Sub,          gen_Sub);
	be_set_transform_function(op_SymConst,     gen_SymConst);
	be_set_transform_function(op_Unknown,      gen_Unknown);
}

/**
 * Initialize fpa Immediate support.
 */
static void arm_init_fpa_immediate(void)
{
	/* 0, 1, 2, 3, 4, 5, 10, or 0.5. */
	fpa_imm[0][fpa_null]  = get_mode_null(mode_F);
	fpa_imm[0][fpa_one]   = get_mode_one(mode_F);
	fpa_imm[0][fpa_two]   = new_tarval_from_str("2", 1, mode_F);
	fpa_imm[0][fpa_three] = new_tarval_from_str("3", 1, mode_F);
	fpa_imm[0][fpa_four]  = new_tarval_from_str("4", 1, mode_F);
	fpa_imm[0][fpa_five]  = new_tarval_from_str("5", 1, mode_F);
	fpa_imm[0][fpa_ten]   = new_tarval_from_str("10", 2, mode_F);
	fpa_imm[0][fpa_half]  = new_tarval_from_str("0.5", 3, mode_F);

	fpa_imm[1][fpa_null]  = get_mode_null(mode_D);
	fpa_imm[1][fpa_one]   = get_mode_one(mode_D);
	fpa_imm[1][fpa_two]   = new_tarval_from_str("2", 1, mode_D);
	fpa_imm[1][fpa_three] = new_tarval_from_str("3", 1, mode_D);
	fpa_imm[1][fpa_four]  = new_tarval_from_str("4", 1, mode_D);
	fpa_imm[1][fpa_five]  = new_tarval_from_str("5", 1, mode_D);
	fpa_imm[1][fpa_ten]   = new_tarval_from_str("10", 2, mode_D);
	fpa_imm[1][fpa_half]  = new_tarval_from_str("0.5", 3, mode_D);

	fpa_imm[2][fpa_null]  = get_mode_null(mode_E);
	fpa_imm[2][fpa_one]   = get_mode_one(mode_E);
	fpa_imm[2][fpa_two]   = new_tarval_from_str("2", 1, mode_E);
	fpa_imm[2][fpa_three] = new_tarval_from_str("3", 1, mode_E);
	fpa_imm[2][fpa_four]  = new_tarval_from_str("4", 1, mode_E);
	fpa_imm[2][fpa_five]  = new_tarval_from_str("5", 1, mode_E);
	fpa_imm[2][fpa_ten]   = new_tarval_from_str("10", 2, mode_E);
	fpa_imm[2][fpa_half]  = new_tarval_from_str("0.5", 3, mode_E);
}

/**
 * Transform a Firm graph into an ARM graph.
 */
void arm_transform_graph(arm_code_gen_t *cg)
{
	static int imm_initialized = 0;

	if (! imm_initialized) {
		arm_init_fpa_immediate();
		imm_initialized = 1;
	}
	arm_register_transformers();
	env_cg = cg;
	be_transform_graph(cg->irg, NULL);
}

void arm_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.arm.transform");
}
