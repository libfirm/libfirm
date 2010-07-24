/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @author  Matthias Braun, Oliver Richter, Tobias Gneist, Michael Beck
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
#include "../beabihelper.h"
#include "../beabi.h"

#include "bearch_arm_t.h"
#include "arm_nodes_attr.h"
#include "arm_transform.h"
#include "arm_optimize.h"
#include "arm_new_nodes.h"
#include "arm_map_regs.h"
#include "arm_cconv.h"

#include "gen_arm_regalloc_if.h"

#include <limits.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/** hold the current code generator during transformation */
static arm_code_gen_t *env_cg;

static const arch_register_t *sp_reg = &arm_gp_regs[REG_SP];
static ir_mode               *mode_gp;
static ir_mode               *mode_fp;
static beabi_helper_env_t    *abihelper;
static calling_convention_t  *cconv = NULL;

static pmap                  *node_to_stack;

static bool mode_needs_gp_reg(ir_mode *mode)
{
	return mode_is_int(mode) || mode_is_reference(mode);
}

/**
 * create firm graph for a constant
 */
static ir_node *create_const_graph_value(dbg_info *dbgi, ir_node *block,
                                         unsigned int value)
{
	ir_node *result;
	arm_vals v, vn;
	int cnt;

	/* We only have 8 bit immediates. So we possibly have to combine several
	 * operations to construct the desired value.
	 *
	 * we can either create the value by adding bits to 0 or by removing bits
	 * from an register with all bits set. Try which alternative needs fewer
	 * operations */
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
		if (USE_FPA(env_cg->isa)) {
			if (mode_is_float(src_mode)) {
				if (mode_is_float(dst_mode)) {
					/* from float to float */
					return new_bd_arm_Mvf(dbg, block, new_op, dst_mode);
				} else {
					/* from float to int */
					panic("TODO");
				}
			} else {
				/* from int to float */
				if (!mode_is_signed(src_mode)) {
					panic("TODO");
				} else {
					return new_bd_arm_FltX(dbg, block, new_op, dst_mode);
				}
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
			/* kill unnecessary conv */
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
	MATCH_COMMUTATIVE  = 1 << 0,  /**< commutative node */
	MATCH_SIZE_NEUTRAL = 1 << 1,
	MATCH_SKIP_NOT     = 1 << 2,  /**< skip Not on ONE input */
} match_flags_t;

/**
 * possible binop constructors.
 */
typedef struct arm_binop_factory_t {
	/** normal reg op reg operation. */
	ir_node *(*new_binop_reg)(dbg_info *dbgi, ir_node *block, ir_node *op1, ir_node *op2);
	/** normal reg op imm operation. */
	ir_node *(*new_binop_imm)(dbg_info *dbgi, ir_node *block, ir_node *op1, unsigned char imm8, unsigned char imm_rot);
	/** barrel shifter reg op (reg shift reg operation. */
	ir_node *(*new_binop_reg_shift_reg)(dbg_info *dbgi, ir_node *block, ir_node *left, ir_node *right, ir_node *shift, arm_shift_modifier_t shift_modifier);
	/** barrel shifter reg op (reg shift imm operation. */
	ir_node *(*new_binop_reg_shift_imm)(dbg_info *dbgi, ir_node *block, ir_node *left, ir_node *right, arm_shift_modifier_t shift_modifier, unsigned shift_immediate);
} arm_binop_factory_t;

static ir_node *gen_int_binop(ir_node *node, match_flags_t flags,
		const arm_binop_factory_t *factory)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_binop_left(node);
	ir_node  *new_op1;
	ir_node  *op2     = get_binop_right(node);
	ir_node  *new_op2;
	dbg_info *dbgi    = get_irn_dbg_info(node);
	arm_immediate_t imm;

	if (flags & MATCH_SKIP_NOT) {
		if (is_Not(op1))
			op1 = get_Not_op(op1);
		else if (is_Not(op2))
			op2 = get_Not_op(op2);
		else
			panic("cannot execute MATCH_SKIP_NOT");
	}
	if (flags & MATCH_SIZE_NEUTRAL) {
		op1 = arm_skip_downconv(op1);
		op2 = arm_skip_downconv(op2);
	} else {
		assert(get_mode_size_bits(get_irn_mode(node)) == 32);
	}

	if (try_encode_as_immediate(op2, &imm)) {
		ir_node *new_op1 = be_transform_node(op1);
		return factory->new_binop_imm(dbgi, block, new_op1, imm.imm_8, imm.rot);
	}
	new_op2 = be_transform_node(op2);
    if ((flags & MATCH_COMMUTATIVE) && try_encode_as_immediate(op1, &imm)) {
		return factory->new_binop_imm(dbgi, block, new_op2, imm.imm_8, imm.rot);
	}
	new_op1 = be_transform_node(op1);

	/* check if we can fold in a Mov */
	if (is_arm_Mov(new_op2)) {
		const arm_shifter_operand_t *attr = get_arm_shifter_operand_attr_const(new_op2);

		switch (attr->shift_modifier) {
		case ARM_SHF_IMM:
		case ARM_SHF_ASR_IMM:
		case ARM_SHF_LSL_IMM:
		case ARM_SHF_LSR_IMM:
		case ARM_SHF_ROR_IMM:
			if (factory->new_binop_reg_shift_imm) {
				ir_node *mov_op = get_irn_n(new_op2, 0);
				return factory->new_binop_reg_shift_imm(dbgi, block, new_op1, mov_op,
					attr->shift_modifier, attr->shift_immediate);
			}
			break;

		case ARM_SHF_ASR_REG:
		case ARM_SHF_LSL_REG:
		case ARM_SHF_LSR_REG:
		case ARM_SHF_ROR_REG:
			if (factory->new_binop_reg_shift_reg) {
				ir_node *mov_op  = get_irn_n(new_op2, 0);
				ir_node *mov_sft = get_irn_n(new_op2, 1);
				return factory->new_binop_reg_shift_reg(dbgi, block, new_op1, mov_op, mov_sft,
					attr->shift_modifier);
			}
			break;
		}
	}
	if ((flags & MATCH_COMMUTATIVE) && is_arm_Mov(new_op1)) {
		const arm_shifter_operand_t *attr = get_arm_shifter_operand_attr_const(new_op1);

		switch (attr->shift_modifier) {
			ir_node *mov_op, *mov_sft;

		case ARM_SHF_IMM:
		case ARM_SHF_ASR_IMM:
		case ARM_SHF_LSL_IMM:
		case ARM_SHF_LSR_IMM:
		case ARM_SHF_ROR_IMM:
			if (factory->new_binop_reg_shift_imm) {
				mov_op = get_irn_n(new_op1, 0);
				return factory->new_binop_reg_shift_imm(dbgi, block, new_op2, mov_op,
					attr->shift_modifier, attr->shift_immediate);
			}
			break;

		case ARM_SHF_ASR_REG:
		case ARM_SHF_LSL_REG:
		case ARM_SHF_LSR_REG:
		case ARM_SHF_ROR_REG:
			if (factory->new_binop_reg_shift_reg) {
				mov_op  = get_irn_n(new_op1, 0);
				mov_sft = get_irn_n(new_op1, 1);
				return factory->new_binop_reg_shift_reg(dbgi, block, new_op2, mov_op, mov_sft,
					attr->shift_modifier);
			}
			break;
		}
	}
	return factory->new_binop_reg(dbgi, block, new_op1, new_op2);
}

/**
 * Creates an ARM Add.
 *
 * @return the created arm Add node
 */
static ir_node *gen_Add(ir_node *node)
{
	static const arm_binop_factory_t add_factory = {
		new_bd_arm_Add_reg,
		new_bd_arm_Add_imm,
		new_bd_arm_Add_reg_shift_reg,
		new_bd_arm_Add_reg_shift_imm
	};

	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		ir_node  *block   = be_transform_node(get_nodes_block(node));
		ir_node  *op1     = get_Add_left(node);
		ir_node  *op2     = get_Add_right(node);
		dbg_info *dbgi    = get_irn_dbg_info(node);
		ir_node  *new_op1 = be_transform_node(op1);
		ir_node  *new_op2 = be_transform_node(op2);
		if (USE_FPA(env_cg->isa)) {
			return new_bd_arm_Adf(dbgi, block, new_op1, new_op2, mode);
		} else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		} else {
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

		return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL, &add_factory);
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
		if (USE_FPA(env_cg->isa)) {
			return new_bd_arm_Muf(dbg, block, new_op1, new_op2, mode);
		} else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	}
	assert(mode_is_data(mode));
	return new_bd_arm_Mul(dbg, block, new_op1, new_op2);
}

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

	if (USE_FPA(env_cg->isa)) {
		return new_bd_arm_Dvf(dbg, block, new_op1, new_op2, mode);
	} else if (USE_VFP(env_cg->isa)) {
		assert(mode != mode_E && "IEEE Extended FP not supported");
		panic("VFP not supported yet");
	} else {
		panic("Softfloat not supported yet");
	}
}

static ir_node *gen_And(ir_node *node)
{
	static const arm_binop_factory_t and_factory = {
		new_bd_arm_And_reg,
		new_bd_arm_And_imm,
		new_bd_arm_And_reg_shift_reg,
		new_bd_arm_And_reg_shift_imm
	};
	static const arm_binop_factory_t bic_factory = {
		new_bd_arm_Bic_reg,
		new_bd_arm_Bic_imm,
		new_bd_arm_Bic_reg_shift_reg,
		new_bd_arm_Bic_reg_shift_imm
	};

	/* check for and not */
	ir_node *left  = get_And_left(node);
	ir_node *right = get_And_right(node);

	if (is_Not(left) || is_Not(right)) {
		return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL | MATCH_SKIP_NOT,
			&bic_factory);
	}

	return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL, &and_factory);
}

static ir_node *gen_Or(ir_node *node)
{
	static const arm_binop_factory_t or_factory = {
		new_bd_arm_Or_reg,
		new_bd_arm_Or_imm,
		new_bd_arm_Or_reg_shift_reg,
		new_bd_arm_Or_reg_shift_imm
	};

	return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL, &or_factory);
}

static ir_node *gen_Eor(ir_node *node)
{
	static const arm_binop_factory_t eor_factory = {
		new_bd_arm_Eor_reg,
		new_bd_arm_Eor_imm,
		new_bd_arm_Eor_reg_shift_reg,
		new_bd_arm_Eor_reg_shift_imm
	};

	return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL, &eor_factory);
}

static ir_node *gen_Sub(ir_node *node)
{
	static const arm_binop_factory_t sub_factory = {
		new_bd_arm_Sub_reg,
		new_bd_arm_Sub_imm,
		new_bd_arm_Sub_reg_shift_reg,
		new_bd_arm_Sub_reg_shift_imm
	};

	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_Sub_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_Sub_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	ir_mode  *mode    = get_irn_mode(node);
	dbg_info *dbgi    = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		if (USE_FPA(env_cg->isa)) {
			return new_bd_arm_Suf(dbgi, block, new_op1, new_op2, mode);
		} else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	} else {
		return gen_int_binop(node, MATCH_SIZE_NEUTRAL, &sub_factory);
	}
}

/**
 * Checks if a given value can be used as an immediate for the given
 * ARM shift mode.
 */
static bool can_use_shift_constant(unsigned int val,
                                   arm_shift_modifier_t modifier)
{
	if (val <= 31)
		return true;
	if (val == 32 && modifier != ARM_SHF_LSL_REG && modifier != ARM_SHF_ROR_REG)
		return true;
	return false;
}

/**
 * generate an ARM shift instruction.
 *
 * @param node            the node
 * @param flags           matching flags
 * @param shift_modifier  initial encoding of the desired shift operation
 */
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

static ir_node *gen_Shl(ir_node *node)
{
	return make_shift(node, MATCH_SIZE_NEUTRAL, ARM_SHF_LSL_REG);
}

static ir_node *gen_Shr(ir_node *node)
{
	return make_shift(node, MATCH_NONE, ARM_SHF_LSR_REG);
}

static ir_node *gen_Shrs(ir_node *node)
{
	return make_shift(node, MATCH_NONE, ARM_SHF_ASR_REG);
}

static ir_node *gen_Ror(ir_node *node, ir_node *op1, ir_node *op2)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *new_op1 = be_transform_node(op1);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *new_op2 = be_transform_node(op2);

	return new_bd_arm_Mov_reg_shift_reg(dbgi, block, new_op1, new_op2,
	                                    ARM_SHF_ROR_REG);
}

static ir_node *gen_Rol(ir_node *node, ir_node *op1, ir_node *op2)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *new_op1 = be_transform_node(op1);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *new_op2 = be_transform_node(op2);

	/* Note: there is no Rol on arm, we have to use Ror */
	new_op2 = new_bd_arm_Rsb_imm(dbgi, block, new_op2, 32, 0);
	return new_bd_arm_Mov_reg_shift_reg(dbgi, block, new_op1, new_op2,
	                                    ARM_SHF_ROR_REG);
}

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

static ir_node *gen_Not(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op      = get_Not_op(node);
	ir_node  *new_op  = be_transform_node(op);
	dbg_info *dbgi    = get_irn_dbg_info(node);

	/* check if we can fold in a Mov */
	if (is_arm_Mov(new_op)) {
		const arm_shifter_operand_t *attr = get_arm_shifter_operand_attr_const(new_op);

		switch (attr->shift_modifier) {
			ir_node *mov_op, *mov_sft;

		case ARM_SHF_IMM:
		case ARM_SHF_ASR_IMM:
		case ARM_SHF_LSL_IMM:
		case ARM_SHF_LSR_IMM:
		case ARM_SHF_ROR_IMM:
			mov_op = get_irn_n(new_op, 0);
			return new_bd_arm_Mvn_reg_shift_imm(dbgi, block, mov_op,
				attr->shift_modifier, attr->shift_immediate);

		case ARM_SHF_ASR_REG:
		case ARM_SHF_LSL_REG:
		case ARM_SHF_LSR_REG:
		case ARM_SHF_ROR_REG:
			mov_op  = get_irn_n(new_op, 0);
			mov_sft = get_irn_n(new_op, 1);
			return new_bd_arm_Mvn_reg_shift_reg(dbgi, block, mov_op, mov_sft,
				attr->shift_modifier);
		}
	}

	return new_bd_arm_Mvn_reg(dbgi, block, new_op);
}

static ir_node *gen_Minus(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op      = get_Minus_op(node);
	ir_node  *new_op  = be_transform_node(op);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_mode  *mode    = get_irn_mode(node);

	if (mode_is_float(mode)) {
		if (USE_FPA(env_cg->isa)) {
			return new_bd_arm_Mvf(dbgi, block, op, mode);
		} else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	}
	assert(mode_is_data(mode));
	return new_bd_arm_Rsb_imm(dbgi, block, new_op, 0, 0);
}

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
		if (USE_FPA(env_cg->isa)) {
			new_load = new_bd_arm_Ldf(dbgi, block, new_ptr, new_mem, mode,
			                          NULL, 0, 0, false);
		} else if (USE_VFP(env_cg->isa)) {
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
		if (USE_FPA(env_cg->isa)) {
			new_store = new_bd_arm_Stf(dbgi, block, new_ptr, new_val,
			                           new_mem, mode, NULL, 0, 0, false);
		} else if (USE_VFP(env_cg->isa)) {
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
		/* TODO: this is broken... */
		new_op1 = be_transform_node(op1);
		new_op2 = be_transform_node(op2);

		return new_bd_arm_Cmfe(dbgi, block, new_op1, new_op2, false);

		panic("FloatCmp NIY");
#if 0
		ir_node *new_op2  = be_transform_node(op2);
		/* floating point compare */
		pn_Cmp pnc = get_Proj_proj(selector);

		if (pnc & pn_Cmp_Uo) {
			/* check for unordered, need cmf */
			return new_bd_arm_CmfBra(dbgi, block, new_op1, new_op2, pnc);
		}
		/* Hmm: use need cmfe */
		return new_bd_arm_CmfeBra(dbgi, block, new_op1, new_op2, pnc);
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

	/* integer compare, TODO: use shifter_op in all its combinations */
	new_op1 = be_transform_node(op1);
	new_op1 = gen_extension(dbgi, block, new_op1, cmp_mode);
	new_op2 = be_transform_node(op2);
	new_op2 = gen_extension(dbgi, block, new_op2, cmp_mode);
	return new_bd_arm_Cmp_reg(dbgi, block, new_op1, new_op2, false,
	                          is_unsigned);
}

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

static ir_node *gen_Const(ir_node *node)
{
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_mode *mode = get_irn_mode(node);
	dbg_info *dbg = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		if (USE_FPA(env_cg->isa)) {
			tarval *tv = get_Const_tarval(node);
			node       = new_bd_arm_fConst(dbg, block, tv);
			be_dep_on_frame(node);
			return node;
		} else if (USE_VFP(env_cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	}
	return create_const_graph(node, block);
}

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

static ir_node *ints_to_double(dbg_info *dbgi, ir_node *block, ir_node *node0,
                               ir_node *node1)
{
	/* the good way to do this would be to use the stm (store multiple)
	 * instructions, since our input is nearly always 2 consecutive 32bit
	 * registers... */
	ir_graph *irg   = current_ir_graph;
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = new_NoMem();
	ir_node  *str0  = new_bd_arm_Str(dbgi, block, stack, node0, nomem, mode_gp,
	                                 NULL, 0, 0, true);
	ir_node  *str1  = new_bd_arm_Str(dbgi, block, stack, node1, nomem, mode_gp,
	                                 NULL, 0, 4, true);
	ir_node  *in[2] = { str0, str1 };
	ir_node  *sync  = new_r_Sync(block, 2, in);
	ir_node  *ldf;
	set_irn_pinned(str0, op_pin_state_floats);
	set_irn_pinned(str1, op_pin_state_floats);

	ldf = new_bd_arm_Ldf(dbgi, block, stack, sync, mode_D, NULL, 0, 0, true);
	set_irn_pinned(ldf, op_pin_state_floats);

	return new_Proj(ldf, mode_fp, pn_arm_Ldf_res);
}

static ir_node *int_to_float(dbg_info *dbgi, ir_node *block, ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = new_NoMem();
	ir_node  *str   = new_bd_arm_Str(dbgi, block, stack, node, nomem, mode_gp,
	                                 NULL, 0, 0, true);
	ir_node  *ldf;
	set_irn_pinned(str, op_pin_state_floats);

	ldf = new_bd_arm_Ldf(dbgi, block, stack, str, mode_F, NULL, 0, 0, true);
	set_irn_pinned(ldf, op_pin_state_floats);

	return new_Proj(ldf, mode_fp, pn_arm_Ldf_res);
}

static ir_node *float_to_int(dbg_info *dbgi, ir_node *block, ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = new_NoMem();
	ir_node  *stf   = new_bd_arm_Stf(dbgi, block, stack, node, nomem, mode_F,
	                                 NULL, 0, 0, true);
	ir_node  *ldr;
	set_irn_pinned(stf, op_pin_state_floats);

	ldr = new_bd_arm_Ldr(dbgi, block, stack, stf, mode_gp, NULL, 0, 0, true);
	set_irn_pinned(ldr, op_pin_state_floats);

	return new_Proj(ldr, mode_gp, pn_arm_Ldr_res);
}

static void double_to_ints(dbg_info *dbgi, ir_node *block, ir_node *node,
                           ir_node **out_value0, ir_node **out_value1)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = new_NoMem();
	ir_node  *stf   = new_bd_arm_Stf(dbgi, block, stack, node, nomem, mode_D,
	                                 NULL, 0, 0, true);
	ir_node  *ldr0, *ldr1;
	set_irn_pinned(stf, op_pin_state_floats);

	ldr0 = new_bd_arm_Ldr(dbgi, block, stack, stf, mode_gp, NULL, 0, 0, true);
	set_irn_pinned(ldr0, op_pin_state_floats);
	ldr1 = new_bd_arm_Ldr(dbgi, block, stack, stf, mode_gp, NULL, 0, 4, true);
	set_irn_pinned(ldr1, op_pin_state_floats);

	*out_value0 = new_Proj(ldr0, mode_gp, pn_arm_Ldr_res);
	*out_value1 = new_Proj(ldr1, mode_gp, pn_arm_Ldr_res);
}

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
	case iro_arm_Ldf:
		if (proj == pn_Load_res) {
			ir_mode *mode = get_Load_mode(load);
			return new_rd_Proj(dbgi, new_load, mode, pn_arm_Ldf_res);
		} else if (proj == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_arm_Ldf_M);
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from Load");
}

static ir_node *gen_Proj_CopyB(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long     proj      = get_Proj_proj(node);

	switch (proj) {
	case pn_CopyB_M:
		if (is_arm_CopyB(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_CopyB_M);
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from CopyB");
}

static ir_node *gen_Proj_Quot(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_mode  *mode     = get_irn_mode(node);
	long     proj      = get_Proj_proj(node);

	switch (proj) {
	case pn_Quot_M:
		if (is_arm_Dvf(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_Dvf_M);
		}
		break;
	case pn_Quot_res:
		if (is_arm_Dvf(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode, pn_arm_Dvf_res);
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from Quot");
}

/**
 * Transform the Projs from a Cmp.
 */
static ir_node *gen_Proj_Cmp(ir_node *node)
{
	(void) node;
	/* we should only be here in case of a Mux node */
	panic("Mux NYI");
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_node *block     = get_nodes_block(node);
	ir_node *new_block = be_transform_node(block);
	ir_node *barrier   = be_transform_node(get_Proj_pred(node));
	long     proj      = get_Proj_proj(node);

	switch ((pn_Start) proj) {
	case pn_Start_X_initial_exec:
		/* we exchange the ProjX with a jump */
		return new_bd_arm_Jmp(NULL, new_block);

	case pn_Start_M:
		return new_r_Proj(barrier, mode_M, 0);

	case pn_Start_T_args:
		return barrier;

	case pn_Start_P_frame_base:
		return be_prolog_get_reg_value(abihelper, sp_reg);

	case pn_Start_P_tls:
		return new_Bad();

	case pn_Start_max:
		break;
	}
	panic("unexpected start proj: %ld\n", proj);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	long       pn          = get_Proj_proj(node);
	ir_node   *block       = get_nodes_block(node);
	ir_node   *new_block   = be_transform_node(block);
	ir_entity *entity      = get_irg_entity(current_ir_graph);
	ir_type   *method_type = get_entity_type(entity);
	ir_type   *param_type  = get_method_param_type(method_type, pn);
	const reg_or_stackslot_t *param;

	/* Proj->Proj->Start must be a method argument */
	assert(get_Proj_proj(get_Proj_pred(node)) == pn_Start_T_args);

	param = &cconv->parameters[pn];

	if (param->reg0 != NULL) {
		/* argument transmitted in register */
		ir_mode *mode  = get_type_mode(param_type);
		ir_node *value = be_prolog_get_reg_value(abihelper, param->reg0);

		if (mode_is_float(mode)) {
			ir_node *value1 = NULL;

			if (param->reg1 != NULL) {
				value1 = be_prolog_get_reg_value(abihelper, param->reg1);
			} else if (param->entity != NULL) {
				ir_graph *irg = get_irn_irg(node);
				ir_node  *fp  = get_irg_frame(irg);
				ir_node  *mem = be_prolog_get_memory(abihelper);
				ir_node  *ldr = new_bd_arm_Ldr(NULL, new_block, fp, mem,
				                               mode_gp, param->entity,
				                               0, 0, true);
				value1 = new_Proj(ldr, mode_gp, pn_arm_Ldr_res);
			}

			/* convert integer value to float */
			if (value1 == NULL) {
				value = int_to_float(NULL, new_block, value);
			} else {
				value = ints_to_double(NULL, new_block, value, value1);
			}
		}
		return value;
	} else {
		/* argument transmitted on stack */
		ir_graph *irg  = get_irn_irg(node);
		ir_node  *fp   = get_irg_frame(irg);
		ir_node  *mem  = be_prolog_get_memory(abihelper);
		ir_mode  *mode = get_type_mode(param->type);
		ir_node  *load;
		ir_node  *value;

		if (mode_is_float(mode)) {
			load  = new_bd_arm_Ldf(NULL, new_block, fp, mem, mode,
			                       param->entity, 0, 0, true);
			value = new_r_Proj(load, mode_fp, pn_arm_Ldf_res);
		} else {
			load  = new_bd_arm_Ldr(NULL, new_block, fp, mem, mode,
			                       param->entity, 0, 0, true);
			value = new_r_Proj(load, mode_gp, pn_arm_Ldr_res);
		}
		set_irn_pinned(load, op_pin_state_floats);

		return value;
	}
}

/**
 * Finds number of output value of a mode_T node which is constrained to
 * a single specific register.
 */
static int find_out_for_reg(ir_node *node, const arch_register_t *reg)
{
	int n_outs = arch_irn_get_n_outs(node);
	int o;

	for (o = 0; o < n_outs; ++o) {
		const arch_register_req_t *req = arch_get_out_register_req(node, o);
		if (req == reg->single_req)
			return o;
	}
	return -1;
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	long                  pn            = get_Proj_proj(node);
	ir_node              *call          = get_Proj_pred(get_Proj_pred(node));
	ir_node              *new_call      = be_transform_node(call);
	ir_type              *function_type = get_Call_type(call);
	calling_convention_t *cconv = arm_decide_calling_convention(function_type);
	const reg_or_stackslot_t *res = &cconv->results[pn];
	ir_mode              *mode;
	int                   regn;

	/* TODO 64bit modes */
	assert(res->reg0 != NULL && res->reg1 == NULL);
	regn = find_out_for_reg(new_call, res->reg0);
	if (regn < 0) {
		panic("Internal error in calling convention for return %+F", node);
	}
	mode = res->reg0->reg_class->mode;

	arm_free_calling_convention(cconv);

	return new_r_Proj(new_call, mode, regn);
}

static ir_node *gen_Proj_Call(ir_node *node)
{
	long     pn        = get_Proj_proj(node);
	ir_node *call      = get_Proj_pred(node);
	ir_node *new_call  = be_transform_node(call);

	switch ((pn_Call) pn) {
	case pn_Call_M:
		return new_r_Proj(new_call, mode_M, 0);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
	case pn_Call_P_value_res_base:
	case pn_Call_max:
		break;
	}
	panic("Unexpected Call proj %ld\n", pn);
}

/**
 * Transform a Proj node.
 */
static ir_node *gen_Proj(ir_node *node)
{
	ir_node  *pred = get_Proj_pred(node);
	long      proj = get_Proj_proj(node);

	switch (get_irn_opcode(pred)) {
	case iro_Store:
		if (proj == pn_Store_M) {
			return be_transform_node(pred);
		} else {
			panic("Unsupported Proj from Store");
		}
	case iro_Load:
		return gen_Proj_Load(node);
	case iro_Call:
		return gen_Proj_Call(node);
	case iro_CopyB:
		return gen_Proj_CopyB(node);
	case iro_Quot:
		return gen_Proj_Quot(node);
	case iro_Cmp:
		return gen_Proj_Cmp(node);
	case iro_Start:
		return gen_Proj_Start(node);
	case iro_Cond:
		/* nothing to do */
		return be_duplicate_node(node);
	case iro_Proj: {
		ir_node *pred_pred = get_Proj_pred(pred);
		if (is_Call(pred_pred)) {
			return gen_Proj_Proj_Call(node);
		} else if (is_Start(pred_pred)) {
			return gen_Proj_Proj_Start(node);
		}
		/* FALLTHROUGH */
	}
	default:
		panic("code selection didn't expect Proj after %+F\n", pred);
	}
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
		ir_node *node = new_bd_arm_fConst(dbgi, new_block, tv);
		be_dep_on_frame(node);
		return node;
	} else if (mode_needs_gp_reg(mode)) {
		return create_const_graph_value(dbgi, new_block, 0);
	}

	panic("Unexpected Unknown mode");
}

/**
 * Produces the type which sits between the stack args and the locals on the
 * stack. It will contain the return address and space to store the old base
 * pointer.
 * @return The Firm type modeling the ABI between type.
 */
static ir_type *arm_get_between_type(void)
{
	static ir_type *between_type = NULL;

	if (between_type == NULL) {
		between_type = new_type_class(new_id_from_str("arm_between_type"));
		set_type_size_bytes(between_type, 0);
	}

	return between_type;
}

static void create_stacklayout(ir_graph *irg)
{
	ir_entity         *entity        = get_irg_entity(irg);
	ir_type           *function_type = get_entity_type(entity);
	be_stack_layout_t *layout        = be_get_irg_stack_layout(irg);
	ir_type           *arg_type;
	int                p;
	int                n_params;

	/* calling conventions must be decided by now */
	assert(cconv != NULL);

	/* construct argument type */
	arg_type = new_type_struct(id_mangle_u(get_entity_ident(entity), new_id_from_chars("arg_type", 8)));
	n_params = get_method_n_params(function_type);
	for (p = 0; p < n_params; ++p) {
		reg_or_stackslot_t *param = &cconv->parameters[p];
		char                buf[128];
		ident              *id;

		if (param->type == NULL)
			continue;

		snprintf(buf, sizeof(buf), "param_%d", p);
		id            = new_id_from_str(buf);
		param->entity = new_entity(arg_type, id, param->type);
		set_entity_offset(param->entity, param->offset);
	}

	/* TODO: what about external functions? we don't know most of the stack
	 * layout for them. And probably don't need all of this... */
	memset(layout, 0, sizeof(*layout));

	layout->frame_type     = get_irg_frame_type(irg);
	layout->between_type   = arm_get_between_type();
	layout->arg_type       = arg_type;
	layout->param_map      = NULL; /* TODO */
	layout->initial_offset = 0;
	layout->initial_bias   = 0;
	layout->stack_dir      = -1;
	layout->sp_relative    = true;

	assert(N_FRAME_TYPES == 3);
	layout->order[0] = layout->frame_type;
	layout->order[1] = layout->between_type;
	layout->order[2] = layout->arg_type;
}

/**
 * transform the start node to the prolog code + initial barrier
 */
static ir_node *gen_Start(ir_node *node)
{
	ir_graph  *irg           = get_irn_irg(node);
	ir_entity *entity        = get_irg_entity(irg);
	ir_type   *function_type = get_entity_type(entity);
	ir_node   *block         = get_nodes_block(node);
	ir_node   *new_block     = be_transform_node(block);
	dbg_info  *dbgi          = get_irn_dbg_info(node);
	ir_node   *start;
	ir_node   *incsp;
	ir_node   *sp;
	ir_node   *barrier;
	int        i;

	/* stackpointer is important at function prolog */
	be_prolog_add_reg(abihelper, sp_reg,
			arch_register_req_type_produces_sp | arch_register_req_type_ignore);
	/* function parameters in registers */
	for (i = 0; i < get_method_n_params(function_type); ++i) {
		const reg_or_stackslot_t *param = &cconv->parameters[i];
		if (param->reg0 != NULL)
			be_prolog_add_reg(abihelper, param->reg0, 0);
		if (param->reg1 != NULL)
			be_prolog_add_reg(abihelper, param->reg1, 0);
	}
	/* announce that we need the values of the callee save regs */
	for (i = 0; i < (int) (sizeof(callee_saves)/sizeof(callee_saves[0])); ++i) {
		be_prolog_add_reg(abihelper, callee_saves[i], 0);
	}

	start = be_prolog_create_start(abihelper, dbgi, new_block);
	sp    = be_prolog_get_reg_value(abihelper, sp_reg);
	incsp = be_new_IncSP(sp_reg, new_block, sp, BE_STACK_FRAME_SIZE_EXPAND, 0);
	be_prolog_set_reg_value(abihelper, sp_reg, incsp);
	barrier = be_prolog_create_barrier(abihelper, new_block);

	return barrier;
}

static ir_node *get_stack_pointer_for(ir_node *node)
{
	/* get predecessor in stack_order list */
	ir_node *stack_pred = be_get_stack_pred(abihelper, node);
	ir_node *stack_pred_transformed;
	ir_node *stack;

	if (stack_pred == NULL) {
		/* first stack user in the current block. We can simply use the
		 * initial sp_proj for it */
		ir_node *sp_proj = be_prolog_get_reg_value(abihelper, sp_reg);
		return sp_proj;
	}

	stack_pred_transformed = be_transform_node(stack_pred);
	stack                  = pmap_get(node_to_stack, stack_pred);
	if (stack == NULL) {
		return get_stack_pointer_for(stack_pred);
	}

	return stack;
}

/**
 * transform a Return node into epilogue code + return statement
 */
static ir_node *gen_Return(ir_node *node)
{
	ir_node   *block          = get_nodes_block(node);
	ir_node   *new_block      = be_transform_node(block);
	dbg_info  *dbgi           = get_irn_dbg_info(node);
	ir_node   *mem            = get_Return_mem(node);
	ir_node   *new_mem        = be_transform_node(mem);
	int        n_callee_saves = sizeof(callee_saves)/sizeof(callee_saves[0]);
	ir_node   *sp_proj        = get_stack_pointer_for(node);
	int        n_res          = get_Return_n_ress(node);
	ir_node   *bereturn;
	ir_node   *incsp;
	int        i;

	be_epilog_begin(abihelper);
	be_epilog_set_memory(abihelper, new_mem);
	/* connect stack pointer with initial stack pointer. fix_stack phase
	   will later serialize all stack pointer adjusting nodes */
	be_epilog_add_reg(abihelper, sp_reg,
			arch_register_req_type_produces_sp | arch_register_req_type_ignore,
			sp_proj);

	/* result values */
	for (i = 0; i < n_res; ++i) {
		ir_node                  *res_value     = get_Return_res(node, i);
		ir_node                  *new_res_value = be_transform_node(res_value);
		const reg_or_stackslot_t *slot          = &cconv->results[i];
		const arch_register_t    *reg           = slot->reg0;
		assert(slot->reg1 == NULL);
		be_epilog_add_reg(abihelper, reg, 0, new_res_value);
	}

	/* connect callee saves with their values at the function begin */
	for (i = 0; i < n_callee_saves; ++i) {
		const arch_register_t *reg   = callee_saves[i];
		ir_node               *value = be_prolog_get_reg_value(abihelper, reg);
		be_epilog_add_reg(abihelper, reg, 0, value);
	}

	/* create the barrier before the epilog code */
	be_epilog_create_barrier(abihelper, new_block);

	/* epilog code: an incsp */
	sp_proj = be_epilog_get_reg_value(abihelper, sp_reg);
	incsp   = be_new_IncSP(sp_reg, new_block, sp_proj,
	                       BE_STACK_FRAME_SIZE_SHRINK, 0);
	be_epilog_set_reg_value(abihelper, sp_reg, incsp);

	bereturn = be_epilog_create_return(abihelper, dbgi, new_block);

	return bereturn;
}


static ir_node *gen_Call(ir_node *node)
{
	ir_graph             *irg          = get_irn_irg(node);
	ir_node              *callee       = get_Call_ptr(node);
	ir_node              *block        = get_nodes_block(node);
	ir_node              *new_block    = be_transform_node(block);
	ir_node              *mem          = get_Call_mem(node);
	ir_node              *new_mem      = be_transform_node(mem);
	dbg_info             *dbgi         = get_irn_dbg_info(node);
	ir_type              *type         = get_Call_type(node);
	calling_convention_t *cconv        = arm_decide_calling_convention(type);
	int                   n_params     = get_Call_n_params(node);
	int                   n_param_regs = sizeof(param_regs)/sizeof(param_regs[0]);
	/* max inputs: memory, callee, register arguments */
	int                   max_inputs   = 2 + n_param_regs;
	ir_node             **in           = ALLOCAN(ir_node*, max_inputs);
	ir_node             **sync_ins     = ALLOCAN(ir_node*, max_inputs);
	struct obstack       *obst         = be_get_be_obst(irg);
	const arch_register_req_t **in_req
		= OALLOCNZ(obst, const arch_register_req_t*, max_inputs);
	int                   in_arity     = 0;
	int                   sync_arity   = 0;
	int                   n_caller_saves
		= sizeof(caller_saves)/sizeof(caller_saves[0]);
	ir_entity            *entity       = NULL;
	ir_node              *incsp        = NULL;
	int                   mem_pos;
	ir_node              *res;
	int                   p;
	int                   o;
	int                   out_arity;

	assert(n_params == get_method_n_params(type));

	/* construct arguments */

	/* memory input */
	in_req[in_arity] = arch_no_register_req;
	mem_pos          = in_arity;
	++in_arity;
	/* parameters */
	for (p = 0; p < n_params; ++p) {
		ir_node                  *value      = get_Call_param(node, p);
		ir_node                  *new_value  = be_transform_node(value);
		ir_node                  *new_value1 = NULL;
		const reg_or_stackslot_t *param      = &cconv->parameters[p];
		ir_type                  *param_type = get_method_param_type(type, p);
		ir_mode                  *mode       = get_type_mode(param_type);
		ir_node                  *str;

		if (mode_is_float(mode) && param->reg0 != NULL) {
			unsigned size_bits = get_mode_size_bits(mode);
			if (size_bits == 64) {
				double_to_ints(dbgi, new_block, new_value, &new_value,
				               &new_value1);
			} else {
				assert(size_bits == 32);
				new_value = float_to_int(dbgi, new_block, new_value);
			}
		}

		/* put value into registers */
		if (param->reg0 != NULL) {
			in[in_arity]     = new_value;
			in_req[in_arity] = param->reg0->single_req;
			++in_arity;
			if (new_value1 == NULL)
				continue;
		}
		if (param->reg1 != NULL) {
			assert(new_value1 != NULL);
			in[in_arity]     = new_value1;
			in_req[in_arity] = param->reg1->single_req;
			++in_arity;
			continue;
		}

		/* we need a store if we're here */
		if (new_value1 != NULL) {
			new_value = new_value1;
			mode      = mode_gp;
		}

		/* create a parameter frame if necessary */
		if (incsp == NULL) {
			ir_node *new_frame = get_stack_pointer_for(node);
			incsp = be_new_IncSP(sp_reg, new_block, new_frame,
								 cconv->param_stack_size, 1);
		}
		if (mode_is_float(mode)) {
			str = new_bd_arm_Stf(dbgi, new_block, incsp, new_value, new_mem,
			                     mode, NULL, 0, param->offset, true);
		} else {
			str = new_bd_arm_Str(dbgi, new_block, incsp, new_value, new_mem,
								 mode, NULL, 0, param->offset, true);
		}
		sync_ins[sync_arity++] = str;
	}
	assert(in_arity <= max_inputs);

	/* construct memory input */
	if (sync_arity == 0) {
		in[mem_pos] = new_mem;
	} else if (sync_arity == 1) {
		in[mem_pos] = sync_ins[0];
	} else {
		in[mem_pos] = new_rd_Sync(NULL, new_block, sync_arity, sync_ins);
	}

	/* TODO: use a generic symconst matcher here */
	if (is_SymConst(callee)) {
		entity = get_SymConst_entity(callee);
	} else {
		/* TODO: finish load matcher here */
#if 0
		/* callee */
		if (is_Proj(callee) && is_Load(get_Proj_pred(callee))) {
			ir_node *load    = get_Proj_pred(callee);
			ir_node *ptr     = get_Load_ptr(load);
			ir_node *new_ptr = be_transform_node(ptr);
			ir_node *mem     = get_Load_mem(load);
			ir_node *new_mem = be_transform_node(mem);
			ir_mode *mode    = get_Load_mode(node);

		} else {
#endif
			in[in_arity]     = be_transform_node(callee);
			in_req[in_arity] = arm_reg_classes[CLASS_arm_gp].class_req;
			++in_arity;
		//}
	}

	/* outputs:
	 *  - memory
	 *  - caller saves
	 */
	out_arity = 1 + n_caller_saves;

	if (entity != NULL) {
		/* TODO: use a generic symconst matcher here
		 * so we can also handle entity+offset, etc. */
		res = new_bd_arm_Bl(dbgi, new_block, in_arity, in, out_arity,entity, 0);
	} else {
		/* TODO:
		 * - use a proper shifter_operand matcher
		 * - we could also use LinkLdrPC
		 */
		res = new_bd_arm_LinkMovPC(dbgi, new_block, in_arity, in, out_arity,
		                           ARM_SHF_REG, 0, 0);
	}

	if (incsp != NULL) {
		/* IncSP to destroy the call stackframe */
		incsp = be_new_IncSP(sp_reg, new_block, incsp, -cconv->param_stack_size,
		                     0);
		/* if we are the last IncSP producer in a block then we have to keep
		 * the stack value.
		 * Note: This here keeps all producers which is more than necessary */
		add_irn_dep(incsp, res);
		keep_alive(incsp);

		pmap_insert(node_to_stack, node, incsp);
	}

	set_arm_in_req_all(res, in_req);

	/* create output register reqs */
	arch_set_out_register_req(res, 0, arch_no_register_req);
	for (o = 0; o < n_caller_saves; ++o) {
		const arch_register_t *reg = caller_saves[o];
		arch_set_out_register_req(res, o+1, reg->single_req);
	}

	/* copy pinned attribute */
	set_irn_pinned(res, get_irn_pinned(node));

	arm_free_calling_convention(cconv);
	return res;
}

static ir_node *gen_Sel(ir_node *node)
{
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *block     = get_nodes_block(node);
	ir_node   *new_block = be_transform_node(block);
	ir_node   *ptr       = get_Sel_ptr(node);
	ir_node   *new_ptr   = be_transform_node(ptr);
	ir_entity *entity    = get_Sel_entity(node);

	/* must be the frame pointer all other sels must have been lowered
	 * already */
	assert(is_Proj(ptr) && is_Start(get_Proj_pred(ptr)));
	/* we should not have value types from parameters anymore - they should be
	   lowered */
	assert(get_entity_owner(entity) !=
			get_method_value_param_type(get_entity_type(get_irg_entity(get_irn_irg(node)))));

	return new_bd_arm_FrameAddr(dbgi, new_block, new_ptr, entity, 0);
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

	be_set_transform_function(op_Add,      gen_Add);
	be_set_transform_function(op_And,      gen_And);
	be_set_transform_function(op_Call,     gen_Call);
	be_set_transform_function(op_Cmp,      gen_Cmp);
	be_set_transform_function(op_Cond,     gen_Cond);
	be_set_transform_function(op_Const,    gen_Const);
	be_set_transform_function(op_Conv,     gen_Conv);
	be_set_transform_function(op_CopyB,    gen_CopyB);
	be_set_transform_function(op_Eor,      gen_Eor);
	be_set_transform_function(op_Jmp,      gen_Jmp);
	be_set_transform_function(op_Load,     gen_Load);
	be_set_transform_function(op_Minus,    gen_Minus);
	be_set_transform_function(op_Mul,      gen_Mul);
	be_set_transform_function(op_Not,      gen_Not);
	be_set_transform_function(op_Or,       gen_Or);
	be_set_transform_function(op_Phi,      gen_Phi);
	be_set_transform_function(op_Proj,     gen_Proj);
	be_set_transform_function(op_Quot,     gen_Quot);
	be_set_transform_function(op_Return,   gen_Return);
	be_set_transform_function(op_Rotl,     gen_Rotl);
	be_set_transform_function(op_Sel,      gen_Sel);
	be_set_transform_function(op_Shl,      gen_Shl);
	be_set_transform_function(op_Shr,      gen_Shr);
	be_set_transform_function(op_Shrs,     gen_Shrs);
	be_set_transform_function(op_Start,    gen_Start);
	be_set_transform_function(op_Store,    gen_Store);
	be_set_transform_function(op_Sub,      gen_Sub);
	be_set_transform_function(op_SymConst, gen_SymConst);
	be_set_transform_function(op_Unknown,  gen_Unknown);
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
	ir_graph  *irg             = cg->irg;
	ir_entity *entity          = get_irg_entity(irg);
	ir_type   *frame_type;

	mode_gp = mode_Iu;
	mode_fp = mode_E;

	if (! imm_initialized) {
		arm_init_fpa_immediate();
		imm_initialized = 1;
	}
	arm_register_transformers();
	env_cg = cg;

	node_to_stack = pmap_create();

	assert(abihelper == NULL);
	abihelper = be_abihelper_prepare(irg);
	be_collect_stacknodes(abihelper);
	assert(cconv == NULL);
	cconv = arm_decide_calling_convention(get_entity_type(entity));
	create_stacklayout(irg);

	be_transform_graph(cg->irg, NULL);

	be_abihelper_finish(abihelper);
	abihelper = NULL;

	arm_free_calling_convention(cconv);
	cconv = NULL;

	frame_type = get_irg_frame_type(irg);
	if (get_type_state(frame_type) == layout_undefined) {
		default_layout_compound_type(frame_type);
	}

	pmap_destroy(node_to_stack);
	node_to_stack = NULL;

	be_add_missing_keeps(irg);
}

void arm_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.arm.transform");
}
