/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   The codegenerator (transform FIRM into arm FIRM)
 * @author  Matthias Braun, Oliver Richter, Tobias Gneist, Michael Beck
 */
#include "bearch_arm_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "ircons.h"
#include "dbginfo.h"
#include "iropt_t.h"
#include "debug.h"
#include "error.h"
#include "util.h"

#include "benode.h"
#include "beirg.h"
#include "beutil.h"
#include "betranshlp.h"
#include "beabihelper.h"
#include "beabi.h"

#include "arm_nodes_attr.h"
#include "arm_transform.h"
#include "arm_optimize.h"
#include "arm_new_nodes.h"
#include "arm_map_regs.h"
#include "arm_cconv.h"

#include "gen_arm_regalloc_if.h"

#include <limits.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static const arch_register_t *sp_reg = &arm_registers[REG_SP];
static ir_mode               *mode_gp;
static ir_mode               *mode_fp;
static beabi_helper_env_t    *abihelper;
static be_stackorder_t       *stackorder;
static calling_convention_t  *cconv = NULL;
static arm_isa_t             *isa;

static pmap                  *node_to_stack;

static const arch_register_t *const callee_saves[] = {
	&arm_registers[REG_R4],
	&arm_registers[REG_R5],
	&arm_registers[REG_R6],
	&arm_registers[REG_R7],
	&arm_registers[REG_R8],
	&arm_registers[REG_R9],
	&arm_registers[REG_R10],
	&arm_registers[REG_R11],
	&arm_registers[REG_LR],
};

static const arch_register_t *const caller_saves[] = {
	&arm_registers[REG_R0],
	&arm_registers[REG_R1],
	&arm_registers[REG_R2],
	&arm_registers[REG_R3],
	&arm_registers[REG_LR],

	&arm_registers[REG_F0],
	&arm_registers[REG_F1],
	&arm_registers[REG_F2],
	&arm_registers[REG_F3],
	&arm_registers[REG_F4],
	&arm_registers[REG_F5],
	&arm_registers[REG_F6],
	&arm_registers[REG_F7],
};

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

		for (cnt = 1; cnt < vn.ops; ++cnt) {
			result = new_bd_arm_Bic_imm(dbgi, block, result,
			                            vn.values[cnt], vn.rors[cnt]);
		}
	} else {
		/* add bits */
		result = new_bd_arm_Mov_imm(dbgi, block, v.values[0], v.rors[0]);

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
	ir_tarval *tv   = get_Const_tarval(irn);
	ir_mode   *mode = get_tarval_mode(tv);
	unsigned   value;

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
		if (USE_FPA(isa)) {
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
		} else if (USE_VFP(isa)) {
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
	MATCH_REVERSE      = 1 << 1,  /**< support reverse opcode */
	MATCH_SIZE_NEUTRAL = 1 << 2,
	MATCH_SKIP_NOT     = 1 << 3,  /**< skip Not on ONE input */
} match_flags_t;
ENUM_BITSET(match_flags_t)

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
		new_op1 = be_transform_node(op1);
		return factory->new_binop_imm(dbgi, block, new_op1, imm.imm_8, imm.rot);
	}
	new_op2 = be_transform_node(op2);
    if ((flags & (MATCH_COMMUTATIVE|MATCH_REVERSE)) && try_encode_as_immediate(op1, &imm)) {
		if (flags & MATCH_REVERSE)
			return factory[1].new_binop_imm(dbgi, block, new_op2, imm.imm_8, imm.rot);
		else
			return factory[0].new_binop_imm(dbgi, block, new_op2, imm.imm_8, imm.rot);
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
		case ARM_SHF_REG:
		case ARM_SHF_RRX:
			break;
		case ARM_SHF_INVALID:
			panic("invalid shift");
		}
	}
	if ((flags & (MATCH_COMMUTATIVE|MATCH_REVERSE)) && is_arm_Mov(new_op1)) {
		const arm_shifter_operand_t *attr = get_arm_shifter_operand_attr_const(new_op1);
		int idx = flags & MATCH_REVERSE ? 1 : 0;

		switch (attr->shift_modifier) {
		ir_node *mov_op, *mov_sft;

		case ARM_SHF_IMM:
		case ARM_SHF_ASR_IMM:
		case ARM_SHF_LSL_IMM:
		case ARM_SHF_LSR_IMM:
		case ARM_SHF_ROR_IMM:
			if (factory[idx].new_binop_reg_shift_imm) {
				mov_op = get_irn_n(new_op1, 0);
				return factory[idx].new_binop_reg_shift_imm(dbgi, block, new_op2, mov_op,
					attr->shift_modifier, attr->shift_immediate);
			}
			break;

		case ARM_SHF_ASR_REG:
		case ARM_SHF_LSL_REG:
		case ARM_SHF_LSR_REG:
		case ARM_SHF_ROR_REG:
			if (factory[idx].new_binop_reg_shift_reg) {
				mov_op  = get_irn_n(new_op1, 0);
				mov_sft = get_irn_n(new_op1, 1);
				return factory[idx].new_binop_reg_shift_reg(dbgi, block, new_op2, mov_op, mov_sft,
					attr->shift_modifier);
			}
			break;

		case ARM_SHF_REG:
		case ARM_SHF_RRX:
			break;
		case ARM_SHF_INVALID:
			panic("invalid shift");
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
		if (USE_FPA(isa)) {
			return new_bd_arm_Adf(dbgi, block, new_op1, new_op2, mode);
		} else if (USE_VFP(isa)) {
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	} else {
		/* TODO: check for MLA */
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
		if (USE_FPA(isa)) {
			return new_bd_arm_Muf(dbg, block, new_op1, new_op2, mode);
		} else if (USE_VFP(isa)) {
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	}
	assert(mode_is_data(mode));
	return new_bd_arm_Mul(dbg, block, new_op1, new_op2);
}

static ir_node *gen_Div(ir_node *node)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_Div_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_Div_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	ir_mode  *mode    = get_Div_resmode(node);
	dbg_info *dbg     = get_irn_dbg_info(node);

	/* integer division should be replaced by builtin call */
	assert(mode_is_float(mode));

	if (USE_FPA(isa)) {
		return new_bd_arm_Dvf(dbg, block, new_op1, new_op2, mode);
	} else if (USE_VFP(isa)) {
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
	static const arm_binop_factory_t sub_rsb_factory[2] = {
		{
			new_bd_arm_Sub_reg,
			new_bd_arm_Sub_imm,
			new_bd_arm_Sub_reg_shift_reg,
			new_bd_arm_Sub_reg_shift_imm
		},
		{
			new_bd_arm_Rsb_reg,
			new_bd_arm_Rsb_imm,
			new_bd_arm_Rsb_reg_shift_reg,
			new_bd_arm_Rsb_reg_shift_imm
		}
	};

	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_Sub_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_Sub_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	ir_mode  *mode    = get_irn_mode(node);
	dbg_info *dbgi    = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		if (USE_FPA(isa)) {
			return new_bd_arm_Suf(dbgi, block, new_op1, new_op2, mode);
		} else if (USE_VFP(isa)) {
			panic("VFP not supported yet");
		} else {
			panic("Softfloat not supported yet");
		}
	} else {
		return gen_int_binop(node, MATCH_SIZE_NEUTRAL | MATCH_REVERSE, sub_rsb_factory);
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
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *new_op1;
	ir_node  *new_op2;

	if (get_mode_modulo_shift(mode) != 32)
		panic("modulo shift!=32 not supported");

	if (flags & MATCH_SIZE_NEUTRAL) {
		op1 = arm_skip_downconv(op1);
		op2 = arm_skip_downconv(op2);
	}

	new_op1 = be_transform_node(op1);
	if (is_Const(op2)) {
		ir_tarval   *tv  = get_Const_tarval(op2);
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
			ir_tarval *tv   = get_Const_tarval(right);
			ir_mode   *mode = get_irn_mode(node);
			long       bits = get_mode_size_bits(mode);
			ir_node   *left = get_Add_left(op2);

			if (is_Minus(left) &&
			    tarval_is_long(tv)          &&
			    get_tarval_long(tv) == bits &&
			    bits                == 32)
				rotate = gen_Ror(node, op1, get_Minus_op(left));
		}
	} else if (is_Sub(op2)) {
		ir_node *left = get_Sub_left(op2);
		if (is_Const(left)) {
			ir_tarval *tv   = get_Const_tarval(left);
			ir_mode   *mode = get_irn_mode(node);
			long       bits = get_mode_size_bits(mode);
			ir_node   *right = get_Sub_right(op2);

			if (tarval_is_long(tv)          &&
			    get_tarval_long(tv) == bits &&
			    bits                == 32)
				rotate = gen_Ror(node, op1, right);
		}
	} else if (is_Const(op2)) {
		ir_tarval *tv   = get_Const_tarval(op2);
		ir_mode   *mode = get_irn_mode(node);
		long       bits = get_mode_size_bits(mode);

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

		case ARM_SHF_REG:
		case ARM_SHF_RRX:
			break;
		case ARM_SHF_INVALID:
			panic("invalid shift");
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
		if (USE_FPA(isa)) {
			return new_bd_arm_Mvf(dbgi, block, op, mode);
		} else if (USE_VFP(isa)) {
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

	if (get_Load_unaligned(node) == align_non_aligned)
		panic("unaligned Loads not supported yet");

	if (mode_is_float(mode)) {
		if (USE_FPA(isa)) {
			new_load = new_bd_arm_Ldf(dbgi, block, new_ptr, new_mem, mode,
			                          NULL, 0, 0, false);
		} else if (USE_VFP(isa)) {
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

	if (get_Store_unaligned(node) == align_non_aligned)
		panic("unaligned Stores not supported yet");

	if (mode_is_float(mode)) {
		if (USE_FPA(isa)) {
			new_store = new_bd_arm_Stf(dbgi, block, new_ptr, new_val,
			                           new_mem, mode, NULL, 0, 0, false);
		} else if (USE_VFP(isa)) {
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

static ir_node *gen_Switch(ir_node *node)
{
	ir_graph              *irg      = get_irn_irg(node);
	ir_node               *block    = be_transform_node(get_nodes_block(node));
	ir_node               *selector = get_Switch_selector(node);
	dbg_info              *dbgi     = get_irn_dbg_info(node);
	ir_node               *new_op   = be_transform_node(selector);
	const ir_switch_table *table    = get_Switch_table(node);
	unsigned               n_outs   = get_Switch_n_outs(node);

	table = ir_switch_table_duplicate(irg, table);

#ifndef NDEBUG
	/* switch with smaller modes not implemented yet */
	ir_mode *mode = get_irn_mode(selector);
	assert(get_mode_size_bits(mode) == 32);
#endif

	return new_bd_arm_SwitchJmp(dbgi, block, new_op, n_outs, table);
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
	}

	assert(get_irn_mode(op2) == cmp_mode);
	is_unsigned = !mode_is_signed(cmp_mode);

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
	ir_node    *const block     = be_transform_node(get_nodes_block(node));
	dbg_info   *const dbgi      = get_irn_dbg_info(node);
	ir_node    *const selector  = get_Cond_selector(node);
	ir_node    *const flag_node = be_transform_node(selector);
	ir_relation const relation  = get_Cmp_relation(selector);
	return new_bd_arm_B(dbgi, block, flag_node, relation);
}

enum fpa_imm_mode {
	FPA_IMM_FLOAT    = 0,
	FPA_IMM_DOUBLE   = 1,
	FPA_IMM_MAX = FPA_IMM_DOUBLE
};

static ir_tarval *fpa_imm[FPA_IMM_MAX + 1][fpa_max];

static ir_node *gen_Const(ir_node *node)
{
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_mode *mode = get_irn_mode(node);
	dbg_info *dbg = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		if (USE_FPA(isa)) {
			ir_tarval *tv = get_Const_tarval(node);
			node          = new_bd_arm_fConst(dbg, block, tv);
			return node;
		} else if (USE_VFP(isa)) {
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
	return new_node;
}

static ir_node *ints_to_double(dbg_info *dbgi, ir_node *block, ir_node *node0,
                               ir_node *node1)
{
	/* the good way to do this would be to use the stm (store multiple)
	 * instructions, since our input is nearly always 2 consecutive 32bit
	 * registers... */
	ir_graph *irg   = get_Block_irg(block);
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
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

	return new_r_Proj(ldf, mode_fp, pn_arm_Ldf_res);
}

static ir_node *int_to_float(dbg_info *dbgi, ir_node *block, ir_node *node)
{
	ir_graph *irg   = get_Block_irg(block);
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *str   = new_bd_arm_Str(dbgi, block, stack, node, nomem, mode_gp,
	                                 NULL, 0, 0, true);
	ir_node  *ldf;
	set_irn_pinned(str, op_pin_state_floats);

	ldf = new_bd_arm_Ldf(dbgi, block, stack, str, mode_F, NULL, 0, 0, true);
	set_irn_pinned(ldf, op_pin_state_floats);

	return new_r_Proj(ldf, mode_fp, pn_arm_Ldf_res);
}

static ir_node *float_to_int(dbg_info *dbgi, ir_node *block, ir_node *node)
{
	ir_graph *irg   = get_Block_irg(block);
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *stf   = new_bd_arm_Stf(dbgi, block, stack, node, nomem, mode_F,
	                                 NULL, 0, 0, true);
	ir_node  *ldr;
	set_irn_pinned(stf, op_pin_state_floats);

	ldr = new_bd_arm_Ldr(dbgi, block, stack, stf, mode_gp, NULL, 0, 0, true);
	set_irn_pinned(ldr, op_pin_state_floats);

	return new_r_Proj(ldr, mode_gp, pn_arm_Ldr_res);
}

static void double_to_ints(dbg_info *dbgi, ir_node *block, ir_node *node,
                           ir_node **out_value0, ir_node **out_value1)
{
	ir_graph *irg   = get_Block_irg(block);
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *stf   = new_bd_arm_Stf(dbgi, block, stack, node, nomem, mode_D,
	                                 NULL, 0, 0, true);
	ir_node  *ldr0, *ldr1;
	set_irn_pinned(stf, op_pin_state_floats);

	ldr0 = new_bd_arm_Ldr(dbgi, block, stack, stf, mode_gp, NULL, 0, 0, true);
	set_irn_pinned(ldr0, op_pin_state_floats);
	ldr1 = new_bd_arm_Ldr(dbgi, block, stack, stf, mode_gp, NULL, 0, 4, true);
	set_irn_pinned(ldr1, op_pin_state_floats);

	*out_value0 = new_r_Proj(ldr0, mode_gp, pn_arm_Ldr_res);
	*out_value1 = new_r_Proj(ldr1, mode_gp, pn_arm_Ldr_res);
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

	src_copy = be_new_Copy(block, new_src);
	dst_copy = be_new_Copy(block, new_dst);

	return new_bd_arm_CopyB(dbg, block, dst_copy, src_copy,
			new_bd_arm_EmptyReg(dbg, block),
			new_bd_arm_EmptyReg(dbg, block),
			new_bd_arm_EmptyReg(dbg, block),
			new_mem, size);
}

/**
 * Transform builtin clz.
 */
static ir_node *gen_clz(ir_node *node)
{
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	dbg_info *dbg    = get_irn_dbg_info(node);
	ir_node  *op     = get_irn_n(node, 1);
	ir_node  *new_op = be_transform_node(op);

	/* TODO armv5 instruction, otherwise create a call */
	return new_bd_arm_Clz(dbg, block, new_op);
}

/**
 * Transform Builtin node.
 */
static ir_node *gen_Builtin(ir_node *node)
{
	ir_builtin_kind kind = get_Builtin_kind(node);

	switch (kind) {
	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_return_address:
	case ir_bk_frame_address:
	case ir_bk_prefetch:
	case ir_bk_ffs:
		break;
	case ir_bk_clz:
		return gen_clz(node);
	case ir_bk_ctz:
	case ir_bk_parity:
	case ir_bk_popcount:
	case ir_bk_bswap:
	case ir_bk_outport:
	case ir_bk_inport:
	case ir_bk_inner_trampoline:
	case ir_bk_saturating_increment:
	case ir_bk_compare_swap:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

/**
 * Transform Proj(Builtin) node.
 */
static ir_node *gen_Proj_Builtin(ir_node *proj)
{
	ir_node         *node     = get_Proj_pred(proj);
	ir_node         *new_node = be_transform_node(node);
	ir_builtin_kind kind      = get_Builtin_kind(node);

	switch (kind) {
	case ir_bk_return_address:
	case ir_bk_frame_address:
	case ir_bk_ffs:
	case ir_bk_clz:
	case ir_bk_ctz:
	case ir_bk_parity:
	case ir_bk_popcount:
	case ir_bk_bswap:
		assert(get_Proj_proj(proj) == pn_Builtin_max+1);
		return new_node;
	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_prefetch:
	case ir_bk_outport:
		assert(get_Proj_proj(proj) == pn_Builtin_M);
		return new_node;
	case ir_bk_inport:
	case ir_bk_inner_trampoline:
	case ir_bk_saturating_increment:
	case ir_bk_compare_swap:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
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

static ir_node *gen_Proj_Div(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_mode  *mode     = get_irn_mode(node);
	long     proj      = get_Proj_proj(node);

	switch (proj) {
	case pn_Div_M:
		return new_rd_Proj(dbgi, new_pred, mode_M, pn_arm_Dvf_M);
	case pn_Div_res:
		return new_rd_Proj(dbgi, new_pred, mode, pn_arm_Dvf_res);
	default:
		break;
	}
	panic("Unsupported Proj from Div");
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_node *block     = get_nodes_block(node);
	ir_node *new_block = be_transform_node(block);
	long     proj      = get_Proj_proj(node);

	switch ((pn_Start) proj) {
	case pn_Start_X_initial_exec:
		/* we exchange the ProjX with a jump */
		return new_bd_arm_Jmp(NULL, new_block);

	case pn_Start_M:
		return be_prolog_get_memory(abihelper);

	case pn_Start_T_args:
		return new_r_Bad(get_irn_irg(block), mode_T);

	case pn_Start_P_frame_base:
		return be_prolog_get_reg_value(abihelper, sp_reg);
	}
	panic("unexpected start proj: %ld\n", proj);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	long       pn          = get_Proj_proj(node);
	ir_node   *block       = get_nodes_block(node);
	ir_node   *new_block   = be_transform_node(block);
	ir_graph  *irg         = get_Block_irg(new_block);
	ir_entity *entity      = get_irg_entity(irg);
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
				ir_node *const fp  = get_irg_frame(irg);
				ir_node *const mem = be_prolog_get_memory(abihelper);
				ir_node *const ldr = new_bd_arm_Ldr(NULL, new_block, fp, mem, mode_gp, param->entity, 0, 0, true);
				value1 = new_r_Proj(ldr, mode_gp, pn_arm_Ldr_res);
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
		ir_node *const fp   = get_irg_frame(irg);
		ir_node *const mem  = be_prolog_get_memory(abihelper);
		ir_mode *const mode = get_type_mode(param->type);
		ir_node       *load;
		ir_node       *value;

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
	be_foreach_out(node, o) {
		const arch_register_req_t *req = arch_get_irn_register_req_out(node, o);
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
	calling_convention_t *cconv
		= arm_decide_calling_convention(NULL, function_type);
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
		break;
	}
	panic("Unexpected Call proj %ld\n", pn);
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node *pred = get_Proj_pred(node);
	long     pn   = get_Proj_proj(node);
	if (pn == pn_Store_M) {
		return be_transform_node(pred);
	} else {
		panic("Unsupported Proj from Store");
	}
}

static ir_node *gen_Proj_Proj(ir_node *node)
{
	ir_node *pred      = get_Proj_pred(node);
	ir_node *pred_pred = get_Proj_pred(pred);
	if (is_Call(pred_pred)) {
		return gen_Proj_Proj_Call(node);
	} else if (is_Start(pred_pred)) {
		return gen_Proj_Proj_Start(node);
	}
	panic("code selection didn't expect Proj(Proj) after %+F\n", pred_pred);
}

static ir_node *gen_Unknown(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	/* just produce a 0 */
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		ir_tarval *tv     = get_mode_null(mode);
		ir_node   *fconst = new_bd_arm_fConst(dbgi, new_block, tv);
		return fconst;
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
	layout->initial_offset = 0;
	layout->initial_bias   = 0;
	layout->sp_relative    = true;

	assert(N_FRAME_TYPES == 3);
	layout->order[0] = layout->frame_type;
	layout->order[1] = layout->between_type;
	layout->order[2] = layout->arg_type;
}

/**
 * transform the start node to the prolog code
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
	size_t     i;

	/* stackpointer is important at function prolog */
	be_prolog_add_reg(abihelper, sp_reg,
			arch_register_req_type_produces_sp | arch_register_req_type_ignore);
	/* function parameters in registers */
	for (i = 0; i < get_method_n_params(function_type); ++i) {
		const reg_or_stackslot_t *param = &cconv->parameters[i];
		if (param->reg0 != NULL)
			be_prolog_add_reg(abihelper, param->reg0, arch_register_req_type_none);
		if (param->reg1 != NULL)
			be_prolog_add_reg(abihelper, param->reg1, arch_register_req_type_none);
	}
	/* announce that we need the values of the callee save regs */
	for (i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		be_prolog_add_reg(abihelper, callee_saves[i], arch_register_req_type_none);
	}

	start = be_prolog_create_start(abihelper, dbgi, new_block);
	return start;
}

static ir_node *get_stack_pointer_for(ir_node *node)
{
	/* get predecessor in stack_order list */
	ir_node *stack_pred = be_get_stack_pred(stackorder, node);
	ir_node *stack;

	if (stack_pred == NULL) {
		/* first stack user in the current block. We can simply use the
		 * initial sp_proj for it */
		ir_node *sp_proj = be_prolog_get_reg_value(abihelper, sp_reg);
		return sp_proj;
	}

	be_transform_node(stack_pred);
	stack = pmap_get(ir_node, node_to_stack, stack_pred);
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
	size_t     n_callee_saves = ARRAY_SIZE(callee_saves);
	ir_node   *sp_proj        = get_stack_pointer_for(node);
	size_t     n_res          = get_Return_n_ress(node);
	ir_node   *bereturn;
	size_t     i;

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
		be_epilog_add_reg(abihelper, reg, arch_register_req_type_none, new_res_value);
	}

	/* connect callee saves with their values at the function begin */
	for (i = 0; i < n_callee_saves; ++i) {
		const arch_register_t *reg   = callee_saves[i];
		ir_node               *value = be_prolog_get_reg_value(abihelper, reg);
		be_epilog_add_reg(abihelper, reg, arch_register_req_type_none, value);
	}

	/* epilog code: an incsp */
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
	calling_convention_t *cconv        = arm_decide_calling_convention(NULL, type);
	size_t                n_params     = get_Call_n_params(node);
	size_t const          n_param_regs = cconv->n_reg_params;
	/* max inputs: memory, callee, register arguments */
	size_t const          max_inputs   = 2 + n_param_regs;
	ir_node             **in           = ALLOCAN(ir_node*, max_inputs);
	ir_node             **sync_ins     = ALLOCAN(ir_node*, max_inputs);
	struct obstack       *obst         = be_get_be_obst(irg);
	const arch_register_req_t **in_req
		= OALLOCNZ(obst, const arch_register_req_t*, max_inputs);
	size_t                in_arity       = 0;
	size_t                sync_arity     = 0;
	size_t const          n_caller_saves = ARRAY_SIZE(caller_saves);
	ir_entity            *entity         = NULL;
	ir_node              *incsp          = NULL;
	int                   mem_pos;
	ir_node              *res;
	size_t                p;
	size_t                o;
	size_t                out_arity;

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
		in[in_arity]     = be_transform_node(callee);
		in_req[in_arity] = arm_reg_classes[CLASS_arm_gp].class_req;
		++in_arity;
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

	arch_set_irn_register_reqs_in(res, in_req);

	/* create output register reqs */
	arch_set_irn_register_req_out(res, 0, arch_no_register_req);
	for (o = 0; o < n_caller_saves; ++o) {
		const arch_register_t *reg = caller_saves[o];
		arch_set_irn_register_req_out(res, o+1, reg->single_req);
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

	return new_bd_arm_FrameAddr(dbgi, new_block, new_ptr, entity, 0);
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (mode_needs_gp_reg(mode)) {
		/* we shouldn't have any 64bit stuff around anymore */
		assert(get_mode_size_bits(mode) <= 32);
		/* all integer operations are on 32bit registers now */
		req  = arm_reg_classes[CLASS_arm_gp].class_req;
	} else {
		req = arch_no_register_req;
	}

	return be_transform_phi(node, req);
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
	be_set_transform_function(op_Div,      gen_Div);
	be_set_transform_function(op_Eor,      gen_Eor);
	be_set_transform_function(op_Jmp,      gen_Jmp);
	be_set_transform_function(op_Load,     gen_Load);
	be_set_transform_function(op_Minus,    gen_Minus);
	be_set_transform_function(op_Mul,      gen_Mul);
	be_set_transform_function(op_Not,      gen_Not);
	be_set_transform_function(op_Or,       gen_Or);
	be_set_transform_function(op_Phi,      gen_Phi);
	be_set_transform_function(op_Return,   gen_Return);
	be_set_transform_function(op_Rotl,     gen_Rotl);
	be_set_transform_function(op_Sel,      gen_Sel);
	be_set_transform_function(op_Shl,      gen_Shl);
	be_set_transform_function(op_Shr,      gen_Shr);
	be_set_transform_function(op_Shrs,     gen_Shrs);
	be_set_transform_function(op_Start,    gen_Start);
	be_set_transform_function(op_Store,    gen_Store);
	be_set_transform_function(op_Sub,      gen_Sub);
	be_set_transform_function(op_Switch,   gen_Switch);
	be_set_transform_function(op_SymConst, gen_SymConst);
	be_set_transform_function(op_Unknown,  gen_Unknown);
	be_set_transform_function(op_Builtin,  gen_Builtin);

	be_set_transform_proj_function(op_Builtin, gen_Proj_Builtin);
	be_set_transform_proj_function(op_Call,    gen_Proj_Call);
	be_set_transform_proj_function(op_Cond,    be_duplicate_node);
	be_set_transform_proj_function(op_CopyB,   gen_Proj_CopyB);
	be_set_transform_proj_function(op_Div,     gen_Proj_Div);
	be_set_transform_proj_function(op_Load,    gen_Proj_Load);
	be_set_transform_proj_function(op_Proj,    gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,   gen_Proj_Start);
	be_set_transform_proj_function(op_Store,   gen_Proj_Store);
	be_set_transform_proj_function(op_Switch,  be_duplicate_node);
}

/**
 * Initialize fpa Immediate support.
 */
static void arm_init_fpa_immediate(void)
{
	/* 0, 1, 2, 3, 4, 5, 10, or 0.5. */
	fpa_imm[FPA_IMM_FLOAT][fpa_null]  = get_mode_null(mode_F);
	fpa_imm[FPA_IMM_FLOAT][fpa_one]   = get_mode_one(mode_F);
	fpa_imm[FPA_IMM_FLOAT][fpa_two]   = new_tarval_from_str("2", 1, mode_F);
	fpa_imm[FPA_IMM_FLOAT][fpa_three] = new_tarval_from_str("3", 1, mode_F);
	fpa_imm[FPA_IMM_FLOAT][fpa_four]  = new_tarval_from_str("4", 1, mode_F);
	fpa_imm[FPA_IMM_FLOAT][fpa_five]  = new_tarval_from_str("5", 1, mode_F);
	fpa_imm[FPA_IMM_FLOAT][fpa_ten]   = new_tarval_from_str("10", 2, mode_F);
	fpa_imm[FPA_IMM_FLOAT][fpa_half]  = new_tarval_from_str("0.5", 3, mode_F);

	fpa_imm[FPA_IMM_DOUBLE][fpa_null]  = get_mode_null(mode_D);
	fpa_imm[FPA_IMM_DOUBLE][fpa_one]   = get_mode_one(mode_D);
	fpa_imm[FPA_IMM_DOUBLE][fpa_two]   = new_tarval_from_str("2", 1, mode_D);
	fpa_imm[FPA_IMM_DOUBLE][fpa_three] = new_tarval_from_str("3", 1, mode_D);
	fpa_imm[FPA_IMM_DOUBLE][fpa_four]  = new_tarval_from_str("4", 1, mode_D);
	fpa_imm[FPA_IMM_DOUBLE][fpa_five]  = new_tarval_from_str("5", 1, mode_D);
	fpa_imm[FPA_IMM_DOUBLE][fpa_ten]   = new_tarval_from_str("10", 2, mode_D);
	fpa_imm[FPA_IMM_DOUBLE][fpa_half]  = new_tarval_from_str("0.5", 3, mode_D);
}

/**
 * Transform a Firm graph into an ARM graph.
 */
void arm_transform_graph(ir_graph *irg)
{
	static int imm_initialized = 0;
	ir_entity *entity          = get_irg_entity(irg);
	const arch_env_t *arch_env = be_get_irg_arch_env(irg);
	ir_type   *frame_type;

	mode_gp = mode_Iu;
	mode_fp = mode_F;

	if (! imm_initialized) {
		arm_init_fpa_immediate();
		imm_initialized = 1;
	}
	arm_register_transformers();

	isa = (arm_isa_t*) arch_env;

	node_to_stack = pmap_create();

	assert(abihelper == NULL);
	abihelper = be_abihelper_prepare(irg);
	stackorder = be_collect_stacknodes(irg);
	assert(cconv == NULL);
	cconv = arm_decide_calling_convention(irg, get_entity_type(entity));
	create_stacklayout(irg);

	be_transform_graph(irg, NULL);

	be_abihelper_finish(abihelper);
	abihelper = NULL;
	be_free_stackorder(stackorder);
	stackorder = NULL;

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
