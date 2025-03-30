/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   The codegenerator (transform FIRM into arm FIRM)
 * @author  Matthias Braun, Oliver Richter, Tobias Gneist, Michael Beck
 */
#include "arm_transform.h"

#include "arm_bearch_t.h"
#include "arm_cconv.h"
#include "arm_new_nodes.h"
#include "arm_nodes_attr.h"
#include "arm_optimize.h"
#include "beasm.h"
#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "beutil.h"
#include "dbginfo.h"
#include "debug.h"
#include "gen_arm_new_nodes.h"
#include "gen_arm_regalloc_if.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "panic.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static be_stack_env_t        stack_env;
static calling_convention_t *cconv = NULL;

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

static ir_node *get_initial_sp(ir_graph *const irg)
{
	return be_get_Start_proj(irg, &arm_registers[REG_SP]);
}

void arm_gen_vals_from_word(uint32_t value, arm_vals *result)
{
	/* TODO: not optimal yet, as we only "shift" the value and don't take
	 * advantage of rotations */

	/* special case: we prefer shift amount 0 */
	if (value <= 0xFF) {
		result->values[0] = value;
		result->rors[0]   = 0;
		result->ops       = 1;
		return;
	}

	int initial = 0;
	result->ops = 0;
	do {
		while ((value & 0x3) == 0) {
			value  >>= 2;
			initial += 2;
		}

		result->values[result->ops] = value & 0xFF;
		result->rors[result->ops]   = (32-initial) % 32;
		++result->ops;

		value  >>= 8;
		initial += 8;
	} while (value != 0);
}

/**
 * create firm graph for a constant
 */
static ir_node *create_const_graph_value(dbg_info *dbgi, ir_node *block,
                                         uint32_t value)
{
	/* We only have 8 bit immediates. So we possibly have to combine several
	 * operations to construct the desired value.
	 *
	 * we can either create the value by adding bits to 0 or by removing bits
	 * from an register with all bits set. Try which alternative needs fewer
	 * operations */
	arm_vals v;
	arm_gen_vals_from_word(value, &v);
	arm_vals vn;
	arm_gen_vals_from_word(~value, &vn);

	ir_node *result;
	if (vn.ops < v.ops) {
		/* remove bits */
		result = new_bd_arm_Mvn_imm(dbgi, block, vn.values[0], vn.rors[0]);

		for (unsigned cnt = 1; cnt < vn.ops; ++cnt) {
			result = new_bd_arm_Bic_imm(dbgi, block, result,
			                            vn.values[cnt], vn.rors[cnt]);
		}
	} else {
		/* add bits */
		result = new_bd_arm_Mov_imm(dbgi, block, v.values[0], v.rors[0]);

		for (unsigned cnt = 1; cnt < v.ops; ++cnt) {
			result = new_bd_arm_Orr_imm(dbgi, block, result, v.values[cnt], v.rors[cnt]);
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
	if (mode_is_reference(mode)) {
		/* ARM is 32bit, so we can safely convert a reference tarval into Iu */
		assert(get_mode_size_bits(mode) == get_mode_size_bits(arm_mode_gp));
		tv = tarval_convert_to(tv, arm_mode_gp);
	}
	long value = get_tarval_long(tv);
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
                                   unsigned src_bits)
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
                                   unsigned src_bits)
{
	unsigned shift_width = 32 - src_bits;
	ir_node *lshift_node = new_bd_arm_Mov_reg_shift_imm(dbgi, block, op, ARM_SHF_LSL_IMM, shift_width);
	ir_node *rshift_node = new_bd_arm_Mov_reg_shift_imm(dbgi, block, lshift_node, ARM_SHF_ASR_IMM, shift_width);
	return rshift_node;
}

static ir_node *gen_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                              ir_mode *orig_mode)
{
	unsigned bits = get_mode_size_bits(orig_mode);
	if (bits == 32)
		return op;

	if (mode_is_signed(orig_mode)) {
		return gen_sign_extension(dbgi, block, op, bits);
	} else {
		return gen_zero_extension(dbgi, block, op, bits);
	}
}

/**
 * Transforms a Conv node.
 *
 * @return The created ia32 Conv node
 */
static ir_node *gen_Conv(ir_node *node)
{
	ir_node  *block    = be_transform_nodes_block(node);
	ir_node  *op       = get_Conv_op(node);
	ir_node  *new_op   = be_transform_node(op);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);
	dbg_info *dbg      = get_irn_dbg_info(node);

	if (src_mode == dst_mode)
		return new_op;

	if (mode_is_float(src_mode) || mode_is_float(dst_mode)) {
		if (arm_cg_config.fpu == ARM_FPU_FPA) {
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
					return new_bd_arm_Flt(dbg, block, new_op, dst_mode);
				}
			}
		} else {
			panic("softfloat not lowered");
		}
	} else { /* complete in gp registers */
		unsigned src_bits = get_mode_size_bits(src_mode);
		unsigned dst_bits = get_mode_size_bits(dst_mode);
		if (src_bits == dst_bits) {
			/* kill unnecessary conv */
			return new_op;
		}

		unsigned min_bits;
		ir_mode *min_mode;
		if (src_bits < dst_bits) {
			min_bits = src_bits;
			min_mode = src_mode;
		} else {
			min_bits = dst_bits;
			min_mode = dst_mode;
		}

		if (be_upper_bits_clean(op, min_mode))
			return new_op;

		if (mode_is_signed(min_mode)) {
			return gen_sign_extension(dbg, block, new_op, min_bits);
		} else {
			return gen_zero_extension(dbg, block, new_op, min_bits);
		}
	}
}

typedef struct {
	uint8_t imm_8;
	uint8_t rot;
} arm_immediate_t;

static bool try_encode_val_as_immediate(uint32_t val, arm_immediate_t *const res)
{
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
	unsigned low_pos  = ntz(val) & ~1u;
	unsigned high_pos = (32-nlz(val)+1) & ~1u;

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

typedef enum imm_match_t {
	IMM_NONE = 0,
	IMM_POS  = 1 << 0,
	IMM_NOT  = 1 << 1,
	IMM_NEG  = 1 << 2,
} imm_match_t;

static imm_match_t try_encode_as_immediate(ir_node const *const node, arm_immediate_t *const res, imm_match_t const match)
{
	if (!is_Const(node))
		return IMM_NONE;
	uint32_t const val = get_Const_long(node);
	return
		match & IMM_POS && try_encode_val_as_immediate( val, res) ? IMM_POS :
		match & IMM_NOT && try_encode_val_as_immediate(~val, res) ? IMM_NOT :
		match & IMM_NEG && try_encode_val_as_immediate(-val, res) ? IMM_NEG :
		IMM_NONE;
}

typedef enum {
	MATCH_NONE         = 0,
	MATCH_COMMUTATIVE  = 1 << 0,  /**< commutative node */
	MATCH_REVERSE      = 1 << 1,  /**< support reverse opcode */
	MATCH_SIZE_NEUTRAL = 1 << 2,
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

static ir_node *gen_int_binop_ops(ir_node *node, ir_node *op1, ir_node *op2,
                                  match_flags_t flags,
                                  const arm_binop_factory_t *factory)
{
	ir_node  *block = be_transform_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);

	if (flags & MATCH_SIZE_NEUTRAL) {
		op1 = be_skip_downconv(op1, true);
		op2 = be_skip_downconv(op2, true);
	} else {
		assert(get_mode_size_bits(get_irn_mode(node)) == 32);
		op1 = be_skip_sameconv(op1);
		op2 = be_skip_sameconv(op2);
	}

	arm_immediate_t imm;
	if (try_encode_as_immediate(op2, &imm, IMM_POS) != IMM_NONE) {
		ir_node *new_op1 = be_transform_node(op1);
		return factory->new_binop_imm(dbgi, block, new_op1, imm.imm_8, imm.rot);
	}
	ir_node *new_op2 = be_transform_node(op2);
    if ((flags & (MATCH_COMMUTATIVE|MATCH_REVERSE)) && try_encode_as_immediate(op1, &imm, IMM_POS) != IMM_NONE) {
		if (flags & MATCH_REVERSE)
			return factory[1].new_binop_imm(dbgi, block, new_op2, imm.imm_8, imm.rot);
		else
			return factory[0].new_binop_imm(dbgi, block, new_op2, imm.imm_8, imm.rot);
	}
	ir_node *new_op1 = be_transform_node(op1);

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
				ir_node *mov_op = get_irn_n(new_op2, n_arm_Mov_Rm);
				return factory->new_binop_reg_shift_imm(dbgi, block, new_op1, mov_op,
					attr->shift_modifier, attr->shift_immediate);
			}
			break;

		case ARM_SHF_ASR_REG:
		case ARM_SHF_LSL_REG:
		case ARM_SHF_LSR_REG:
		case ARM_SHF_ROR_REG:
			if (factory->new_binop_reg_shift_reg) {
				ir_node *mov_op  = get_irn_n(new_op2, n_arm_Mov_Rm);
				ir_node *mov_sft = get_irn_n(new_op2, n_arm_Mov_Rs);
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
		case ARM_SHF_IMM:
		case ARM_SHF_ASR_IMM:
		case ARM_SHF_LSL_IMM:
		case ARM_SHF_LSR_IMM:
		case ARM_SHF_ROR_IMM:
			if (factory[idx].new_binop_reg_shift_imm) {
				ir_node *mov_op = get_irn_n(new_op1, n_arm_Mov_Rm);
				return factory[idx].new_binop_reg_shift_imm(dbgi, block, new_op2, mov_op,
					attr->shift_modifier, attr->shift_immediate);
			}
			break;

		case ARM_SHF_ASR_REG:
		case ARM_SHF_LSL_REG:
		case ARM_SHF_LSR_REG:
		case ARM_SHF_ROR_REG:
			if (factory[idx].new_binop_reg_shift_reg) {
				ir_node *mov_op  = get_irn_n(new_op1, n_arm_Mov_Rm);
				ir_node *mov_sft = get_irn_n(new_op1, n_arm_Mov_Rs);
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

static ir_node *gen_int_binop(ir_node *node, match_flags_t flags,
                              const arm_binop_factory_t *factory)
{
	ir_node *op1 = get_binop_left(node);
	ir_node *op2 = get_binop_right(node);
	return gen_int_binop_ops(node, op1, op2, flags, factory);
}

static ir_node *gen_Ror(ir_node *node, ir_node *op1, ir_node *op2,
                        bool negate_op)
{
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_nodes_block(node);
	ir_node  *new_op1 = be_transform_node(op1);
	if (is_Const(op2)) {
		ir_tarval *tv   = get_Const_tarval(op2);
		ir_mode   *mode = get_irn_mode(node);
		long       bits = get_mode_size_bits(mode);
		if (tarval_is_long(tv) && bits == 32) {
			long val = get_tarval_long(tv);
			val = (negate_op ? bits - val : val) & 31;
			return new_bd_arm_Mov_reg_shift_imm(dbgi, block, new_op1, ARM_SHF_ROR_IMM, val);
		}
	}

	ir_node *new_op2 = be_transform_node(op2);
	if (negate_op) {
		new_op2 = new_bd_arm_Rsb_imm(dbgi, block, new_op2, 32, 0);
	}
	return new_bd_arm_Mov_reg_shift_reg(dbgi, block, new_op1, new_op2,
	                                    ARM_SHF_ROR_REG);
}

static bool is_low_mask(ir_tarval *tv)
{
	return get_tarval_popcount(tv) == 16 && get_tarval_highest_bit(tv) == 15;
}

static bool is_high_mask(ir_tarval *tv)
{
	return get_tarval_popcount(tv) == 16 && get_tarval_lowest_bit(tv) == 16;
}

static ir_node *match_pkh(ir_node *node)
{
	assert(is_Or(node) || is_Add(node));
	ir_node *left  = get_binop_left(node);
	ir_node *right = get_binop_right(node);
	if (!is_And(left) || !is_And(right))
		return NULL;
	ir_node *left_right  = get_And_right(left);
	ir_node *right_right = get_And_right(right);
	if (!is_Const(left_right) || !is_Const(right_right))
		return NULL;
	/* we want the low-mask on the right side */
	if (is_high_mask(get_Const_tarval(left_right))) {
		ir_node *tmp = left;
		left       = right;
		right      = tmp;
		left_right = right_right;
	} else if (!is_high_mask(get_Const_tarval(right_right))) {
		return NULL;
	}
	if (!is_low_mask(get_Const_tarval(left_right)))
		return NULL;
	ir_node *left_left  = get_And_left(left);
	ir_node *right_left = get_And_left(right);
	static const arm_binop_factory_t pkhbt_pkhtb_factory[2] = {
		{
			new_bd_arm_Pkhbt_reg,
			new_bd_arm_Pkhbt_imm,
			new_bd_arm_Pkhbt_reg_shift_reg,
			new_bd_arm_Pkhbt_reg_shift_imm
		},
		{
			new_bd_arm_Pkhtb_reg,
			new_bd_arm_Pkhtb_imm,
			new_bd_arm_Pkhtb_reg_shift_reg,
			new_bd_arm_Pkhtb_reg_shift_imm
		}
	};
	return gen_int_binop_ops(node, left_left, right_left, MATCH_REVERSE,
	                         pkhbt_pkhtb_factory);
}

/**
 * Creates an ARM Add.
 *
 * @return the created arm Add node
 */
static ir_node *gen_Add(ir_node *node)
{
	ir_node *rotl_left;
	ir_node *rotl_right;
	if (be_pattern_is_rotl(node, &rotl_left, &rotl_right)) {
		if (is_Minus(rotl_right))
			return gen_Ror(node, rotl_left, get_Minus_op(rotl_right), false);
		return gen_Ror(node, rotl_left, rotl_right, true);
	}
	ir_node *pkh = match_pkh(node);
	if (pkh != NULL)
		return pkh;

	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		ir_node  *block   = be_transform_nodes_block(node);
		ir_node  *op1     = get_Add_left(node);
		ir_node  *op2     = get_Add_right(node);
		dbg_info *dbgi    = get_irn_dbg_info(node);
		ir_node  *new_op1 = be_transform_node(op1);
		ir_node  *new_op2 = be_transform_node(op2);
		if (arm_cg_config.fpu == ARM_FPU_FPA) {
			return new_bd_arm_Adf(dbgi, block, new_op1, new_op2, mode);
		} else {
			panic("softfloat not lowered");
		}
	} else {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);
		ir_node *mul_left;
		ir_node *mul_right;
		ir_node *other;
		if (is_Mul(left)) {
			mul_left  = get_Mul_left(left);
			mul_right = get_Mul_right(left);
			other     = right;
			goto create_mla;
		} else if (is_Mul(right)) {
			mul_left  = get_Mul_left(right);
			mul_right = get_Mul_right(right);
			other     = left;
create_mla:;
			dbg_info *dbgi      = get_irn_dbg_info(node);
			ir_node  *block     = be_transform_nodes_block(node);
			ir_node  *new_left  = be_transform_node(mul_left);
			ir_node  *new_right = be_transform_node(mul_right);
			ir_node  *new_add   = be_transform_node(other);
			if (arm_cg_config.variant < ARM_VARIANT_6)
				return new_bd_arm_Mla_v5(dbgi, block, new_left, new_right, new_add);
			else
				return new_bd_arm_Mla(dbgi, block, new_left, new_right,
				                      new_add);
		}

		arm_immediate_t imm;
		if (try_encode_as_immediate(right, &imm, IMM_NEG) != IMM_NONE) {
			dbg_info *const dbgi     = get_irn_dbg_info(node);
			ir_node  *const block    = be_transform_nodes_block(node);
			ir_node  *const new_left = be_transform_node(left);
			return new_bd_arm_Sub_imm(dbgi, block, new_left, imm.imm_8, imm.rot);
		}

		static const arm_binop_factory_t add_factory = {
			new_bd_arm_Add_reg,
			new_bd_arm_Add_imm,
			new_bd_arm_Add_reg_shift_reg,
			new_bd_arm_Add_reg_shift_imm
		};
		return gen_int_binop_ops(node, left, right,
		                         MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL,
		                         &add_factory);
	}
}

static ir_node *gen_arm_AddS_t(ir_node *node)
{
	static const arm_binop_factory_t adds_factory = {
		new_bd_arm_AddS_reg,
		new_bd_arm_AddS_imm,
		new_bd_arm_AddS_reg_shift_reg,
		new_bd_arm_AddS_reg_shift_imm,
	};
	ir_node *left  = get_irn_n(node, n_arm_AddS_t_left);
	ir_node *right = get_irn_n(node, n_arm_AddS_t_right);
	ir_node *res   = gen_int_binop_ops(node, left, right,
	                                   MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL,
	                                   &adds_factory);
	arch_set_irn_register_out(res, pn_arm_AddS_flags, &arm_registers[REG_FL]);
	return res;
}

static ir_node *gen_Proj_arm_AddS_t(ir_node *node)
{
	unsigned pn       = get_Proj_num(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	switch ((pn_arm_AddS_t)pn) {
	case pn_arm_AddS_t_res:
		return be_new_Proj(new_pred, pn_arm_AddS_res);
	case pn_arm_AddS_t_flags:
		return be_new_Proj(new_pred, pn_arm_AddS_flags);
	}
	panic("%+F: Invalid proj number", node);
}

static ir_node *gen_arm_AdC_t(ir_node *node)
{
	ir_node *left  = get_irn_n(node, n_arm_AdC_t_left);
	ir_node *right = get_irn_n(node, n_arm_AdC_t_right);
	ir_node *flags = get_irn_n(node, n_arm_AdC_t_flags);
	/* TODO: handle complete set of shifter operands */
	ir_node *new_left  = be_transform_node(left);
	ir_node *new_right = be_transform_node(right);
	ir_node *new_flags = be_transform_node(flags);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *res       = new_bd_arm_AdC_reg(dbgi, new_block, new_left,
	                                         new_right, new_flags);
	return res;
}

/**
 * Creates an ARM Mul.
 *
 * @return the created arm Mul node
 */
static ir_node *gen_Mul(ir_node *node)
{
	ir_node  *block   = be_transform_nodes_block(node);
	ir_node  *op1     = get_Mul_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_Mul_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	ir_mode  *mode    = get_irn_mode(node);
	dbg_info *dbg     = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		if (arm_cg_config.fpu == ARM_FPU_FPA) {
			return new_bd_arm_Muf(dbg, block, new_op1, new_op2, mode);
		} else {
			panic("softfloat not lowered");
		}
	}
	if (arm_cg_config.variant < ARM_VARIANT_6) {
		return new_bd_arm_Mul_v5(dbg, block, new_op1, new_op2);
	} else {
		return new_bd_arm_Mul(dbg, block, new_op1, new_op2);
	}
}

static ir_node *gen_arm_UMulL_t(ir_node *node)
{
	ir_node  *block     = be_transform_nodes_block(node);
	ir_node  *left      = get_irn_n(node, n_arm_UMulL_t_left);
	ir_node  *new_left  = be_transform_node(left);
	ir_node  *right     = get_irn_n(node, n_arm_UMulL_t_right);
	ir_node  *new_right = be_transform_node(right);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	return new_bd_arm_UMulL(dbgi, block, new_left, new_right);
}

static ir_node *gen_Proj_arm_UMulL_t(ir_node *node)
{
	unsigned pn       = get_Proj_num(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	switch ((pn_arm_UMulL_t)pn) {
	case pn_arm_UMulL_t_low:
		return be_new_Proj(new_pred, pn_arm_UMulL_low);
	case pn_arm_UMulL_t_high:
		return be_new_Proj(new_pred, pn_arm_UMulL_high);
	}
	panic("%+F: Invalid proj number", node);
}

static ir_node *gen_Div(ir_node *node)
{
	ir_node  *block   = be_transform_nodes_block(node);
	ir_node  *op1     = get_Div_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_Div_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	ir_mode  *mode    = get_Div_resmode(node);
	dbg_info *dbg     = get_irn_dbg_info(node);

	/* integer division should be replaced by builtin call */
	assert(mode_is_float(mode));

	if (arm_cg_config.fpu == ARM_FPU_FPA) {
		return new_bd_arm_Dvf(dbg, block, new_op1, new_op2, mode);
	} else {
		panic("softfloat not lowered");
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
	arm_immediate_t imm;
	ir_node *left  = get_And_left(node);
	ir_node *right = get_And_right(node);
	if (is_Not(right)) {
		ir_node *right_not = get_Not_op(right);
		return gen_int_binop_ops(node, left, right_not, MATCH_SIZE_NEUTRAL,
		                         &bic_factory);
	} else if (is_Not(left)) {
		ir_node *left_not = get_Not_op(left);
		return gen_int_binop_ops(node, right, left_not, MATCH_SIZE_NEUTRAL,
		                         &bic_factory);
	} else if (try_encode_as_immediate(right, &imm, IMM_NOT) != IMM_NONE) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const new_l = be_transform_node(left);
		return new_bd_arm_Bic_imm(dbgi, block, new_l, imm.imm_8, imm.rot);
	} else {
		return gen_int_binop(node, MATCH_COMMUTATIVE|MATCH_SIZE_NEUTRAL,
		                     &and_factory);
	}
}

static ir_node *gen_Or(ir_node *node)
{
	ir_node *rotl_left;
	ir_node *rotl_right;
	if (be_pattern_is_rotl(node, &rotl_left, &rotl_right)) {
		if (is_Minus(rotl_right))
			return gen_Ror(node, rotl_left, get_Minus_op(rotl_right), false);
		return gen_Ror(node, rotl_left, rotl_right, true);
	}
	ir_node *pkh = match_pkh(node);
	if (pkh != NULL)
		return pkh;

	static const arm_binop_factory_t or_factory = {
		new_bd_arm_Orr_reg,
		new_bd_arm_Orr_imm,
		new_bd_arm_Orr_reg_shift_reg,
		new_bd_arm_Orr_reg_shift_imm
	};
	return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL, &or_factory);
}

static ir_node *gen_arm_OrPl_t(ir_node *node)
{
	ir_node *left     = get_irn_n(node, n_arm_OrPl_t_left);
	ir_node *right    = get_irn_n(node, n_arm_OrPl_t_right);
	ir_node *falseval = get_irn_n(node, n_arm_OrPl_t_falseval);
	ir_node *flags    = get_irn_n(node, n_arm_OrPl_t_flags);
	/* TODO: handle complete set of shifter operands */
	ir_node *new_left     = be_transform_node(left);
	ir_node *new_right    = be_transform_node(right);
	ir_node *new_falseval = be_transform_node(falseval);
	ir_node *new_flags    = be_transform_node(flags);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *res       = new_bd_arm_OrrPl(dbgi, new_block, new_left, new_right, new_falseval, new_flags);
	return res;
}

static ir_node *gen_Eor(ir_node *node)
{
	static const arm_binop_factory_t eor_factory = {
		new_bd_arm_Eor_reg,
		new_bd_arm_Eor_imm,
		new_bd_arm_Eor_reg_shift_reg,
		new_bd_arm_Eor_reg_shift_imm
	};
	return gen_int_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL,
	                     &eor_factory);
}

static ir_node *gen_Sub(ir_node *node)
{
	ir_mode *mode  = get_irn_mode(node);
	ir_node *left  = get_Sub_left(node);
	ir_node *right = get_Sub_right(node);
	if (mode_is_float(mode)) {
		ir_node  *block     = be_transform_nodes_block(node);
		ir_node  *new_left  = be_transform_node(left);
		ir_node  *new_right = be_transform_node(right);
		dbg_info *dbgi      = get_irn_dbg_info(node);

		if (arm_cg_config.fpu == ARM_FPU_FPA) {
			return new_bd_arm_Suf(dbgi, block, new_left, new_right, mode);
		} else {
			panic("softfloat not lowered");
		}
	} else {
		if (is_Mul(right) && arm_cg_config.variant >= ARM_VARIANT_6T2) {
			dbg_info *dbgi      = get_irn_dbg_info(node);
			ir_node  *block     = be_transform_nodes_block(node);
			ir_node  *mul_left  = get_Mul_left(right);
			ir_node  *mul_right = get_Mul_right(right);
			ir_node  *new_left  = be_transform_node(mul_left);
			ir_node  *new_right = be_transform_node(mul_right);
			ir_node  *new_sub   = be_transform_node(left);
			return new_bd_arm_Mls(dbgi, block, new_left, new_right, new_sub);
		}

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
		return gen_int_binop(node, MATCH_SIZE_NEUTRAL | MATCH_REVERSE, sub_rsb_factory);
	}
}

static ir_node *gen_arm_SubS_t(ir_node *node)
{
	static const arm_binop_factory_t subs_factory[2] = {
		{
			new_bd_arm_SubS_reg,
			new_bd_arm_SubS_imm,
			new_bd_arm_SubS_reg_shift_reg,
			new_bd_arm_SubS_reg_shift_imm,
		},
		{
			new_bd_arm_RsbS_reg,
			new_bd_arm_RsbS_imm,
			new_bd_arm_RsbS_reg_shift_reg,
			new_bd_arm_RsbS_reg_shift_imm,
		},
	};
	ir_node *left  = get_irn_n(node, n_arm_SubS_t_left);
	ir_node *right = get_irn_n(node, n_arm_SubS_t_right);
	ir_node *res   = gen_int_binop_ops(node, left, right,
	                                   MATCH_SIZE_NEUTRAL | MATCH_REVERSE,
	                                   subs_factory);
	assert((int)pn_arm_SubS_flags == (int)pn_arm_RsbS_flags);
	arch_set_irn_register_out(res, pn_arm_SubS_flags, &arm_registers[REG_FL]);
	return res;
}

static ir_node *gen_Proj_arm_SubS_t(ir_node *node)
{
	unsigned pn       = get_Proj_num(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	assert((int)pn_arm_SubS_flags == (int)pn_arm_RsbS_flags);
	assert((int)pn_arm_SubS_res == (int)pn_arm_RsbS_res);
	switch ((pn_arm_SubS_t)pn) {
	case pn_arm_SubS_t_res:
		return be_new_Proj(new_pred, pn_arm_SubS_res);
	case pn_arm_SubS_t_flags:
		return be_new_Proj(new_pred, pn_arm_SubS_flags);
	}
	panic("%+F: Invalid proj number", node);
}

static ir_node *gen_arm_SbC_t(ir_node *node)
{
	ir_node *left  = get_irn_n(node, n_arm_SbC_t_left);
	ir_node *right = get_irn_n(node, n_arm_SbC_t_right);
	ir_node *flags = get_irn_n(node, n_arm_SbC_t_flags);
	/* TODO: handle complete set of shifter operands */
	ir_node *new_left  = be_transform_node(left);
	ir_node *new_right = be_transform_node(right);
	ir_node *new_flags = be_transform_node(flags);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *res       = new_bd_arm_SbC_reg(dbgi, new_block, new_left,
	                                         new_right, new_flags);
	return res;
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
	ir_node  *block = be_transform_nodes_block(node);
	ir_node  *op1   = get_binop_left(node);
	ir_node  *op2   = get_binop_right(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	if (get_mode_modulo_shift(mode) != 256)
		panic("modulo shift!=256 not supported");

	if (flags & MATCH_SIZE_NEUTRAL) {
		op1 = be_skip_downconv(op1, true);
		op2 = be_skip_downconv(op2, true);
	}

	ir_node *new_op1 = be_transform_node(op1);
	if (is_Const(op2)) {
		unsigned int const val = get_Const_long(op2);
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

	ir_node *new_op2 = be_transform_node(op2);
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

static ir_node *gen_Not(ir_node *node)
{
	ir_node  *block  = be_transform_nodes_block(node);
	ir_node  *op     = get_Not_op(node);
	ir_node  *new_op = be_transform_node(op);
	dbg_info *dbgi   = get_irn_dbg_info(node);

	/* check if we can fold in a Mov */
	if (is_arm_Mov(new_op)) {
		const arm_shifter_operand_t *attr = get_arm_shifter_operand_attr_const(new_op);

		switch (attr->shift_modifier) {
		case ARM_SHF_IMM:
		case ARM_SHF_ASR_IMM:
		case ARM_SHF_LSL_IMM:
		case ARM_SHF_LSR_IMM:
		case ARM_SHF_ROR_IMM: {
			ir_node *mov_op = get_irn_n(new_op, n_arm_Mov_Rm);
			return new_bd_arm_Mvn_reg_shift_imm(dbgi, block, mov_op,
				attr->shift_modifier, attr->shift_immediate);
		}

		case ARM_SHF_ASR_REG:
		case ARM_SHF_LSL_REG:
		case ARM_SHF_LSR_REG:
		case ARM_SHF_ROR_REG: {
			ir_node *mov_op  = get_irn_n(new_op, n_arm_Mov_Rm);
			ir_node *mov_sft = get_irn_n(new_op, n_arm_Mov_Rs);
			return new_bd_arm_Mvn_reg_shift_reg(dbgi, block, mov_op, mov_sft,
				attr->shift_modifier);
		}

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
	ir_node  *block  = be_transform_nodes_block(node);
	ir_node  *op     = get_Minus_op(node);
	ir_node  *new_op = be_transform_node(op);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_mode  *mode   = get_irn_mode(node);

	if (mode_is_float(mode)) {
		if (arm_cg_config.fpu == ARM_FPU_FPA) {
			return new_bd_arm_Mvf(dbgi, block, op, mode);
		} else {
			panic("softfloat not lowered");
		}
	}
	return new_bd_arm_Rsb_imm(dbgi, block, new_op, 0, 0);
}

typedef struct arm_am_t {
	ir_node   *base;
	ir_entity *entity;
	long       offset;
	bool       is_frame_entity;
} arm_am_t;

static arm_am_t transform_am(ir_node *addr, ir_mode *const mode, bool const is_store)
{
	long offset = 0;
	if (is_Add(addr)) {
		ir_node *const r = get_Add_right(addr);
		if (is_Const(r)) {
			long const c = get_Const_long(r);
			if (arm_is_valid_offset(c, mode, is_store)) {
				offset = c;
				addr   = get_Add_left(addr);
			}
		}
	}

	ir_entity *entity = NULL;
	if (is_Member(addr)) {
		entity = get_Member_entity(addr);
		addr   = get_Member_ptr(addr);
		/* Must be the frame pointer. All other sels must have been lowered
		 * already. */
		assert(is_Proj(addr) && is_Start(get_Proj_pred(addr)));
	}

	ir_node *const base = be_transform_node(addr);

	return (arm_am_t){
		.base            = base,
		.entity          = entity,
		.offset          = offset,
		.is_frame_entity = entity != NULL,
	};
}

static ir_node *gen_Load(ir_node *node)
{
	ir_node  *block   = be_transform_nodes_block(node);
	ir_node  *ptr     = get_Load_ptr(node);
	ir_mode  *mode    = get_Load_mode(node);
	arm_am_t  am      = transform_am(ptr, mode, false);
	ir_node  *mem     = get_Load_mem(node);
	ir_node  *new_mem = be_transform_node(mem);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	if (get_Load_unaligned(node) == align_non_aligned)
		panic("unaligned Loads not supported yet");

	ir_node *new_load;
	if (mode_is_float(mode)) {
		if (arm_cg_config.fpu == ARM_FPU_FPA) {
			new_load = new_bd_arm_Ldf(dbgi, block, am.base, new_mem, mode, am.entity, 0, am.offset, am.is_frame_entity);
		} else {
			panic("softfloat not lowered");
		}
	} else {
		new_load = new_bd_arm_Ldr(dbgi, block, am.base, new_mem, mode, am.entity, 0, am.offset, am.is_frame_entity);
	}
	set_irn_pinned(new_load, get_irn_pinned(node));

	return new_load;
}

static ir_node *gen_Store(ir_node *node)
{
	ir_node  *block   = be_transform_nodes_block(node);
	ir_node  *ptr     = get_Store_ptr(node);
	ir_node  *val     = get_Store_value(node);
	ir_mode  *mode    = get_irn_mode(val);
	arm_am_t  am      = transform_am(ptr, mode, true);
	ir_node  *mem     = get_Store_mem(node);
	ir_node  *new_mem = be_transform_node(mem);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	if (get_Store_unaligned(node) == align_non_aligned)
		panic("unaligned Stores not supported yet");

	ir_node *new_store;
	if (mode_is_float(mode)) {
		ir_node *const new_val = be_transform_node(val);
		if (arm_cg_config.fpu == ARM_FPU_FPA) {
			new_store = new_bd_arm_Stf(dbgi, block, am.base, new_val, new_mem, mode, am.entity, 0, am.offset, am.is_frame_entity);
		} else {
			panic("softfloat not lowered");
		}
	} else {
		val = be_skip_downconv(val, false);
		ir_node *const new_val = be_transform_node(val);
		new_store = new_bd_arm_Str(dbgi, block, am.base, new_val, new_mem, mode, am.entity, 0, am.offset, am.is_frame_entity);
	}
	set_irn_pinned(new_store, get_irn_pinned(node));
	return new_store;
}

static ir_node *gen_IJmp(ir_node *const node)
{
	dbg_info *const dbgi       = get_irn_dbg_info(node);
	ir_node  *const block      = be_transform_nodes_block(node);
	ir_node  *const target     = get_IJmp_target(node);
	ir_node  *const new_target = be_transform_node(target);
	return new_bd_arm_IJmp(dbgi, block, new_target);
}

static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	return new_bd_arm_B(dbgi, new_block);
}

static ir_node *gen_Switch(ir_node *node)
{
	ir_graph              *irg      = get_irn_irg(node);
	ir_node               *block    = be_transform_nodes_block(node);
	ir_node               *selector = get_Switch_selector(node);
	dbg_info              *dbgi     = get_irn_dbg_info(node);
	ir_node               *new_op   = be_transform_node(selector);
	const ir_switch_table *table    = get_Switch_table(node);
	unsigned               n_outs   = get_Switch_n_outs(node);

	table = ir_switch_table_duplicate(irg, table);

	/* switch selector should be lowered to singled word already */
	ir_mode *mode = get_irn_mode(selector);
	if (get_mode_size_bits(mode) != 32)
		panic("unexpected switch selector mode");

	return new_bd_arm_SwitchJmp(dbgi, block, new_op, n_outs, table);
}

static ir_node *gen_Cmp(ir_node *node)
{
	ir_node  *block    = be_transform_nodes_block(node);
	ir_node  *op1      = get_Cmp_left(node);
	ir_node  *op2      = get_Cmp_right(node);
	ir_mode  *cmp_mode = get_irn_mode(op1);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	if (mode_is_float(cmp_mode)) {
		/* TODO: this is broken... */
		ir_node *new_op1 = be_transform_node(op1);
		ir_node *new_op2 = be_transform_node(op2);

		return new_bd_arm_Cmfe(dbgi, block, new_op1, new_op2, false);
	}

	assert(get_irn_mode(op2) == cmp_mode);
	bool is_unsigned = !mode_is_signed(cmp_mode);

	/* integer compare, TODO: use shifter_op in all its combinations */
	ir_node *new_op1 = be_transform_node(op1);
	new_op1 = gen_extension(dbgi, block, new_op1, cmp_mode);

	arm_immediate_t imm;
	switch (try_encode_as_immediate(op2, &imm, IMM_POS | IMM_NEG)) {
	case IMM_POS:
		return new_bd_arm_Cmp_imm(dbgi, block, new_op1, imm.imm_8, imm.rot, false, is_unsigned);
	case IMM_NEG:
		return new_bd_arm_Cmn_imm(dbgi, block, new_op1, imm.imm_8, imm.rot, false, is_unsigned);
	default: {
		ir_node *new_op2 = be_transform_node(op2);
		new_op2 = gen_extension(dbgi, block, new_op2, cmp_mode);
		return new_bd_arm_Cmp_reg(dbgi, block, new_op1, new_op2, false, is_unsigned);
	}
	}
}

static ir_node *gen_Cond(ir_node *node)
{
	ir_node    *const block     = be_transform_nodes_block(node);
	dbg_info   *const dbgi      = get_irn_dbg_info(node);
	ir_node    *const selector  = get_Cond_selector(node);
	ir_node    *const flag_node = be_transform_node(selector);
	ir_relation const relation  = get_Cmp_relation(selector);
	return new_bd_arm_Bcc(dbgi, block, flag_node, relation);
}

enum fpa_imm_mode {
	FPA_IMM_FLOAT  = 0,
	FPA_IMM_DOUBLE = 1,
	FPA_IMM_MAX    = FPA_IMM_DOUBLE
};

static ir_tarval *fpa_imm[FPA_IMM_MAX + 1][fpa_max];

static ir_node *gen_Const(ir_node *node)
{
	ir_node  *block = be_transform_nodes_block(node);
	ir_mode  *mode  = get_irn_mode(node);
	dbg_info *dbg   = get_irn_dbg_info(node);

	if (mode_is_float(mode)) {
		if (arm_cg_config.fpu == ARM_FPU_FPA) {
			ir_tarval *tv = get_Const_tarval(node);
			return new_bd_arm_fConst(dbg, block, tv);
		} else {
			panic("softfloat not lowered");
		}
	}
	return create_const_graph(node, block);
}

static ir_node *gen_Address(ir_node *node)
{
	ir_node   *block  = be_transform_nodes_block(node);
	ir_entity *entity = get_Address_entity(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	if (is_tls_entity(entity))
		panic("TLS not supported yet");
	return new_bd_arm_Address(dbgi, block, entity, 0);
}

static ir_node *ints_to_double(dbg_info *dbgi, ir_node *block, ir_node *node0,
                               ir_node *node1)
{
	/* the good way to do this would be to use the stm (store multiple)
	 * instructions, since our input is nearly always 2 consecutive 32bit
	 * registers... */
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *str0  = new_bd_arm_Str(dbgi, block, stack, node0, nomem,
	                                 arm_mode_gp, NULL, 0, 0, true);
	ir_node  *str1  = new_bd_arm_Str(dbgi, block, stack, node1, nomem,
	                                 arm_mode_gp, NULL, 0, 4, true);
	ir_node  *in[]  = { str0, str1 };
	ir_node  *sync  = new_r_Sync(block, ARRAY_SIZE(in), in);
	set_irn_pinned(str0, false);
	set_irn_pinned(str1, false);

	ir_node *ldf = new_bd_arm_Ldf(dbgi, block, stack, sync, mode_D, NULL, 0, 0,
	                              true);
	set_irn_pinned(ldf, false);

	return be_new_Proj(ldf, pn_arm_Ldf_res);
}

static ir_node *int_to_float(dbg_info *dbgi, ir_node *block, ir_node *node)
{
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *str   = new_bd_arm_Str(dbgi, block, stack, node, nomem,
	                                 arm_mode_gp, NULL, 0, 0, true);
	set_irn_pinned(str, false);

	ir_node *ldf = new_bd_arm_Ldf(dbgi, block, stack, str, mode_F, NULL, 0, 0,
	                              true);
	set_irn_pinned(ldf, false);

	return be_new_Proj(ldf, pn_arm_Ldf_res);
}

static ir_node *float_to_int(dbg_info *dbgi, ir_node *block, ir_node *node)
{
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *stf   = new_bd_arm_Stf(dbgi, block, stack, node, nomem, mode_F,
	                                 NULL, 0, 0, true);
	set_irn_pinned(stf, false);

	ir_node *ldr = new_bd_arm_Ldr(dbgi, block, stack, stf, arm_mode_gp,
	                              NULL, 0, 0, true);
	set_irn_pinned(ldr, false);

	return be_new_Proj(ldr, pn_arm_Ldr_res);
}

static void double_to_ints(dbg_info *dbgi, ir_node *block, ir_node *node,
                           ir_node **out_value0, ir_node **out_value1)
{
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *stf   = new_bd_arm_Stf(dbgi, block, stack, node, nomem, mode_D,
	                                 NULL, 0, 0, true);
	set_irn_pinned(stf, false);

	ir_node *ldr0 = new_bd_arm_Ldr(dbgi, block, stack, stf, arm_mode_gp, NULL,
	                               0, 0, true);
	set_irn_pinned(ldr0, false);
	ir_node *ldr1 = new_bd_arm_Ldr(dbgi, block, stack, stf, arm_mode_gp, NULL,
	                               0, 4, true);
	set_irn_pinned(ldr1, false);

	*out_value0 = be_new_Proj(ldr0, pn_arm_Ldr_res);
	*out_value1 = be_new_Proj(ldr1, pn_arm_Ldr_res);
}

/**
 * Transform builtin clz.
 */
static ir_node *gen_clz(ir_node *node)
{
	ir_node  *block  = be_transform_nodes_block(node);
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
	case ir_bk_saturating_increment:
	case ir_bk_compare_swap:
	case ir_bk_may_alias:
	case ir_bk_va_start:
	case ir_bk_va_arg:
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
		assert(get_Proj_num(proj) == pn_Builtin_max+1);
		return new_node;
	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_prefetch:
	case ir_bk_outport:
		assert(get_Proj_num(proj) == pn_Builtin_M);
		return new_node;
	case ir_bk_inport:
	case ir_bk_saturating_increment:
	case ir_bk_compare_swap:
	case ir_bk_may_alias:
	case ir_bk_va_start:
	case ir_bk_va_arg:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *load     = get_Proj_pred(node);
	ir_node  *new_load = be_transform_node(load);
	unsigned  pn       = get_Proj_num(node);

	/* renumber the proj */
	switch (get_arm_irn_opcode(new_load)) {
	case iro_arm_Ldr:
		/* handle all gp loads equal: they have the same proj numbers. */
		if (pn == pn_Load_res) {
			return be_new_Proj(new_load, pn_arm_Ldr_res);
		} else if (pn == pn_Load_M) {
			return be_new_Proj(new_load, pn_arm_Ldr_M);
		}
		break;
	case iro_arm_Ldf:
		if (pn == pn_Load_res) {
			return be_new_Proj(new_load, pn_arm_Ldf_res);
		} else if (pn == pn_Load_M) {
			return be_new_Proj(new_load, pn_arm_Ldf_M);
		}
		break;
	default:
		break;
	}
	panic("unsupported Proj from Load");
}

static ir_node *gen_Proj_Div(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	unsigned  pn       = get_Proj_num(node);

	switch ((pn_Div)pn) {
	case pn_Div_M:
		return be_new_Proj(new_pred, pn_arm_Dvf_M);
	case pn_Div_res:
		return be_new_Proj(new_pred, pn_arm_Dvf_res);
	case pn_Div_X_regular:
	case pn_Div_X_except:
		break;
	}
	panic("unsupported Proj from Div");
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_graph *const irg = get_irn_irg(node);
	unsigned pn = get_Proj_num(node);
	switch ((pn_Start)pn) {
	case pn_Start_M:
		return be_get_Start_mem(irg);

	case pn_Start_T_args:
		return new_r_Bad(irg, mode_T);

	case pn_Start_P_frame_base:
		return get_initial_sp(irg);
	}
	panic("unexpected start proj: %u", pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	/* Proj->Proj->Start must be a method argument */
	assert(get_Proj_num(get_Proj_pred(node)) == pn_Start_T_args);

	ir_node                  *const new_block = be_transform_nodes_block(node);
	ir_graph                 *const irg       = get_irn_irg(new_block);
	unsigned                  const pn        = get_Proj_num(node);
	reg_or_stackslot_t const *const param     = &cconv->parameters[pn];
	arch_register_t    const *const reg0      = param->reg0;
	if (reg0 != NULL) {
		/* argument transmitted in register */
		ir_node *value = be_get_Start_proj(irg, reg0);

		if (mode_is_float(reg0->cls->mode)) {
			ir_node *value1 = NULL;

			const arch_register_t *reg1 = param->reg1;
			if (reg1 != NULL) {
				value1 = be_get_Start_proj(irg, reg1);
			} else if (param->entity != NULL) {
				ir_node *const fp  = get_irg_frame(irg);
				ir_node *const mem = be_get_Start_mem(irg);
				ir_node *const ldr = new_bd_arm_Ldr(NULL, new_block, fp, mem,
				                                    arm_mode_gp, param->entity,
				                                    0, 0, true);
				value1 = be_new_Proj(ldr, pn_arm_Ldr_res);
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
		ir_node *const mem  = be_get_Start_mem(irg);
		ir_mode *const mode = get_type_mode(param->type);

		ir_node *load;
		ir_node *value;
		if (mode_is_float(mode)) {
			load  = new_bd_arm_Ldf(NULL, new_block, fp, mem, mode,
			                       param->entity, 0, 0, true);
			value = be_new_Proj(load, pn_arm_Ldf_res);
		} else {
			load  = new_bd_arm_Ldr(NULL, new_block, fp, mem, mode,
			                       param->entity, 0, 0, true);
			value = be_new_Proj(load, pn_arm_Ldr_res);
		}
		set_irn_pinned(load, false);

		return value;
	}
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	unsigned              pn            = get_Proj_num(node);
	ir_node              *call          = get_Proj_pred(get_Proj_pred(node));
	ir_node              *new_call      = be_transform_node(call);
	ir_type              *function_type = get_Call_type(call);
	calling_convention_t *cconv
		= arm_decide_calling_convention(NULL, function_type);
	const reg_or_stackslot_t *res = &cconv->results[pn];

	assert(res->reg0 != NULL && res->reg1 == NULL);
	unsigned const regn = be_get_out_for_reg(new_call, res->reg0);

	arm_free_calling_convention(cconv);

	return be_new_Proj(new_call, regn);
}

static ir_node *gen_Proj_Call(ir_node *node)
{
	unsigned pn        = get_Proj_num(node);
	ir_node *call      = get_Proj_pred(node);
	ir_node *new_call  = be_transform_node(call);
	switch ((pn_Call)pn) {
	case pn_Call_M:
		return be_new_Proj(new_call, pn_arm_Bl_M);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
		break;
	}
	panic("unexpected Call proj %u", pn);
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node *pred = get_Proj_pred(node);
	unsigned pn   = get_Proj_num(node);
	switch ((pn_Store)pn) {
	case pn_Store_M:
		return be_transform_node(pred);
	case pn_Store_X_regular:
	case pn_Store_X_except:
		break;
	}
	panic("unsupported Proj from Store");
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
	panic("code selection didn't expect Proj(Proj) after %+F", pred_pred);
}

static ir_node *gen_Unknown(ir_node *node)
{
	ir_node *const block = be_transform_nodes_block(node);
	ir_mode *const mode  = get_irn_mode(node);
	if (mode_is_float(mode)) {
		return be_new_Unknown(block, &arm_class_reg_req_fpa);
	} else if (get_mode_arithmetic(mode) == irma_twos_complement) {
		return be_new_Unknown(block, &arm_class_reg_req_gp);
	} else {
		panic("unexpected Unknown mode");
	}
}

static void create_stacklayout(ir_graph *irg, calling_convention_t const *cconv)
{
	/* construct entities for arguments. */
	ir_entity **const param_map  = be_collect_parameter_entities(irg);
	ir_type    *const frame_type = get_irg_frame_type(irg);
	for (unsigned p = 0, n_params = cconv->n_parameters; p < n_params; ++p) {
		reg_or_stackslot_t *param = &cconv->parameters[p];
		if (param->type == NULL)
			continue;

		ir_entity *ent = param_map[p];
		if (!param->reg0) {
			if (!ent)
				ent = new_parameter_entity(frame_type, p, param->type);
			set_entity_offset(ent, param->offset);
		}
		param->entity = ent;
	}
	free(param_map);
}

/**
 * transform the start node to the prolog code
 */
static ir_node *gen_Start(ir_node *node)
{
	be_start_out outs[N_ARM_REGISTERS] = { [REG_SP] = BE_START_IGNORE };

	/* function parameters in registers */
	for (size_t i = 0, n = cconv->n_parameters; i != n; ++i) {
		const reg_or_stackslot_t *param = &cconv->parameters[i];
		const arch_register_t    *reg0  = param->reg0;
		if (reg0)
			outs[reg0->global_index] = BE_START_REG;
		const arch_register_t *reg1 = param->reg1;
		if (reg1)
			outs[reg1->global_index] = BE_START_REG;
	}

	/* callee save regs */
	for (size_t i = 0; i < ARRAY_SIZE(callee_saves); ++i) {
		outs[callee_saves[i]->global_index] = BE_START_REG;
	}

	ir_graph *const irg = get_irn_irg(node);
	return be_new_Start(irg, outs);
}

/**
 * transform a Return node into epilogue code + return statement
 */
static ir_node *gen_Return(ir_node *node)
{
	ir_node        *new_block      = be_transform_nodes_block(node);
	dbg_info       *dbgi           = get_irn_dbg_info(node);
	ir_node        *mem            = get_Return_mem(node);
	ir_node        *new_mem        = be_transform_node(mem);
	unsigned        n_callee_saves = ARRAY_SIZE(callee_saves);
	unsigned        n_res          = get_Return_n_ress(node);
	ir_graph       *irg            = get_irn_irg(node);

	unsigned       p     = n_arm_Return_first_result;
	unsigned const n_ins = p + n_res + n_callee_saves;

	arch_register_req_t const **const reqs = be_allocate_in_reqs(irg, n_ins);
	ir_node **in = ALLOCAN(ir_node*, n_ins);

	in[n_arm_Return_mem]   = new_mem;
	reqs[n_arm_Return_mem] = arch_memory_req;

	in[n_arm_Return_sp]   = get_initial_sp(irg);
	reqs[n_arm_Return_sp] = &arm_single_reg_req_gp_sp;

	/* result values */
	for (size_t i = 0; i < n_res; ++i) {
		ir_node                  *res_value     = get_Return_res(node, i);
		ir_node                  *new_res_value = be_transform_node(res_value);
		const reg_or_stackslot_t *slot          = &cconv->results[i];
		const arch_register_t    *reg           = slot->reg0;
		assert(slot->reg1 == NULL);
		in[p]   = new_res_value;
		reqs[p] = reg->single_req;
		++p;
	}
	/* connect callee saves with their values at the function begin */
	for (unsigned i = 0; i < n_callee_saves; ++i) {
		arch_register_t const *const reg = callee_saves[i];
		in[p]   = be_get_Start_proj(irg, reg);
		reqs[p] = reg->single_req;
		++p;
	}
	assert(p == n_ins);

	ir_node *const ret = new_bd_arm_Return(dbgi, new_block, n_ins, in, reqs);
	be_stack_record_chain(&stack_env, ret, n_arm_Return_sp, NULL);
	return ret;
}

static ir_node *gen_Call(ir_node *node)
{
	ir_graph             *irg          = get_irn_irg(node);
	ir_node              *callee       = get_Call_ptr(node);
	ir_node              *new_block    = be_transform_nodes_block(node);
	ir_node              *mem          = get_Call_mem(node);
	ir_node              *new_mem      = be_transform_node(mem);
	dbg_info             *dbgi         = get_irn_dbg_info(node);
	ir_type              *type         = get_Call_type(node);
	calling_convention_t *cconv        = arm_decide_calling_convention(NULL, type);
	size_t                n_params     = get_Call_n_params(node);
	size_t const          n_param_regs = cconv->n_param_regs;
	/* max inputs: memory, stack, callee, register arguments */
	size_t const          max_inputs   = 3 + n_param_regs;
	ir_node             **in           = ALLOCAN(ir_node*, max_inputs);
	ir_node             **sync_ins     = ALLOCAN(ir_node*, n_params);
	arch_register_req_t const **const in_req = be_allocate_in_reqs(irg, max_inputs);
	size_t                in_arity       = 0;
	size_t                sync_arity     = 0;
	size_t const          n_caller_saves = ARRAY_SIZE(caller_saves);
	ir_entity            *entity         = NULL;

	assert(n_params == cconv->n_parameters);

	/* memory input */
	int mem_pos     = in_arity++;
	in_req[mem_pos] = arch_memory_req;
	/* stack pointer (create parameter stackframe + align stack)
	 * Note that we always need an IncSP to ensure stack alignment */
	ir_node *const new_frame = get_initial_sp(irg);
	ir_node *const callframe = be_new_IncSP(new_block, new_frame, cconv->param_stack_size, false);
	int sp_pos = in_arity++;
	in_req[sp_pos] = &arm_single_reg_req_gp_sp;
	in[sp_pos]     = callframe;

	/* parameters */
	for (size_t p = 0; p < n_params; ++p) {
		ir_node                  *value      = get_Call_param(node, p);
		ir_node                  *new_value  = be_transform_node(value);
		ir_node                  *new_value1 = NULL;
		const reg_or_stackslot_t *param      = &cconv->parameters[p];
		ir_type                  *param_type = get_method_param_type(type, p);
		ir_mode                  *mode       = get_type_mode(param_type);

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
			mode      = arm_mode_gp;
		}

		/* Create a parameter frame if necessary */
		ir_node *const str = mode_is_float(mode)
			? new_bd_arm_Stf(dbgi, new_block, callframe, new_value, new_mem,
			                 mode, NULL, 0, param->offset, false)
			: new_bd_arm_Str(dbgi, new_block, callframe, new_value, new_mem,
			                 mode, NULL, 0, param->offset, false);
		sync_ins[sync_arity++] = str;
	}

	/* construct memory input */
	if (sync_arity == 0) {
		in[mem_pos] = new_mem;
	} else {
		in[mem_pos] = be_make_Sync(new_block, sync_arity, sync_ins);
	}

	/* TODO: use a generic address matcher here */
	unsigned shiftop_input = 0;
	if (is_Address(callee)) {
		entity = get_Address_entity(callee);
	} else {
		/* TODO: finish load matcher here */
		shiftop_input    = in_arity;
		in[in_arity]     = be_transform_node(callee);
		in_req[in_arity] = &arm_class_reg_req_gp;
		++in_arity;
	}
	assert(sync_arity <= n_params);
	assert(in_arity <= max_inputs);

	/* Count outputs. */
	unsigned const out_arity = pn_arm_Bl_first_result + n_caller_saves;

	ir_node *res;
	if (entity != NULL) {
		/* TODO: use a generic address matcher here
		 * so we can also handle entity+offset, etc. */
		res = new_bd_arm_Bl(dbgi, new_block, in_arity, in, in_req, out_arity, entity, 0);
	} else {
		/* TODO:
		 * - use a proper shifter_operand matcher
		 * - we could also use LinkLdrPC
		 */
		res = new_bd_arm_LinkMovPC(dbgi, new_block, in_arity, in, in_req, out_arity, shiftop_input, ARM_SHF_REG, 0, 0);
	}

	/* create output register reqs */
	arch_set_irn_register_req_out(res, pn_arm_Bl_M, arch_memory_req);
	arch_copy_irn_out_info(res, pn_arm_Bl_stack, callframe);

	for (size_t o = 0; o < n_caller_saves; ++o) {
		const arch_register_t *reg = caller_saves[o];
		arch_set_irn_register_req_out(res, pn_arm_Bl_first_result + o, reg->single_req);
	}

	/* copy pinned attribute */
	set_irn_pinned(res, get_irn_pinned(node));

	/* IncSP to destroy the call stackframe */
	ir_node *const call_stack = be_new_Proj(res, pn_arm_Bl_stack);
	ir_node *const incsp      = be_new_IncSP(new_block, call_stack, -cconv->param_stack_size, false);
	be_stack_record_chain(&stack_env, callframe, n_be_IncSP_pred, incsp);

	arm_free_calling_convention(cconv);
	return res;
}

static ir_node *gen_Member(ir_node *node)
{
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *new_block = be_transform_nodes_block(node);
	ir_node   *ptr       = get_Member_ptr(node);
	ir_node   *new_ptr   = be_transform_node(ptr);
	ir_entity *entity    = get_Member_entity(node);

	/* must be the frame pointer all other sels must have been lowered
	 * already */
	assert(is_Proj(ptr) && is_Start(get_Proj_pred(ptr)));

	return new_bd_arm_FrameAddr(dbgi, new_block, new_ptr, entity, 0);
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (get_mode_arithmetic(mode) == irma_twos_complement) {
		/* we shouldn't have any 64bit stuff around anymore */
		assert(get_mode_size_bits(mode) <= 32);
		/* all integer operations are on 32bit registers now */
		req = &arm_class_reg_req_gp;
	} else {
		req = arch_memory_req;
	}

	return be_transform_phi(node, req);
}

static void arm_parse_constraint_letter(void const *const env, be_asm_constraint_t* const c, char const l)
{
	(void)env;

	switch (l) {
	case 'g':
		c->all_registers_allowed = true;
		c->memory_possible       = true;
		/* FALLTHROUGH */
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'i':
	case 'n':
		c->cls            = &arm_reg_classes[CLASS_arm_gp];
		c->immediate_type = l;
		break;

	case 'Q':
	case 'm':
		c->memory_possible = true;
		break;

	case 'l':
	case 'r':
		c->cls                   = &arm_reg_classes[CLASS_arm_gp];
		c->all_registers_allowed = true;
		break;

	default:
		panic("unknown asm constraint '%c'", l);
	}
}

static bool arm_check_immediate_constraint(long const val, char const imm_type)
{
	(void)val;
	arm_immediate_t res;
	switch (imm_type) {
	case 'I': return try_encode_val_as_immediate( val, &res);
	case 'J': return -4095 <= val && val <= 4095;
	case 'K': return try_encode_val_as_immediate(~val, &res);
	case 'L': return try_encode_val_as_immediate(-val, &res);
	case 'M': return     0 <= val && val <=   32;

	case 'g':
	case 'i':
	case 'n': return true;
	}
	panic("invalid immediate constraint found");
}

static bool arm_match_immediate(arm_asm_operand_t *const operand, ir_node *const node, char const imm_type)
{
	ir_tarval *offset;
	ir_entity *entity;
	unsigned   reloc_kind;
	if (!be_match_immediate(node, &offset, &entity, &reloc_kind))
		return false;
	assert(reloc_kind == 0);

	if (entity && imm_type != 'g' && imm_type != 'i')
		return false;

	long value;
	if (offset) {
		value = get_tarval_long(offset);
		if (!arm_check_immediate_constraint(value, imm_type))
			return false;
	} else {
		value = 0;
	}

	operand->val = value;
	operand->ent = entity;
	return true;
}

static ir_node *gen_ASM(ir_node *const node)
{
	be_asm_info_t info = be_asm_prepare_info(node);

	ir_asm_constraint const *const constraints   = get_ASM_constraints(node);
	size_t                   const n_constraints = get_ASM_n_constraints(node);
	ir_graph                *const irg           = get_irn_irg(node);
	struct obstack          *const obst          = get_irg_obstack(irg);
	arm_asm_operand_t       *const operands      = NEW_ARR_DZ(arm_asm_operand_t, obst, n_constraints);
	for (size_t i = 0; i != n_constraints; ++i) {
		ir_asm_constraint const *const c = &constraints[i];

		be_asm_constraint_t be_constraint;
		be_parse_asm_constraints_internal(&be_constraint, c->constraint, &arm_parse_constraint_letter, NULL);

		arm_asm_operand_t *const op = &operands[i];

		int const in_pos = c->in_pos;
		if (in_pos >= 0) {
			ir_node *const in  = get_ASM_input(node, in_pos);
			char     const imm = be_constraint.immediate_type;
			if (imm != '\0' && arm_match_immediate(op, in, imm)) {
				be_asm_add_immediate(&op->op);
			} else if (be_constraint.same_as >= 0) {
				int                        const out_pos = operands[be_constraint.same_as].op.pos;
				arch_register_req_t const *const ireq    = info.out_reqs[out_pos];
				be_asm_add_inout(&info, &op->op, obst, in, ireq, out_pos);
			} else if (be_constraint.cls) {
				arch_register_req_t const *const ireq = be_make_register_req(obst, &be_constraint);
				be_asm_add_inout(&info, &op->op, obst, in, ireq, c->out_pos);
			} else {
				ir_node                   *const new_in = be_transform_node(in);
				arch_register_req_t const *const ireq   = arch_get_irn_register_req(new_in)->cls->class_req;
				be_asm_add_in(&info, &op->op, BE_ASM_OPERAND_MEMORY, new_in, ireq);
			}
		} else {
			be_asm_add_out(&info, &op->op, obst, &be_constraint, c->out_pos);
		}
	}

	return be_make_asm(node, &info, operands);
}

/**
 * Enters all transform functions into the generic pointer
 */
static void arm_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_ASM,         gen_ASM);
	be_set_transform_function(op_Add,         gen_Add);
	be_set_transform_function(op_Address,     gen_Address);
	be_set_transform_function(op_And,         gen_And);
	be_set_transform_function(op_arm_AdC_t,   gen_arm_AdC_t);
	be_set_transform_function(op_arm_AddS_t,  gen_arm_AddS_t);
	be_set_transform_function(op_arm_OrPl_t,  gen_arm_OrPl_t);
	be_set_transform_function(op_arm_SbC_t,   gen_arm_SbC_t);
	be_set_transform_function(op_arm_SubS_t,  gen_arm_SubS_t);
	be_set_transform_function(op_arm_UMulL_t, gen_arm_UMulL_t);
	be_set_transform_function(op_Builtin,     gen_Builtin);
	be_set_transform_function(op_Call,        gen_Call);
	be_set_transform_function(op_Cmp,         gen_Cmp);
	be_set_transform_function(op_Cond,        gen_Cond);
	be_set_transform_function(op_Const,       gen_Const);
	be_set_transform_function(op_Conv,        gen_Conv);
	be_set_transform_function(op_Div,         gen_Div);
	be_set_transform_function(op_Eor,         gen_Eor);
	be_set_transform_function(op_IJmp,        gen_IJmp);
	be_set_transform_function(op_Jmp,         gen_Jmp);
	be_set_transform_function(op_Load,        gen_Load);
	be_set_transform_function(op_Member,      gen_Member);
	be_set_transform_function(op_Minus,       gen_Minus);
	be_set_transform_function(op_Mul,         gen_Mul);
	be_set_transform_function(op_Not,         gen_Not);
	be_set_transform_function(op_Or,          gen_Or);
	be_set_transform_function(op_Phi,         gen_Phi);
	be_set_transform_function(op_Return,      gen_Return);
	be_set_transform_function(op_Shl,         gen_Shl);
	be_set_transform_function(op_Shr,         gen_Shr);
	be_set_transform_function(op_Shrs,        gen_Shrs);
	be_set_transform_function(op_Start,       gen_Start);
	be_set_transform_function(op_Store,       gen_Store);
	be_set_transform_function(op_Sub,         gen_Sub);
	be_set_transform_function(op_Switch,      gen_Switch);
	be_set_transform_function(op_Unknown,     gen_Unknown);

	be_set_transform_proj_function(op_arm_AddS_t,  gen_Proj_arm_AddS_t);
	be_set_transform_proj_function(op_arm_SubS_t,  gen_Proj_arm_SubS_t);
	be_set_transform_proj_function(op_arm_UMulL_t, gen_Proj_arm_UMulL_t);
	be_set_transform_proj_function(op_Builtin,     gen_Proj_Builtin);
	be_set_transform_proj_function(op_Call,        gen_Proj_Call);
	be_set_transform_proj_function(op_Div,         gen_Proj_Div);
	be_set_transform_proj_function(op_Load,        gen_Proj_Load);
	be_set_transform_proj_function(op_Proj,        gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,       gen_Proj_Start);
	be_set_transform_proj_function(op_Store,       gen_Proj_Store);
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
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_NO_BADS
	                         | IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	static bool imm_initialized = false;
	if (!imm_initialized) {
		arm_init_fpa_immediate();
		imm_initialized = true;
	}
	arm_register_transformers();

	assert(cconv == NULL);
	be_stack_init(&stack_env);
	ir_entity *entity = get_irg_entity(irg);
	cconv = arm_decide_calling_convention(irg, get_entity_type(entity));
	create_stacklayout(irg, cconv);
	be_add_parameter_entity_stores(irg);

	be_transform_graph(irg, NULL);

	be_stack_finish(&stack_env);

	arm_free_calling_convention(cconv);
	cconv = NULL;
}

void arm_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.arm.transform");
}
