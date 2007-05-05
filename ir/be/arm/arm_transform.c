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
 * @brief   The codegenerator (transform FIRM into arm FIRM)
 * @author  Oliver Richter, Tobias Gneist, Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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

#include "../benode_t.h"
#include "bearch_arm_t.h"

#include "arm_nodes_attr.h"
#include "../arch/archop.h"     /* we need this for Min and Max nodes */
#include "arm_transform.h"
#include "arm_new_nodes.h"
#include "arm_map_regs.h"

#include "gen_arm_regalloc_if.h"

#include <limits.h>


extern ir_op *get_op_Mulh(void);



/****************************************************************************************************
 *                  _        _                        __                           _   _
 *                 | |      | |                      / _|                         | | (_)
 *  _ __   ___   __| | ___  | |_ _ __ __ _ _ __  ___| |_ ___  _ __ _ __ ___   __ _| |_ _  ___  _ __
 * | '_ \ / _ \ / _` |/ _ \ | __| '__/ _` | '_ \/ __|  _/ _ \| '__| '_ ` _ \ / _` | __| |/ _ \| '_ \
 * | | | | (_) | (_| |  __/ | |_| | | (_| | | | \__ \ || (_) | |  | | | | | | (_| | |_| | (_) | | | |
 * |_| |_|\___/ \__,_|\___|  \__|_|  \__,_|_| |_|___/_| \___/|_|  |_| |_| |_|\__,_|\__|_|\___/|_| |_|
 *
 ****************************************************************************************************/

typedef struct vals_ {
	int ops;
	unsigned char values[4];
	unsigned char shifts[4];
} vals;

/** Execute ROL. */
static unsigned do_rol(unsigned v, unsigned rol) {
	return (v << rol) | (v >> (32 - rol));
}

/**
 * construct 8bit values und rot amounts for a value
 */
static void gen_vals_from_word(unsigned int value, vals *result)
{
	int initial = 0;

	memset(result, 0, sizeof(*result));

	/* special case: we prefer shift amount 0 */
	if (value < 0x100) {
		result->values[0] = value;
		result->ops       = 1;
		return;
	}

	while (value != 0) {
		if (value & 0xFF) {
			unsigned v = do_rol(value, 8) & 0xFFFFFF;
			int shf = 0;
			for (;;) {
				if ((v & 3) != 0)
					break;
				shf += 2;
				v >>= 2;
			}
			v  &= 0xFF;
			shf = (initial + shf - 8) & 0x1F;
			result->values[result->ops] = v;
			result->shifts[result->ops] = shf;
			++result->ops;

			value ^= do_rol(v, shf) >> initial;
		}
		else {
			value >>= 8;
			initial += 8;
		}
	}
}

/**
 * Creates a arm_Const node.
 */
static ir_node *create_const_node(ir_node *irn, ir_node *block, long value) {
	tarval *tv = new_tarval_from_long(value, mode_Iu);
	dbg_info *dbg = get_irn_dbg_info(irn);
	return new_rd_arm_Mov_i(dbg, current_ir_graph, block, get_irn_mode(irn), tv);
}

/**
 * Creates a arm_Const_Neg node.
 */
static ir_node *create_const_neg_node(ir_node *irn, ir_node *block, long value) {
	tarval *tv = new_tarval_from_long(value, mode_Iu);
	dbg_info *dbg = get_irn_dbg_info(irn);
	return new_rd_arm_Mvn_i(dbg, current_ir_graph, block, get_irn_mode(irn), tv);
}

#define NEW_BINOP_NODE(opname, env, op1, op2) new_rd_arm_##opname(env->dbg, current_ir_graph, env->block, op1, op2, env->mode)

/**
 * Encodes an immediate with shifter operand
 */
static unsigned int arm_encode_imm_w_shift(unsigned int shift, unsigned int immediate) {
	return immediate | ((shift>>1)<<8);
}

/**
 * Decode an immediate with shifter operand
 */
unsigned int arm_decode_imm_w_shift(tarval *tv) {
	unsigned l = get_tarval_long(tv);
	unsigned rol = (l & ~0xFF) >> 7;

	return do_rol(l & 0xFF, rol);
}

/**
 * Creates a possible DAG for an constant.
 */
static ir_node *create_const_graph_value(ir_node *irn, ir_node *block, unsigned int value) {
	ir_node *result;
	vals v, vn;
	int cnt;
	ir_mode *mode = get_irn_mode(irn);
	dbg_info *dbg = get_irn_dbg_info(irn);

	gen_vals_from_word(value, &v);
	gen_vals_from_word(~value, &vn);

	if (vn.ops < v.ops) {
		/* remove bits */
		result = create_const_neg_node(irn, block, arm_encode_imm_w_shift(vn.shifts[0], vn.values[0]));

		for (cnt = 1; cnt < vn.ops; ++cnt) {
			tarval *tv = new_tarval_from_long(arm_encode_imm_w_shift(vn.shifts[cnt], vn.values[cnt]), mode_Iu);
			ir_node *bic_i_node = new_rd_arm_Bic_i(dbg, current_ir_graph, block, result, mode, tv);
			result = bic_i_node;
		}
	}
	else {
		/* add bits */
		result = create_const_node(irn, block, arm_encode_imm_w_shift(v.shifts[0], v.values[0]));

		for (cnt = 1; cnt < v.ops; ++cnt) {
			tarval *tv = new_tarval_from_long(arm_encode_imm_w_shift(v.shifts[cnt], v.values[cnt]), mode_Iu);
			ir_node *orr_i_node = new_rd_arm_Or_i(dbg, current_ir_graph, block, result, mode, tv);
			result = orr_i_node;
		}
	}
	return result;
}

/**
 * Create a DAG constructing a given Const.
 *
 * @param irn  a Firm const
 */
static ir_node *create_const_graph(ir_node *irn, ir_node *block) {
	int value = get_tarval_long(get_Const_tarval(irn));
	return create_const_graph_value(irn, block, value);
}


/**
 * Creates code for a Firm Const node.
 */
static ir_node *gen_Const(ir_node *irn, arm_code_gen_t *cg) {
	ir_graph *irg = current_ir_graph;
	ir_node *block = get_nodes_block(irn);
	ir_mode *mode = get_irn_mode(irn);
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (mode_is_float(mode)) {
		if (USE_FPA(cg->isa))
			return new_rd_arm_fpaConst(dbg, irg, block, mode, get_Const_tarval(irn));
		else if (USE_VFP(cg->isa))
			assert(mode != mode_E && "IEEE Extended FP not supported");
		assert(0 && "NYI");
	}
	else if (mode_is_reference(mode))
		return irn;
	return create_const_graph(irn, block);
}

static ir_node *gen_mask(ir_node *irn, ir_node *op, int result_bits) {
	ir_node *block = get_nodes_block(irn);
	unsigned mask_bits = (1 << result_bits) - 1;
	ir_node *mask_node = create_const_graph_value(irn, block, mask_bits);
	dbg_info *dbg = get_irn_dbg_info(irn);
	return new_rd_arm_And(dbg, current_ir_graph, block, op, mask_node, get_irn_mode(irn), ARM_SHF_NONE, NULL);
}

static ir_node *gen_sign_extension(ir_node *irn, ir_node *op, int result_bits) {
	ir_node *block = get_nodes_block(irn);
	int shift_width = 32 - result_bits;
	ir_graph *irg = current_ir_graph;
	ir_node *shift_const_node = create_const_graph_value(irn, block, shift_width);
	dbg_info *dbg = get_irn_dbg_info(irn);
	ir_node *lshift_node = new_rd_arm_Shl(dbg, irg, block, op, shift_const_node, get_irn_mode(op));
	ir_node *rshift_node = new_rd_arm_Shrs(dbg, irg, block, lshift_node, shift_const_node, get_irn_mode(irn));
	return rshift_node;
}

/**
 * Transforms a Conv node.
 *
 * @param env   The transformation environment
 * @return the created arm Conv node
 */
static ir_node *gen_Conv(ir_node *irn, arm_code_gen_t *cg) {
	ir_graph *irg = current_ir_graph;
	ir_node *block   = get_nodes_block(irn);
	ir_node *op      = get_Conv_op(irn);
	ir_mode *in_mode = get_irn_mode(op);
	ir_mode *out_mode = get_irn_mode(irn);
	dbg_info *dbg    = get_irn_dbg_info(irn);

	if (in_mode == out_mode)
		return op;

	if (mode_is_float(in_mode) || mode_is_float(out_mode)) {
		cg->have_fp = 1;

		if (USE_FPA(cg->isa)) {
			if (mode_is_float(in_mode)) {
				if (mode_is_float(out_mode)) {
					/* from float to float */
					return new_rd_arm_fpaMov(dbg, irg, block, op, out_mode);
				}
				else {
					/* from float to int */
					return new_rd_arm_fpaFix(dbg, irg, block, op, out_mode);
				}
			}
			else {
				/* from int to float */
				return new_rd_arm_fpaFlt(dbg, irg, block, op, out_mode);
			}
		}
		assert(0 && "NYI");
	}
	else { /* complete in gp registers */
		int in_bits  = get_mode_size_bits(in_mode);
		int out_bits = get_mode_size_bits(out_mode);
		int in_sign  = get_mode_sign(in_mode);
		int out_sign = get_mode_sign(out_mode);

		// 32 -> 32
			// NOPpen
		if (in_bits == out_bits && in_bits == 32)
			return op;

		// 16 -> 16
			// unsigned -> unsigned
				// NOP
			// unsigned -> signed
				// sign extension (31:16)=(15)
			// signed -> unsigned
				// maskieren (31:16)=0
			// signed -> signed
				// NOP
		if (in_bits == out_bits && out_bits < 32) {
			if (in_sign && !out_sign) {
				return gen_mask(irn, op, out_bits);
			} else {
				return gen_sign_extension(irn, op, out_bits);
			}
		}

		// 16 -> 32
			// unsigned -> unsigned
				// NOP
			// unsigned -> signed
				// NOP
			// signed -> unsigned
				// sign extension (31:16)=(15)
			// signed -> signed
				// sign extension (31:16)=(15)
		if (in_bits < out_bits) {
			if (in_sign) {
				return gen_sign_extension(irn, op, out_bits);
			} else {
				return op;
			}
		}

		// 32 -> 16
			// unsigned -> unsigned
				// maskieren (31:16)=0
			// unsigned -> signed
				// maskieren (31:16)=0
			// signed -> unsigned
				// maskieren (31:16)=0
			// signed -> signed
				// sign extension (erledigt auch maskieren) (31:16)=(15)
		if (in_bits > out_bits) {
			if (in_sign && out_sign) {
				return gen_sign_extension(irn, op, out_bits);
			} else {
				return gen_mask(irn, op, out_bits);
			}
		}
		assert(0 && "recheck integer conversion logic!");
		return irn;
	}
	return NULL;
}

/**
 * Return true if an operand is a shifter operand
 */
static int is_shifter_operand(ir_node *n, arm_shift_modifier *pmod) {
	arm_shift_modifier mod = ARM_SHF_NONE;

	if (is_arm_Mov(n))
		mod = get_arm_shift_modifier(n);

	*pmod = mod;
	if (mod != ARM_SHF_NONE) {
		long v = get_tarval_long(get_arm_value(n));
		if (v < 32)
			return (int)v;
	}
	return 0;
}

/**
 * Creates an arm Add.
 *
 * @param env   The transformation environment
 * @return the created arm Add node
 */
static ir_node *gen_Add(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_node *op1 = get_Add_left(irn);
	ir_node *op2 = get_Add_right(irn);
	ir_mode *mode = get_irn_mode(irn);
	ir_graph *irg = current_ir_graph;
	ir_node *op3;
	int v;
	arm_shift_modifier mod;
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (mode_is_float(mode)) {
		cg->have_fp = 1;
		if (USE_FPA(cg->isa))
			return new_rd_arm_fpaAdd(dbg, irg, block, op1, op2, mode);
		else if (USE_VFP(cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
		}
		assert(0 && "NYI");
	}
	if (mode_is_numP(mode)) {
		if (is_arm_Mov_i(op1))
			return new_rd_arm_Add_i(dbg, irg, block, op2, mode, get_arm_value(op1));
		if (is_arm_Mov_i(op2))
			return new_rd_arm_Add_i(dbg, irg, block, op1, mode, get_arm_value(op2));

		/* check for MLA */
		if (is_arm_Mul(op1) && get_irn_n_edges(op1) == 1) {
			op3 = op2;
			op2 = get_irn_n(op1, 1);
			op1 = get_irn_n(op1, 0);

			return new_rd_arm_Mla(dbg, irg, block, op1, op2, op3, mode);
		}
		if (is_arm_Mul(op2) && get_irn_n_edges(op2) == 1) {
			op3 = op1;
			op1 = get_irn_n(op2, 0);
			op2 = get_irn_n(op2, 1);

			return new_rd_arm_Mla(dbg, irg, block, op1, op2, op3, mode);
		}

		/* is the first a shifter */
		v = is_shifter_operand(op1, &mod);
		if (v) {
			op1 = get_irn_n(op1, 0);
			return new_rd_arm_Add(dbg, irg, block, op2, op1, mode, mod, new_tarval_from_long(v, mode_Iu));
		}
		/* is the second a shifter */
		v = is_shifter_operand(op2, &mod);
		if (v) {
			op2 = get_irn_n(op2, 0);
			return new_rd_arm_Add(dbg, irg, block, op1, op2, mode, mod, new_tarval_from_long(v, mode_Iu));
		}

		/* normal ADD */
		return new_rd_arm_Add(dbg, irg, block, op1, op2, mode, ARM_SHF_NONE, NULL);
	}

	assert(0 && "unknown mode for add");
	return NULL;
}

/**
 * Creates an arm Mul.
 *
 * @param env   The transformation environment
 * @return the created arm Mul node
 */
static ir_node *gen_Mul(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_node *op1 = get_Mul_left(irn);
	ir_node *op2 = get_Mul_right(irn);
	ir_mode *mode = get_irn_mode(irn);
	ir_graph *irg = current_ir_graph;
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (mode_is_float(mode)) {
		cg->have_fp = 1;
		if (USE_FPA(cg->isa))
			return new_rd_arm_fpaMul(dbg, irg, block, op1, op2, mode);
		else if (USE_VFP(cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
		}
		assert(0 && "NYI");
	}
	return new_rd_arm_Mul(dbg, irg, block, op1, op2, mode);
}

/**
 * Creates an arm floating point Div.
 *
 * @param env   The transformation environment
 * @return the created arm fDiv node
 */
static ir_node *gen_Quot(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_node *op1 = get_Quot_left(irn);
	ir_node *op2 = get_Quot_right(irn);
	ir_mode *mode = get_irn_mode(irn);
	dbg_info *dbg = get_irn_dbg_info(irn);

	assert(mode != mode_E && "IEEE Extended FP not supported");

	cg->have_fp = 1;
	if (USE_FPA(cg->isa))
		return new_rd_arm_fpaDiv(dbg, current_ir_graph, block, op1, op2, mode);
	else if (USE_VFP(cg->isa)) {
		assert(mode != mode_E && "IEEE Extended FP not supported");
	}
	assert(0 && "NYI");

	return NULL;
}

#define GEN_INT_OP(op) \
static ir_node *gen_ ## op(ir_node *irn, arm_code_gen_t *cg) { \
	ir_graph *irg = current_ir_graph; \
	ir_node *block = get_nodes_block(irn); \
	ir_node *op1 = get_ ## op ## _left(irn); \
	ir_node *op2 = get_ ## op ## _right(irn); \
	int v; \
	arm_shift_modifier mod; \
	ir_mode *mode = get_irn_mode(irn); \
	dbg_info *dbg = get_irn_dbg_info(irn); \
 \
	if (is_arm_Mov_i(op1)) \
		return new_rd_arm_ ## op ## _i(dbg, irg, block, op2, mode, get_arm_value(op1)); \
	if (is_arm_Mov_i(op2)) \
		return new_rd_arm_ ## op ## _i(dbg, irg, block, op1, mode, get_arm_value(op2)); \
	/* is the first a shifter */ \
	v = is_shifter_operand(op1, &mod); \
	if (v) { \
		op1 = get_irn_n(op1, 0); \
		return new_rd_arm_ ## op(dbg, irg, block, op2, op1, mode, mod, new_tarval_from_long(v, mode_Iu)); \
	} \
	/* is the second a shifter */ \
	v = is_shifter_operand(op2, &mod); \
	if (v) { \
		op2 = get_irn_n(op2, 0); \
		return new_rd_arm_ ## op(dbg, irg, block, op1, op2, mode, mod, new_tarval_from_long(v, mode_Iu)); \
	} \
	/* Normal op */ \
	return new_rd_arm_ ## op(dbg, irg, block, op1, op2, mode, ARM_SHF_NONE, NULL); \
}


/**
 * Creates an arm And.
 *
 * @param env   The transformation environment
 * @return the created arm And node
 */
static ir_node *gen_And(ir_node *irn, arm_code_gen_t *cg);
GEN_INT_OP(And)

/**
 * Creates an arm Orr.
 *
 * @param env   The transformation environment
 * @return the created arm Or node
 */
static ir_node *gen_Or(ir_node *irn, arm_code_gen_t *cg);
GEN_INT_OP(Or)

/**
 * Creates an arm Eor.
 *
 * @param env   The transformation environment
 * @return the created arm Eor node
 */
static ir_node *gen_Eor(ir_node *irn, arm_code_gen_t *cg);
GEN_INT_OP(Eor)

/**
 * Creates an arm Sub.
 *
 * @param env   The transformation environment
 * @return the created arm Sub node
 */
static ir_node *gen_Sub(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_node *op1 = get_Sub_left(irn);
	ir_node *op2 = get_Sub_right(irn);
	int v;
	arm_shift_modifier mod;
	ir_mode *mode = get_irn_mode(irn);
	ir_graph *irg = current_ir_graph;
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (mode_is_float(mode)) {
		cg->have_fp = 1;
		if (USE_FPA(cg->isa))
			return new_rd_arm_fpaSub(dbg, irg, block, op1, op2, mode);
		else if (USE_VFP(cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
		}
		assert(0 && "NYI");
	}
	if (mode_is_numP(mode)) {
		if (is_arm_Mov_i(op1))
			return new_rd_arm_Rsb_i(dbg, irg, block, op2, mode, get_arm_value(op1));
		if (is_arm_Mov_i(op2))
			return new_rd_arm_Sub_i(dbg, irg, block, op1, mode, get_arm_value(op2));

		/* is the first a shifter */
		v = is_shifter_operand(op1, &mod);
		if (v) {
			op1 = get_irn_n(op1, 0);
			return new_rd_arm_Rsb(dbg, irg, block, op2, op1, mode, mod, new_tarval_from_long(v, mode_Iu));
		}
		/* is the second a shifter */
		v = is_shifter_operand(op2, &mod);
		if (v) {
			op2 = get_irn_n(op2, 0);
			return new_rd_arm_Sub(dbg, irg, block, op1, op2, mode, mod, new_tarval_from_long(v, mode_Iu));
		}
		/* normal sub */
		return new_rd_arm_Sub(dbg, irg, block, op1, op2, mode, ARM_SHF_NONE, NULL);
	}
	assert(0 && "unknown mode for sub");
	return NULL;
}

/**
 * Creates an arm Shl.
 *
 * @param env   The transformation environment
 * @return the created arm Shl node
 */
static ir_node *gen_Shl(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *result;
	ir_node *block = get_nodes_block(irn);
	ir_node *op1 = get_Shl_left(irn);
	ir_node *op2 = get_Shl_right(irn);
	ir_mode *mode = get_irn_mode(irn);
	ir_graph *irg = current_ir_graph;
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (is_arm_Mov_i(op2)) {
		result = new_rd_arm_Mov(dbg, irg, block, op1, mode, ARM_SHF_LSL, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shl(dbg, irg, block, op1, op2, mode);
	}
	return result;
}

/**
 * Creates an arm Shr.
 *
 * @param env   The transformation environment
 * @return the created arm Shr node
 */
static ir_node *gen_Shr(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *result;
	ir_node *block = get_nodes_block(irn);
	ir_node *op1 = get_Shr_left(irn);
	ir_node *op2 = get_Shr_right(irn);
	ir_mode *mode = get_irn_mode(irn);
	ir_graph *irg = current_ir_graph;
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (is_arm_Mov_i(op2)) {
		result = new_rd_arm_Mov(dbg, irg, block, op1, mode, ARM_SHF_LSR, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shr(dbg, irg, block, op1, op2, mode);
	}
	return result;
}

/**
 * Creates an arm Shrs.
 *
 * @param env   The transformation environment
 * @return the created arm Shrs node
 */
static ir_node *gen_Shrs(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *result;
	ir_node *block = get_nodes_block(irn);
	ir_node *op1 = get_Shrs_left(irn);
	ir_node *op2 = get_Shrs_right(irn);
	ir_mode *mode = get_irn_mode(irn);
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (is_arm_Mov_i(op2)) {
		result = new_rd_arm_Mov(dbg, current_ir_graph, block, op1, mode, ARM_SHF_ASR, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shrs(dbg, current_ir_graph, block, op1, op2, mode);
	}
	return result;
}

/**
 * Transforms a Not node.
 *
 * @param env   The transformation environment
 * @return the created arm Not node
 */
static ir_node *gen_Not(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_node *op = get_Not_op(irn);
	int v;
	arm_shift_modifier mod = ARM_SHF_NONE;
	tarval  *tv = NULL;
	dbg_info *dbg = get_irn_dbg_info(irn);

	v = is_shifter_operand(op, &mod);
	if (v) {
		op = get_irn_n(op, 0);
		tv = new_tarval_from_long(v, mode_Iu);
	}
	return new_rd_arm_Mvn(dbg, current_ir_graph, block, op, get_irn_mode(irn), mod, tv);
}

/**
 * Transforms an Abs node.
 *
 * @param env   The transformation environment
 * @return the created arm Abs node
 */
static ir_node *gen_Abs(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_node *op = get_Abs_op(irn);
	ir_mode *mode = get_irn_mode(irn);
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (mode_is_float(mode)) {
		cg->have_fp = 1;
		if (USE_FPA(cg->isa))
			return new_rd_arm_fpaAbs(dbg, current_ir_graph, block, op, mode);
		else if (USE_VFP(cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
		}
		assert(0 && "NYI");
	}
	return new_rd_arm_Abs(dbg, current_ir_graph, block, op, mode);
}

/**
 * Transforms a Minus node.
 *
 * @param env   The transformation environment
 * @return the created arm Minus node
 */
static ir_node *gen_Minus(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_node *op = get_Minus_op(irn);
	ir_mode *mode = get_irn_mode(irn);
	ir_graph *irg = current_ir_graph;
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (mode_is_float(mode)) {
		cg->have_fp = 1;
		if (USE_FPA(cg->isa))
			return new_rd_arm_fpaMnv(dbg, irg, block, op, mode);
		else if (USE_VFP(cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
		}
		assert(0 && "NYI");
	}
	return new_rd_arm_Rsb_i(dbg, irg, block, op, mode, get_mode_null(mode));
}

/**
 * Transforms a Load.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Load node
 * @param mode    node mode
 * @return the created arm Load node
 */
static ir_node *gen_Load(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_mode *mode = get_Load_mode(irn);
	ir_graph *irg = current_ir_graph;
	dbg_info *dbg = get_irn_dbg_info(irn);

	if (mode_is_float(mode)) {
		cg->have_fp = 1;
		if (USE_FPA(cg->isa))
			return new_rd_arm_fpaLdf(dbg, irg, block, get_Load_ptr(irn), get_Load_mem(irn),
				get_Load_mode(irn));
		else if (USE_VFP(cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
		}
		assert(0 && "NYI");
	}
	if (mode == mode_Bu) {
		return new_rd_arm_Loadb(dbg, irg, block, get_Load_ptr(irn), get_Load_mem(irn));
	}
	if (mode == mode_Bs) {
		return new_rd_arm_Loadbs(dbg, irg, block, get_Load_ptr(irn), get_Load_mem(irn));
	}
	if (mode == mode_Hu) {
		return new_rd_arm_Loadh(dbg, irg, block, get_Load_ptr(irn), get_Load_mem(irn));
	}
	if (mode == mode_Hs) {
		return new_rd_arm_Loadhs(dbg, irg, block, get_Load_ptr(irn), get_Load_mem(irn));
	}
	if (mode_is_reference(mode)) {
		return new_rd_arm_Load(dbg, irg, block, get_Load_ptr(irn), get_Load_mem(irn));
	}
	return new_rd_arm_Load(dbg, irg, block, get_Load_ptr(irn), get_Load_mem(irn));
}

/**
 * Transforms a Store.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Store node
 * @param mode    node mode
 * @return the created arm Store node
 */
static ir_node *gen_Store(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_mode *mode = get_irn_mode(get_Store_value(irn));
	ir_graph *irg = current_ir_graph;
	dbg_info *dbg = get_irn_dbg_info(irn);

	assert(mode != mode_E && "IEEE Extended FP not supported");
	if (mode_is_float(mode)) {
		cg->have_fp = 1;
		if (USE_FPA(cg->isa))
			return new_rd_arm_fpaStf(dbg, irg, block, get_Store_ptr(irn), get_Store_value(irn),
				get_Store_mem(irn), get_irn_mode(get_Store_value(irn)));
		else if (USE_VFP(cg->isa)) {
			assert(mode != mode_E && "IEEE Extended FP not supported");
		}
		assert(0 && "NYI");
	}
	if (mode == mode_Bu) {
		return new_rd_arm_Storeb(dbg, irg, block, get_Store_ptr(irn), get_Store_value(irn), get_Store_mem(irn));
	}
	if (mode == mode_Bs) {
		return new_rd_arm_Storebs(dbg, irg, block, get_Store_ptr(irn), get_Store_value(irn), get_Store_mem(irn));
	}
	if (mode == mode_Hu) {
		return new_rd_arm_Storeh(dbg, irg, block, get_Store_ptr(irn), get_Store_value(irn), get_Store_mem(irn));
	}
	if (mode == mode_Hs) {
		return new_rd_arm_Storehs(dbg, irg, block, get_Store_ptr(irn), get_Store_value(irn), get_Store_mem(irn));
	}
	return new_rd_arm_Store(dbg, irg, block, get_Store_ptr(irn), get_Store_value(irn), get_Store_mem(irn));
}


static ir_node *gen_Cond(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *result   = NULL;
	ir_node *selector = get_Cond_selector(irn);
	ir_node *block    = get_nodes_block(irn);
	ir_graph *irg     = current_ir_graph;
	dbg_info *dbg     = get_irn_dbg_info(irn);

	if ( get_irn_mode(selector) == mode_b ) {
		//CondJmp
		ir_node *proj_node = get_Cond_selector(irn);
		ir_node *cmp_node = get_Proj_pred(proj_node);
		ir_node *op1 = get_Cmp_left(cmp_node);
		ir_node *op2 = get_Cmp_right(cmp_node);
		result = new_rd_arm_CondJmp(dbg, irg, block, op1, op2, mode_T);
		set_arm_proj_num(result, get_Proj_proj(proj_node));
	} else {
		//SwitchJmp
		ir_node *op = get_irn_n(irn, 0);
		ir_node *const_graph;
		ir_node *sub;
		ir_node *const_node;

		ir_node *proj;
		const ir_edge_t *edge;
		int min = INT_MAX;
		int max = INT_MIN;
		int translation;
		int norm_max;
		int norm_min;
		int pn;
		int n_projs;
		ir_node **projs;

		foreach_out_edge(irn, edge) {
			proj = get_edge_src_irn(edge);
			assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

			pn = get_Proj_proj(proj);

			min = pn<min ? pn : min;
			max = pn>max ? pn : max;
		}
		translation = min;
		norm_max = max - translation;
		norm_min = min - translation;

		n_projs = norm_max + 1;
		projs = xcalloc(n_projs , sizeof(ir_node*));


		foreach_out_edge(irn, edge) {
			proj = get_edge_src_irn(edge);
			assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

			pn = get_Proj_proj(proj) - translation;
			set_Proj_proj(proj, pn);
		}


		const_node = new_rd_Const(dbg, irg, block, mode_Iu, new_tarval_from_long(translation, mode_Iu));
		const_graph = gen_Const(const_node, cg);
		sub = new_rd_arm_Sub(dbg, irg, block, op, const_graph, get_irn_mode(op), ARM_SHF_NONE, NULL);
		result = new_rd_arm_SwitchJmp(dbg, irg, block, sub, mode_T);
		set_arm_n_projs(result, n_projs);
		set_arm_default_proj_num(result, get_Cond_defaultProj(irn)-translation);
	}
	return result;
}

/**
 * Returns the name of a SymConst.
 * @param symc  the SymConst
 * @return name of the SymConst
 */
const char *get_sc_name(ir_node *symc) {
	ir_entity *ent;
	if (get_irn_opcode(symc) != iro_SymConst)
		return "NONE";

	switch (get_SymConst_kind(symc)) {
		case symconst_addr_name:
			return get_id_str(get_SymConst_name(symc));

		case symconst_addr_ent:
			ent = get_SymConst_entity(symc);
			mark_entity_visited(ent);
			return get_entity_ld_name(ent);

		default:
			assert(0 && "Unsupported SymConst");
	}

	return NULL;
}

static ir_node *gen_SymConst(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block = get_nodes_block(irn);
	ir_mode *mode = get_irn_mode(irn);
	dbg_info *dbg = get_irn_dbg_info(irn);
	return new_rd_arm_SymConst(dbg, current_ir_graph, block, mode, get_sc_name(irn));
}



/**
 * Transforms a CopyB node.
 *
 * @param env   The transformation environment
 * @return The transformed node.
 */
static ir_node *gen_CopyB(ir_node *irn, arm_code_gen_t *cg) {
	ir_node  *res   = NULL;
	dbg_info *dbg   = get_irn_dbg_info(irn);
	ir_mode  *mode  = get_irn_mode(irn);
	ir_node  *src   = get_CopyB_src(irn);
	ir_node  *dst   = get_CopyB_dst(irn);
	ir_node  *mem   = get_CopyB_mem(irn);
	ir_node  *block = get_nodes_block(irn);
	int       size  = get_type_size_bytes(get_CopyB_type(irn));
	ir_graph *irg   = current_ir_graph;
	ir_node *src_copy;
	ir_node *dst_copy;

	src_copy = be_new_Copy(&arm_reg_classes[CLASS_arm_gp], irg, block, src);
	dst_copy = be_new_Copy(&arm_reg_classes[CLASS_arm_gp], irg, block, dst);

 	res = new_rd_arm_CopyB( dbg, irg, block, dst_copy, src_copy, new_rd_arm_EmptyReg(dbg, irg, block, mode_Iu), new_rd_arm_EmptyReg(dbg, irg, block, mode_Iu), new_rd_arm_EmptyReg(dbg, irg, block, mode_Iu), mem, mode);
	set_arm_value(res, new_tarval_from_long(size, mode_Iu));

	return res;
}





/********************************************
 *  _                          _
 * | |                        | |
 * | |__   ___ _ __   ___   __| | ___  ___
 * | '_ \ / _ \ '_ \ / _ \ / _` |/ _ \/ __|
 * | |_) |  __/ | | | (_) | (_| |  __/\__ \
 * |_.__/ \___|_| |_|\___/ \__,_|\___||___/
 *
 ********************************************/

/**
 * Return an expanding stack offset.
 * Note that function is called in the transform phase
 * where the stack offsets are still relative regarding
 * the first (frame allocating) IncSP.
 * However this is exactly what we want because frame
 * access must be done relative the the fist IncSP ...
 */
static int get_sp_expand_offset(ir_node *inc_sp) {
	int offset = be_get_IncSP_offset(inc_sp);

	if (offset == BE_STACK_FRAME_SIZE_EXPAND)
		return 0;

	return offset;
}

#if 0
static ir_node *gen_StackParam(ir_node *irn, arm_code_gen_t *cg) {
	ir_node   *new_op = NULL;
	ir_node   *block  = get_nodes_block(irn);
	ir_node   *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node   *mem    = new_rd_NoMem(env->irg);
	ir_node   *ptr    = get_irn_n(irn, 0);
	ir_entity *ent    = be_get_frame_entity(irn);
	ir_mode   *mode   = env->mode;

//	/* If the StackParam has only one user ->     */
//	/* put it in the Block where the user resides */
//	if (get_irn_n_edges(node) == 1) {
//		env->block = get_nodes_block(get_edge_src_irn(get_irn_out_edge_first(node)));
//	}

	if (mode_is_float(mode)) {
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_fLoad(env->dbg, env->irg, block, ptr, noreg, mem, mode_T);
		else {
			env->cg->used_x87 = 1;
			new_op = new_rd_ia32_vfld(env->dbg, env->irg, block, ptr, noreg, mem, mode_T);
		}
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, env->irg, block, ptr, noreg, mem, mode_T);
	}

	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_rd_Proj(env->dbg, env->irg, block, new_op, mode, 0);
}
#endif

/**
 * Transforms a FrameAddr into an ia32 Add.
 */
static ir_node *gen_be_FrameAddr(ir_node *irn, arm_code_gen_t *cg) {
	ir_node *block  = get_nodes_block(irn);
	ir_entity *ent  = be_get_frame_entity(irn);
	int     offset  = get_entity_offset(ent);
	ir_node *op     = get_irn_n(irn, 0);
	ir_node *cnst;
	ir_mode *mode   = get_irn_mode(irn);
	dbg_info *dbg   = get_irn_dbg_info(irn);

	if (be_is_IncSP(op)) {
		/* BEWARE: we get an offset which is absolute from an offset that
		   is relative. Both must be merged */
		offset += get_sp_expand_offset(op);
	}
	cnst = create_const_graph_value(irn, block, (unsigned)offset);
	if (is_arm_Mov_i(cnst))
		return new_rd_arm_Add_i(dbg, current_ir_graph, block, op, mode, get_arm_value(cnst));
	return new_rd_arm_Add(dbg, current_ir_graph, block, op, cnst, mode, ARM_SHF_NONE, NULL);
}

#if 0
/**
 * Transforms a FrameLoad into an ia32 Load.
 */
static ir_node *gen_FrameLoad(ir_node *irn, arm_code_gen_t *cg) {
	ir_node   *new_op = NULL;
	ir_node   *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node   *mem    = get_irn_n(irn, 0);
	ir_node   *ptr    = get_irn_n(irn, 1);
	ir_entity *ent    = be_get_frame_entity(irn);
	ir_mode   *mode   = get_type_mode(get_entity_type(ent));

	if (mode_is_float(mode)) {
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_fLoad(env->dbg, current_ir_graph, env->block, ptr, noreg, mem, mode_T);
		else {
			env->cg->used_x87 = 1;
			new_op = new_rd_ia32_vfld(env->dbg, current_ir_graph, env->block, ptr, noreg, mem, mode_T);
		}
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, current_ir_graph, env->block, ptr, noreg, mem, mode_T);
	}

	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_op;
}
#endif

#if 0
/**
 * Transforms a FrameStore into an ia32 Store.
 */
static ir_node *gen_FrameStore(ir_node *irn, arm_code_gen_t *cg) {
	ir_node   *new_op = NULL;
	ir_node   *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node   *mem    = get_irn_n(irn, 0);
	ir_node   *ptr    = get_irn_n(irn, 1);
	ir_node   *val    = get_irn_n(irn, 2);
	ir_entity *ent    = be_get_frame_entity(irn);
	ir_mode   *mode   = get_irn_mode(val);

	if (mode_is_float(mode)) {
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_fStore(env->dbg, env->irg, env->block, ptr, noreg, val, mem, mode_T);
		else {
			env->cg->used_x87 = 1;
			new_op = new_rd_ia32_vfst(env->dbg, env->irg, env->block, ptr, noreg, val, mem, mode_T);
		}
	}
	else if (get_mode_size_bits(mode) == 8) {
		new_op = new_rd_ia32_Store8Bit(env->dbg, env->irg, env->block, ptr, noreg, val, mem, mode_T);
	}
	else {
		new_op = new_rd_ia32_Store(env->dbg, env->irg, env->block, ptr, noreg, val, mem, mode_T);
	}

	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	set_ia32_am_support(new_op, ia32_am_Dest);
	set_ia32_op_type(new_op, ia32_AddrModeD);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_op;
}
#endif


// static ir_node *gen_be_Copy(ir_node *irn, arm_code_gen_t *cg) {
// 	return new_rd_arm_Copy(env->dbg, env->irg, env->block, op, env->mode);
// }

/*********************************************************
 *                  _             _      _
 *                 (_)           | |    (_)
 *  _ __ ___   __ _ _ _ __     __| |_ __ ___   _____ _ __
 * | '_ ` _ \ / _` | | '_ \   / _` | '__| \ \ / / _ \ '__|
 * | | | | | | (_| | | | | | | (_| | |  | |\ V /  __/ |
 * |_| |_| |_|\__,_|_|_| |_|  \__,_|_|  |_| \_/ \___|_|
 *
 *********************************************************/

/**
 * move constants out of the start block
 */
void arm_move_consts(ir_node *node, void *env) {
	int i;

	if (is_Block(node))
		return;

	if (is_Phi(node)) {
		for (i = get_irn_arity(node) - 1; i >= 0; --i) {
			ir_node *pred = get_irn_n(node,i);
			ir_opcode pred_code = get_irn_opcode(pred);
			if (pred_code == iro_Const) {
				ir_node *const_graph;
				const_graph = create_const_graph(pred, get_nodes_block(get_irn_n(get_nodes_block(node),i)));
				set_irn_n(node, i, const_graph);
			}
			else if (pred_code == iro_SymConst) {
				/* FIXME: in general, SymConst always require a load, so it
				   might be better to place them into the first real block
				   and let the spiller rematerialize them. */
				const char *str = get_sc_name(pred);
				ir_node *symconst_node;
				symconst_node = new_rd_arm_SymConst(get_irn_dbg_info(pred),
					current_ir_graph, get_nodes_block(get_irn_n(get_nodes_block(node),i)),
					get_irn_mode(pred), str);
				set_irn_n(node, i, symconst_node);
			}
		}
		return;
	}
	for (i = 0; i < get_irn_arity(node); i++) {
		ir_node *pred = get_irn_n(node,i);
		ir_opcode pred_code = get_irn_opcode(pred);
		if (pred_code == iro_Const) {
			ir_node *const_graph;
			const_graph = create_const_graph(pred, get_nodes_block(node));
			set_irn_n(node, i, const_graph);
		} else if (pred_code == iro_SymConst) {
			const char *str = get_sc_name(pred);
			ir_node *symconst_node;
			symconst_node = new_rd_arm_SymConst(get_irn_dbg_info(pred),
				current_ir_graph, get_nodes_block(node),
				get_irn_mode(pred), str);
			set_irn_n(node, i, symconst_node);
		}
	}
}


/************************************************************************/
/* move symbolic constants out of startblock                            */
/************************************************************************/
void arm_move_symconsts(ir_node *node, void *env) {
	int i;

	if (is_Block(node))
		return;

	for (i = 0; i < get_irn_arity(node); i++) {
		ir_node *pred       = get_irn_n(node,i);
		ir_opcode pred_code = get_irn_opcode(pred);

		if (pred_code == iro_SymConst) {
			const char *str = get_sc_name(pred);
			ir_node    *symconst_node;

			symconst_node = new_rd_arm_SymConst(get_irn_dbg_info(pred),
				current_ir_graph, get_nodes_block(node), get_irn_mode(pred), str);
			set_irn_n(node, i, symconst_node);
		}
	}
}

/**
 * the BAD transformer.
 */
static ir_node *bad_transform(ir_node *irn, arm_code_gen_t *cg) {
	ir_fprintf(stderr, "Not implemented: %+F\n", irn);
	assert(0);
	return NULL;
}

/**
 * Enters all transform functions into the generic pointer
 */
void arm_register_transformers(void) {
	ir_op *op_Max, *op_Min, *op_Mulh;

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

#define FIRM_OP(a)     op_##a->ops.generic = (op_func)gen_##a
#define BAD(a)         op_##a->ops.generic = (op_func)bad_transform
#define IGN(a)

	FIRM_OP(Add);  // done
	FIRM_OP(Mul);  // done
	FIRM_OP(Quot); // done
	FIRM_OP(And);  // done
	FIRM_OP(Or);   // done
	FIRM_OP(Eor);  // done

	FIRM_OP(Sub);  // done
	FIRM_OP(Shl);  // done
	FIRM_OP(Shr);  // done
	FIRM_OP(Shrs); // done

	FIRM_OP(Minus); // done
	FIRM_OP(Not);   // done
	FIRM_OP(Abs);   // done

	FIRM_OP(CopyB); // done
	FIRM_OP(Const); // TODO: floating point consts
	FIRM_OP(Conv); // TODO: floating point conversions

	FIRM_OP(Load);   // done
	FIRM_OP(Store);  // done

	FIRM_OP(SymConst);
	FIRM_OP(Cond);	  // integer done

	/* TODO: implement these nodes */

	IGN(Div);    // intrinsic lowering
	IGN(Mod);    // intrinsic lowering
	IGN(DivMod); // TODO: implement DivMod

	IGN(Mux);
	IGN(Unknown);
	IGN(Cmp);     // done, implemented in cond

	/* You probably don't need to handle the following nodes */

	IGN(Call);
	IGN(Proj);
	IGN(Alloc);

	IGN(Block);
	IGN(Start);
	IGN(End);
	IGN(NoMem);
	IGN(Phi);
	IGN(IJmp);
	IGN(Jmp);     // emitter done
	IGN(Break);
	IGN(Sync);

	BAD(Raise);
	BAD(Sel);
	BAD(InstOf);
	BAD(Cast);
	BAD(Free);
	BAD(Tuple);
	BAD(Id);
	BAD(Bad);
	BAD(Confirm);
	BAD(Filter);
	BAD(CallBegin);
	BAD(EndReg);
	BAD(EndExcept);

	FIRM_OP(be_FrameAddr);

	op_Max = get_op_Max();
	if (op_Max)
		BAD(Max);
	op_Min = get_op_Min();
	if (op_Min)
		BAD(Min);
	op_Mulh = get_op_Mulh();
	if (op_Mulh)
		BAD(Mulh);

#undef IGN
#undef FIRM_OP
#undef BAD
}

typedef ir_node *(transform_func)(ir_node *irn, arm_code_gen_t *cg);

/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void arm_transform_node(ir_node *node, void *env) {
	arm_code_gen_t *cg = (arm_code_gen_t *)env;
	ir_op *op          = get_irn_op(node);
	ir_node *asm_node  = NULL;

	if (op == op_Block)
		return;

	DBG((cg->mod, LEVEL_1, "check %+F ... ", node));

	if (op->ops.generic) {
		transform_func *transform = (transform_func *)op->ops.generic;

		asm_node = (*transform)(node, cg);
	}

	if (asm_node) {
		exchange(node, asm_node);
		DB((cg->mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DB((cg->mod, LEVEL_1, "ignored\n"));
	}
}
