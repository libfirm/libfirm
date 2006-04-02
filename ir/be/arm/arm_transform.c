/* The codegenrator (transform FIRM into arm FIRM */
/* $Id$ */

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

/** Execute ROL. */
static unsigned do_rol(unsigned v, unsigned rol)
{
	return (v << rol) | (v >> (32 - rol));
}

typedef struct vals_ {
	int ops;
	unsigned char values[4];
	unsigned char shifts[4];
} vals;

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
static ir_node *create_const_node(arm_transform_env_t *env, long value) {
    tarval *tv = new_tarval_from_long(value, mode_Iu);
	return new_rd_arm_Mov_i(env->dbg, env->irg, env->block, env->mode, tv);
}

/**
 * Creates a arm_Const_Neg node.
 */
static ir_node *create_const_neg_node(arm_transform_env_t *env, long value) {
    tarval *tv = new_tarval_from_long(value, mode_Iu);
	return new_rd_arm_Mvn_i(env->dbg, env->irg, env->block, env->mode, tv);
}

#define NEW_BINOP_NODE(opname, env, op1, op2) new_rd_arm_##opname(env->dbg, env->irg, env->block, op1, op2, env->mode)

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
static ir_node *create_const_graph_value(arm_transform_env_t *env, unsigned int value) {
	ir_node *result;
	int negate = 0;
	vals v, vn;
	int cnt;

	gen_vals_from_word(value, &v);
	gen_vals_from_word(~value, &vn);

	if (vn.ops < v.ops) {
		/* remove bits */
		result = create_const_neg_node(env, arm_encode_imm_w_shift(vn.shifts[0], vn.values[0]));

		for (cnt = 1; cnt < vn.ops; ++cnt) {
			tarval *tv = new_tarval_from_long(arm_encode_imm_w_shift(vn.shifts[cnt], vn.values[cnt]), mode_Iu);
			ir_node *bic_i_node = new_rd_arm_Bic_i(env->dbg, env->irg, env->block, result, env->mode, tv);
			result = bic_i_node;
		}
	}
	else {
		/* add bits */
		result = create_const_node(env, arm_encode_imm_w_shift(v.shifts[0], v.values[0]));

		for (cnt = 1; cnt < v.ops; ++cnt) {
			tarval *tv = new_tarval_from_long(arm_encode_imm_w_shift(v.shifts[cnt], v.values[cnt]), mode_Iu);
			ir_node *orr_i_node = new_rd_arm_Or_i(env->dbg, env->irg, env->block, result, env->mode, tv);
			result = orr_i_node;
		}
	}
	return result;
}

static ir_node *create_const_graph(arm_transform_env_t *env) {
	int value = get_tarval_long(get_Const_tarval(env->irn));
	return create_const_graph_value(env, value);
}



static ir_node *gen_Const(arm_transform_env_t *env) {
	ir_node *result;
	assert(env->mode != mode_E && "IEEE Extended FP not supported");
	if (env->mode == mode_F) {
		result = new_rd_arm_fConst(env->dbg, env->irg, env->block, env->mode);
		get_arm_attr(result)->value = get_Const_tarval(env->irn);
	} else if (env->mode == mode_D) {
		result = new_rd_arm_fConst(env->dbg, env->irg, env->block, env->mode);
		get_arm_attr(result)->value = get_Const_tarval(env->irn);
	} else if (env->mode == mode_P) {
		return env->irn;
	} else {
		result = create_const_graph(env);
	}
	return result;
}

static ir_node *gen_mask(arm_transform_env_t *env, ir_node *op, int result_bits) {
	unsigned mask_bits = (1 << result_bits) - 1;
	ir_node *mask_node = create_const_graph_value(env, mask_bits);
	return new_rd_arm_And(env->dbg, env->irg, env->block, op, mask_node, get_irn_mode(env->irn), ARM_SHF_NONE, NULL);
}

static ir_node *gen_sign_extension(arm_transform_env_t *env, ir_node *op, int result_bits) {
	int shift_width = 32 - result_bits;
	ir_node *shift_const_node = create_const_graph_value(env, shift_width);
	ir_node *lshift_node = new_rd_arm_Shl(env->dbg, env->irg, env->block, op, shift_const_node, get_irn_mode(op));
	ir_node *rshift_node = new_rd_arm_Shrs(env->dbg, env->irg, env->block, lshift_node, shift_const_node, get_irn_mode(env->irn));
	return rshift_node;
}

/**
 * Transforms a Conv node.
 *
 * @param env   The transformation environment
 * @return the created arm Conv node
 */
static ir_node *gen_Conv(arm_transform_env_t *env) {
	ir_node *op      = get_Conv_op(env->irn);
	ir_mode *in_mode = get_irn_mode(op);
	ir_mode *out_mode = env->mode;

	assert( in_mode != mode_E && "");
	assert( in_mode != mode_Ls && "");
	assert( in_mode != mode_Lu && "");
	assert( out_mode != mode_E && "");
	assert( out_mode != mode_Ls && "");
	assert( out_mode != mode_Lu && "");

	if (in_mode == out_mode)
		return op;

	if ((mode_is_int(in_mode) || mode_is_reference(in_mode))
		&& (mode_is_reference(out_mode) || mode_is_int(out_mode))) {
		int in_bits = get_mode_size_bits(in_mode);
		int out_bits = get_mode_size_bits(out_mode);
		int in_sign = get_mode_sign(in_mode);
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
				return gen_mask(env, op, out_bits);
			} else {
				return gen_sign_extension(env, op, out_bits);
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
				return gen_sign_extension(env, op, out_bits);
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
				return gen_sign_extension(env, op, out_bits);
			} else {
				return gen_mask(env, op, out_bits);
			}
		}
		assert(0 && "recheck integer conversion logic!");
		return env->irn;
	} else if (in_mode == mode_D && out_mode == mode_F) {
		return new_rd_arm_fConvD2S(env->dbg, env->irg, env->block, op, env->mode);
	} else if (in_mode == mode_F && out_mode == mode_D) {
		return new_rd_arm_fConvS2D(env->dbg, env->irg, env->block, op, env->mode);
	} else if (mode_is_int(in_mode) && mode_is_float(out_mode)) {
		env->cg->have_fp = 1;
		return env->irn; /* TODO: implement int->float conversion*/
	} else if (mode_is_float(in_mode) && mode_is_int(out_mode)) {
		env->cg->have_fp = 1;
		return env->irn; /* TODO: implement float->int conversion*/
	} else {
		assert(0 && "not implemented conversion");
		return env->irn;
	}
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
static ir_node *gen_Add(arm_transform_env_t *env) {
	ir_node *irn = env->irn;
	ir_node *op1 = get_Add_left(irn);
	ir_node *op2 = get_Add_right(irn);
	ir_node *op3;
	int v;
	arm_shift_modifier mod;

	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (mode_is_float(env->mode)) {
		env->cg->have_fp = 1;
		return new_rd_arm_fAdd(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	if (mode_is_numP(env->mode)) {
		if (is_arm_Mov_i(op1))
			return new_rd_arm_Add_i(env->dbg, env->irg, env->block, op2, env->mode,
			                        get_arm_value(op1));
		if (is_arm_Mov_i(op2))
			return new_rd_arm_Add_i(env->dbg, env->irg, env->block, op1, env->mode,
			                        get_arm_value(op2));

		/* check for MLA */
		if (is_arm_Mul(op1) && get_irn_n_edges(op1) == 1) {
			op3 = op2;
			op2 = get_irn_n(op1, 1);
			op1 = get_irn_n(op1, 0);

			return new_rd_arm_Mla(env->dbg, env->irg, env->block, op1, op2, op3, env->mode);
		}
		if (is_arm_Mul(op2) && get_irn_n_edges(op2) == 1) {
			op3 = op1;
			op1 = get_irn_n(op2, 0);
			op2 = get_irn_n(op2, 1);

			return new_rd_arm_Mla(env->dbg, env->irg, env->block, op1, op2, op3, env->mode);
		}

		/* is the first a shifter */
		v = is_shifter_operand(op1, &mod);
		if (v) {
			op1 = get_irn_n(op1, 0);
			return new_rd_arm_Add(env->dbg, env->irg, env->block, op2, op1, env->mode,
			                      mod, new_tarval_from_long(v, mode_Iu));
		}
		/* is the second a shifter */
		v = is_shifter_operand(op2, &mod);
		if (v) {
			op2 = get_irn_n(op2, 0);
			return new_rd_arm_Add(env->dbg, env->irg, env->block, op1, op2, env->mode,
			                      mod, new_tarval_from_long(v, mode_Iu));
		}

		/* normal ADD */
		return new_rd_arm_Add(env->dbg, env->irg, env->block, op1, op2, env->mode, ARM_SHF_NONE, NULL);
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
static ir_node *gen_Mul(arm_transform_env_t *env) {
	ir_node *irn = env->irn;
	ir_node *op1 = get_Mul_left(irn);
	ir_node *op2 = get_Mul_right(irn);

	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (mode_is_float(env->mode)) {
		env->cg->have_fp = 1;
		return new_rd_arm_fMul(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return new_rd_arm_Mul(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

/**
 * Creates an arm floating point Div.
 *
 * @param env   The transformation environment
 * @return the created arm fDiv node
 */
static ir_node *gen_Quot(arm_transform_env_t *env) {
	ir_node *irn = env->irn;
	ir_node *op1 = get_Quot_left(irn);
	ir_node *op2 = get_Quot_right(irn);

	assert(mode_is_float(get_irn_mode(op1)));
	assert(get_irn_mode(op1) != mode_E && "IEEE Extended FP not supported");

	return new_rd_arm_fDiv(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

#define GEN_INT_OP(op) \
static ir_node *gen_ ## op(arm_transform_env_t *env) { \
	ir_node *irn = env->irn; \
	ir_node *op1 = get_ ## op ## _left(irn); \
	ir_node *op2 = get_ ## op ## _right(irn); \
	int v; \
	arm_shift_modifier mod; \
 \
	if (is_arm_Mov_i(op1)) \
		return new_rd_arm_ ## op ## _i(env->dbg, env->irg, env->block, op2, env->mode, \
		                        get_arm_value(op1)); \
	if (is_arm_Mov_i(op2)) \
		return new_rd_arm_ ## op ## _i(env->dbg, env->irg, env->block, op1, env->mode, \
		                        get_arm_value(op2)); \
	/* is the first a shifter */ \
	v = is_shifter_operand(op1, &mod); \
	if (v) { \
		op1 = get_irn_n(op1, 0); \
		return new_rd_arm_ ## op(env->dbg, env->irg, env->block, op2, op1, env->mode, \
			                    mod, new_tarval_from_long(v, mode_Iu)); \
	} \
	/* is the second a shifter */ \
	v = is_shifter_operand(op2, &mod); \
	if (v) { \
		op2 = get_irn_n(op2, 0); \
		return new_rd_arm_ ## op(env->dbg, env->irg, env->block, op1, op2, env->mode, \
			                    mod, new_tarval_from_long(v, mode_Iu)); \
	} \
	/* Normal op */ \
	return new_rd_arm_ ## op(env->dbg, env->irg, env->block, op1, op2, env->mode, ARM_SHF_NONE, NULL); \
}


/**
 * Creates an arm And.
 *
 * @param env   The transformation environment
 * @return the created arm And node
 */
static ir_node *gen_And(arm_transform_env_t *env);
GEN_INT_OP(And)

/**
 * Creates an arm Orr.
 *
 * @param env   The transformation environment
 * @return the created arm Or node
 */
static ir_node *gen_Or(arm_transform_env_t *env);
GEN_INT_OP(Or)

/**
 * Creates an arm Eor.
 *
 * @param env   The transformation environment
 * @return the created arm Eor node
 */
static ir_node *gen_Eor(arm_transform_env_t *env);
GEN_INT_OP(Eor)

/**
 * Creates an arm Sub.
 *
 * @param env   The transformation environment
 * @return the created arm Sub node
 */
static ir_node *gen_Sub(arm_transform_env_t *env) {
	ir_node *irn = env->irn;
	ir_node *op1 = get_Sub_left(irn);
	ir_node *op2 = get_Sub_right(irn);
	int v;
	arm_shift_modifier mod;

	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (mode_is_float(env->mode)) {
		env->cg->have_fp = 1;
		return new_rd_arm_fSub(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	if (mode_is_numP(env->mode)) {
		if (is_arm_Mov_i(op1))
			return new_rd_arm_Rsb_i(env->dbg, env->irg, env->block, op2, env->mode,
			                        get_arm_value(op1));
		if (is_arm_Mov_i(op2))
			return new_rd_arm_Sub_i(env->dbg, env->irg, env->block, op1, env->mode,
			                        get_arm_value(op2));

		/* is the first a shifter */
		v = is_shifter_operand(op1, &mod);
		if (v) {
			op1 = get_irn_n(op1, 0);
			return new_rd_arm_Rsb(env->dbg, env->irg, env->block, op2, op1, env->mode,
			                      mod, new_tarval_from_long(v, mode_Iu));
		}
		/* is the second a shifter */
		v = is_shifter_operand(op2, &mod);
		if (v) {
			op2 = get_irn_n(op2, 0);
			return new_rd_arm_Sub(env->dbg, env->irg, env->block, op1, op2, env->mode,
			                      mod, new_tarval_from_long(v, mode_Iu));
		}
		/* normal sub */
		return new_rd_arm_Sub(env->dbg, env->irg, env->block, op1, op2, env->mode, ARM_SHF_NONE, NULL);
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
static ir_node *gen_Shl(arm_transform_env_t *env) {
	ir_node *result;
	ir_node *irn = env->irn;
	ir_node *op1 = get_Shl_left(irn);
	ir_node *op2 = get_Shl_right(irn);

	if (is_arm_Mov_i(op2)) {
		result = new_rd_arm_Mov(env->dbg, env->irg, env->block, op1, env->mode,
		                        ARM_SHF_LSL, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shl(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}

/**
 * Creates an arm Shr.
 *
 * @param env   The transformation environment
 * @return the created arm Shr node
 */
static ir_node *gen_Shr(arm_transform_env_t *env) {
	ir_node *result;
	ir_node *irn = env->irn;
	ir_node *op1 = get_Shr_left(irn);
	ir_node *op2 = get_Shr_right(irn);

	if (is_arm_Mov_i(op2)) {
		result = new_rd_arm_Mov(env->dbg, env->irg, env->block, op1, env->mode,
		                        ARM_SHF_LSR, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shr(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}

/**
 * Creates an arm Shrs.
 *
 * @param env   The transformation environment
 * @return the created arm Shrs node
 */
static ir_node *gen_Shrs(arm_transform_env_t *env) {
	ir_node *result;
	ir_node *irn = env->irn;
	ir_node *op1 = get_Shrs_left(irn);
	ir_node *op2 = get_Shrs_right(irn);

	if (is_arm_Mov_i(op2)) {
		result = new_rd_arm_Mov(env->dbg, env->irg, env->block, op1, env->mode,
		                        ARM_SHF_ASR, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shrs(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}

/**
 * Transforms a Not node.
 *
 * @param env   The transformation environment
 * @return the created arm Not node
 */
static ir_node *gen_Not(arm_transform_env_t *env) {
	ir_node *op = get_Not_op(env->irn);
	int v;
	arm_shift_modifier mod = ARM_SHF_NONE;
	tarval  *tv = NULL;

	v = is_shifter_operand(op, &mod);
	if (v) {
		op = get_irn_n(op, 0);
		tv = new_tarval_from_long(v, mode_Iu);
	}
	return new_rd_arm_Mvn(env->dbg, env->irg, env->block, op, env->mode, mod, tv);
}

/**
 * Transforms an Abs node.
 *
 * @param env   The transformation environment
 * @return the created arm Abs node
 */
static ir_node *gen_Abs(arm_transform_env_t *env) {
	ir_node *op = get_Abs_op(env->irn);

	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (mode_is_float(env->mode)) {
		env->cg->have_fp = 1;
		return new_rd_arm_fAbs(env->dbg, env->irg, env->block, op, env->mode);
	}

	return new_rd_arm_Abs(env->dbg, env->irg, env->block, op, env->mode);
}

/**
 * Transforms a Minus node.
 *
 * @param env   The transformation environment
 * @return the created arm Minus node
 */
static ir_node *gen_Minus(arm_transform_env_t *env) {
	ir_node *op = get_Minus_op(env->irn);

	if (mode_is_float(env->mode)) {
		return new_rd_arm_fMinus(env->dbg, env->irg, env->block, op, env->mode);
	}
	return new_rd_arm_Rsb_i(env->dbg, env->irg, env->block, op, env->mode, get_mode_null(env->mode));
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
static ir_node *gen_Load(arm_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_mode *mode = get_Load_mode(node);

	if (mode_is_float(mode)) {
		env->cg->have_fp = 1;
		/* FIXME: set the load mode */
		return new_rd_arm_fLoad(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	if (mode == mode_Bu) {
		return new_rd_arm_Loadb(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	if (mode == mode_Bs) {
		return new_rd_arm_Loadbs(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	if (mode == mode_Hu) {
		return new_rd_arm_Loadh(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	if (mode == mode_Hs) {
		return new_rd_arm_Loadhs(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	if (mode_is_reference(mode)) {
		return new_rd_arm_Load(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	return new_rd_arm_Load(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
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
static ir_node *gen_Store(arm_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_mode *mode = get_irn_mode(get_Store_value(node));
	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (mode_is_float(mode)) {
		env->cg->have_fp = 1;
		/* FIXME: set the store mode */
		return new_rd_arm_fStore(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	if (mode == mode_Bu) {
		return new_rd_arm_Storeb(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	if (mode == mode_Bs) {
		return new_rd_arm_Storebs(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	if (mode == mode_Hu) {
		return new_rd_arm_Storeh(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	if (mode == mode_Hs) {
		return new_rd_arm_Storehs(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	return new_rd_arm_Store(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
}


static ir_node *gen_Cond(arm_transform_env_t *env) {
	ir_node *result   = NULL;
	ir_node *selector = get_Cond_selector(env->irn);
	ir_node *irn      = env->irn;

	if ( get_irn_mode(selector) == mode_b ) {
		//CondJmp
		ir_node *proj_node = get_Cond_selector(irn);
		ir_node *cmp_node = get_Proj_pred(proj_node);
		ir_node *op1 = get_Cmp_left(cmp_node);
		ir_node *op2 = get_Cmp_right(cmp_node);
		result = new_rd_arm_CondJmp(env->dbg, env->irg, env->block, op1, op2, mode_T);
		set_arm_proj_num(result, get_Proj_proj(proj_node));
	} else {
		//SwitchJmp
		ir_node *op = get_irn_n(env->irn, 0);
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
		arm_transform_env_t const_env;
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


		const_node = new_rd_Const(env->dbg, env->irg, env->block, mode_Iu, new_tarval_from_long(translation, mode_Iu));
		const_env = *env;
		const_env.mode = mode_Is;
		const_env.irn = const_node;
		const_graph = gen_Const(&const_env);
		sub = new_rd_arm_Sub(env->dbg, env->irg, env->block, op, const_graph, get_irn_mode(op), ARM_SHF_NONE, NULL);
		result = new_rd_arm_SwitchJmp(env->dbg, env->irg, env->block, sub, mode_T);
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
	if (get_irn_opcode(symc) != iro_SymConst)
		return "NONE";

	switch (get_SymConst_kind(symc)) {
		case symconst_addr_name:
			return get_id_str(get_SymConst_name(symc));

		case symconst_addr_ent:
			return get_entity_ld_name(get_SymConst_entity(symc));

		default:
			assert(0 && "Unsupported SymConst");
	}

	return NULL;
}

static ir_node *gen_SymConst(arm_transform_env_t *env) {
	return new_rd_arm_SymConst(env->dbg, env->irg, env->block,
	                           env->mode, get_sc_name(env->irn));
}



/**
 * Transforms a CopyB node.
 *
 * @param env   The transformation environment
 * @return The transformed node.
 */
static ir_node *gen_CopyB(arm_transform_env_t *env) {
	ir_node  *res   = NULL;
	dbg_info *dbg   = env->dbg;
	ir_graph *irg   = env->irg;
	ir_mode  *mode  = env->mode;
	ir_node  *block = env->block;
	ir_node  *node  = env->irn;
	ir_node  *src   = get_CopyB_src(node);
	ir_node  *dst   = get_CopyB_dst(node);
	ir_node  *mem   = get_CopyB_mem(node);
	int       size  = get_type_size_bytes(get_CopyB_type(node));
	ir_node *src_copy;
	ir_node *dst_copy;

	arm_transform_env_t const_env;
	const_env.block    = block;
	const_env.dbg      = dbg;
	const_env.irg      = irg;
	const_env.irn      = node;
	DEBUG_ONLY(const_env.mod      = env->mod;)
	const_env.mode     = mode_Iu;

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
	unsigned offset    = be_get_IncSP_offset(inc_sp);
	be_stack_dir_t dir = be_get_IncSP_direction(inc_sp);

	if (offset == BE_STACK_FRAME_SIZE)
		return 0;
	return dir == be_stack_dir_expand ? (int)offset : -(int)offset;
}

static ir_node *gen_StackParam(arm_transform_env_t *env) {
#if 0
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *mem    = new_rd_NoMem(env->irg);
	ir_node *ptr    = get_irn_n(node, 0);
	entity  *ent    = be_get_frame_entity(node);
	ir_mode *mode   = env->mode;

//	/* If the StackParam has only one user ->     */
//	/* put it in the Block where the user resides */
//	if (get_irn_n_edges(node) == 1) {
//		env->block = get_nodes_block(get_edge_src_irn(get_irn_out_edge_first(node)));
//	}

	if (mode_is_float(mode)) {
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_fLoad(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
		else {
			env->cg->used_x87 = 1;
			new_op = new_rd_ia32_vfld(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
		}
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
	}

	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_rd_Proj(env->dbg, env->irg, env->block, new_op, mode, 0);
#endif
}

/**
 * Transforms a FrameAddr into an ia32 Add.
 */
static ir_node *gen_be_FrameAddr(arm_transform_env_t *env) {
	ir_node *node   = env->irn;
	entity  *ent    = be_get_frame_entity(node);
	int     offset  = get_entity_offset_bytes(ent);
	ir_node *op     = get_irn_n(node, 0);
	ir_node *cnst;

	if (be_is_IncSP(op)) {
		/* BEWARE: we get an offset which is absolute from an offset that
		   is relative. Both must be merged */
		offset += get_sp_expand_offset(op);
	}
	cnst = create_const_graph_value(env, (unsigned)offset);
	if (is_arm_Mov_i(cnst)) {
		return new_rd_arm_Add_i(env->dbg, env->irg, env->block, op, env->mode,
		                        get_arm_value(cnst));
	}
	return new_rd_arm_Add(env->dbg, env->irg, env->block, op, cnst, env->mode, ARM_SHF_NONE, NULL);
}

/**
 * Transforms a FrameLoad into an ia32 Load.
 */
static ir_node *gen_FrameLoad(arm_transform_env_t *env) {
#if 0
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *mem    = get_irn_n(node, 0);
	ir_node *ptr    = get_irn_n(node, 1);
	entity  *ent    = be_get_frame_entity(node);
	ir_mode *mode   = get_type_mode(get_entity_type(ent));

	if (mode_is_float(mode)) {
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_fLoad(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
		else {
			env->cg->used_x87 = 1;
			new_op = new_rd_ia32_vfld(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
		}
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
	}

	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_op;
#endif
}


/**
 * Transforms a FrameStore into an ia32 Store.
 */
static ir_node *gen_FrameStore(arm_transform_env_t *env) {
#if 0
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *mem    = get_irn_n(node, 0);
	ir_node *ptr    = get_irn_n(node, 1);
	ir_node *val    = get_irn_n(node, 2);
	entity  *ent    = be_get_frame_entity(node);
	ir_mode *mode   = get_irn_mode(val);

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
#endif
}


// static ir_node *gen_be_Copy(arm_transform_env_t *env, ir_node *op) {
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

/************************************************************************/
/* move constants out of startblock                                       */
/************************************************************************/
void arm_move_consts(ir_node *node, void *env) {
	arm_code_gen_t *cgenv = (arm_code_gen_t *)env;
	arm_transform_env_t tenv;
	int i;

	if (is_Block(node))
		return;

	tenv.irg = current_ir_graph;
	DEBUG_ONLY(tenv.mod = cgenv->mod);

	if (is_Phi(node)) {
		for (i = 0; i < get_irn_arity(node); i++) {
			ir_node *pred = get_irn_n(node,i);
			opcode pred_code = get_irn_opcode(pred);
			if (pred_code == iro_Const) {
				ir_node *const_graph;
				tenv.block    = get_nodes_block(get_irn_n(get_nodes_block(node),i));
				tenv.dbg      = get_irn_dbg_info(pred);
				tenv.irn      = pred;
				tenv.mode     = get_irn_mode(pred);
				const_graph = create_const_graph(&tenv);
				set_irn_n(node, i, const_graph);
			} else if (pred_code == iro_SymConst) {
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
		opcode pred_code = get_irn_opcode(pred);
		if (pred_code == iro_Const) {
			ir_node *const_graph;
			tenv.block  = get_nodes_block(node);
			tenv.dbg    = get_irn_dbg_info(pred);
			tenv.irn    = pred;
			tenv.mode   = get_irn_mode(pred);
			const_graph = create_const_graph(&tenv);
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
		ir_node *pred      = get_irn_n(node,i);
		opcode   pred_code = get_irn_opcode(pred);

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
static ir_node *bad_transform(arm_transform_env_t *env) {
	ir_fprintf(stderr, "Not implemented: %+F\n", env->irn);
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

typedef ir_node *(transform_func)(arm_transform_env_t *env);

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
		arm_transform_env_t tenv;
		transform_func *transform = (transform_func *)op->ops.generic;

		tenv.cg       = cg;
		tenv.block    = get_nodes_block(node);
		tenv.dbg      = get_irn_dbg_info(node);
		tenv.irg      = current_ir_graph;
		tenv.irn      = node;
		tenv.mode     = get_irn_mode(node);
		DEBUG_ONLY(tenv.mod = cg->mod);

		asm_node = (*transform)(&tenv);
	}

	if (asm_node) {
		exchange(node, asm_node);
		DB((cg->mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DB((cg->mod, LEVEL_1, "ignored\n"));
	}
}
