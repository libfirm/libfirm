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

static vals construct_vals(void) {
	vals result = { 0, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };
    return result;
}

static vals gen_vals_from_word(unsigned int value)
{
     vals result = construct_vals();
     int cur_offset = 0;
     while (value != 0) {
          if ((value & 3) == 0) {
               cur_offset += 2;
               value >>= 2;
          } else {
               result.values[result.ops] = value & 0xff;
               result.shifts[result.ops] = cur_offset;
               ++result.ops;
               value >>= 8;
               cur_offset += 8;
          }
     }
     return result;
}

static ir_node *create_const_node(arm_transform_env_t *env, int value) {
	ir_node *result = new_rd_arm_Const(env->dbg, env->irg, env->block, env->mode);
	get_arm_attr(result)->value = new_tarval_from_long(value, env->mode);
	return result;
}

#define NEW_BINOP_NODE(opname, env, op1, op2) new_rd_arm_##opname(env->dbg, env->irg, env->block, op1, op2, env->mode)

unsigned int create_shifter_operand(unsigned int shift, unsigned int immediate) {
	return immediate | ((shift>>1)<<8);
}

static ir_node *create_const_graph_value(arm_transform_env_t *env, unsigned int value) {
	ir_node *result;
	int negate = 0;

	if (value < 0x100 || ~value < 0x100 ) {
		if ( ~value < 0x100 ) {
			negate = 1;
			value = ~value;
		}
		result = create_const_node(env, value);
	} else {
		vals v;
		vals v1 = gen_vals_from_word(value);
		vals v2 = gen_vals_from_word(~value);
		int cnt = 0;
		if (v2.ops == 1) {
			result = new_rd_arm_Const_Neg(env->dbg, env->irg, env->block, env->mode);
			set_arm_value(result, new_tarval_from_long(create_shifter_operand(v2.shifts[0], v2.values[0]), mode_Iu));
		} else {
			if ( v2.ops < v1.ops ) {
				negate = 1;
				v = v2;
			} else {
				v = v1;
			}
			if (v.shifts[cnt] == 0) {
				result = create_const_node(env, v.values[cnt]);
			} else {
				result = create_const_node(env, create_shifter_operand(v.shifts[cnt], v.values[cnt]));
			}
			++cnt;
			while(cnt < v.ops) {
				ir_node *const_node = create_const_node(env, v.values[cnt]);
				ir_node *orr_i_node = new_rd_arm_Or_i(env->dbg, env->irg, env->block, result, env->mode);
				set_arm_value(orr_i_node, new_tarval_from_long(create_shifter_operand(v.shifts[cnt], v.values[cnt]), mode_Iu));
				result = orr_i_node;
				++cnt;
			}
		}
	}
	if ( negate ) {
		result = new_rd_arm_Not(env->dbg, env->irg, env->block, result, env->mode);
	}
	return result;
}

// static ir_node *create_const_graph_value(arm_transform_env_t *env, unsigned int value) {
// 	ir_node *irn = env->irn;
// 	ir_node *result;
// 	int negate = 0;
//
// 	if ( ~value < 0x100 ) {
// 		negate = 1;
// 		value = ~value;
// 	}
//
// 	if (value < 0x100) {
// 		return create_const_node(env, value);
// 	} else {
// 		vals v = gen_vals_from_word(value);
// 		int cnt = 0;
// 		ir_node *mov_val = create_const_node(env, v.values[cnt]);
// 		if (v.shifts[cnt] != 0) {
// 			ir_node *shift_node = new_rd_arm_Shl_i(env->dbg, env->irg, env->block, mov_val, env->mode);
// 			set_arm_value(shift_node, new_tarval_from_long(v.shifts[cnt], mode_Iu));
// 			result = shift_node;
// 		} else {
// 			result = mov_val;
// 		}
// 		++cnt;
// 		while(cnt < v.ops) {
// 			ir_node *const_node = create_const_node(env, v.values[cnt]);
// 			ir_node *orr_i_node = new_rd_arm_Or_i(env->dbg, env->irg, env->block, result, env->mode);
// 			unsigned shift = v.shifts[cnt];
// 			unsigned immediate = v.values[cnt];
// 			unsigned immediate_with_shift = immediate | ((shift>>1)<<8);
// 			set_arm_value(orr_i_node, new_tarval_from_long(immediate_with_shift, mode_Iu));
// 			result = orr_i_node;
// 			++cnt;
// 		}
// 		return result;
// 	}
// }

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
	return new_rd_arm_And(env->dbg, env->irg, env->block, op, mask_node, get_irn_mode(env->irn));
}

static ir_node *gen_sign_extension(arm_transform_env_t *env, ir_node *op, int result_bits) {
	int shift_width = 32 - result_bits;
	ir_node *shift_const_node = create_const_graph_value(env, shift_width);
	ir_node *lshift_node = new_rd_arm_Shl(env->dbg, env->irg, env->block, op, shift_const_node, get_irn_mode(op));
	ir_node *rshift_node = new_rd_arm_Shrs(env->dbg, env->irg, env->block, lshift_node, shift_const_node, get_irn_mode(env->irn));
	return rshift_node;
}

static ir_node *gen_Conv(arm_transform_env_t *env, ir_node *op) {
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
		return env->irn; /* TODO: implement int->float conversion*/
	} else if (mode_is_float(in_mode) && mode_is_int(out_mode)) {
		return env->irn; /* TODO: implement float->int conversion*/
	} else {
		assert(0 && "not implemented conversion");
		return env->irn;
	}
}


/**
 * Creates an arm Add.
 *
 * @param env   The transformation environment
 * @param op1   first operator
 * @param op2   second operator
 * @return the created arm Add node
 */
static ir_node *gen_Add(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *result = NULL;
	assert(!mode_is_float(env->mode) || (get_irn_mode(op1)->sort == get_irn_mode(op2)->sort));
	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (env->mode == mode_F) {
		result = new_rd_arm_fAdds(env->dbg, env->irg, env->block, op1, op2, env->mode);
	} else if (env->mode == mode_D) {
		result = new_rd_arm_fAddd(env->dbg, env->irg, env->block, op1, op2, env->mode);
	} else if (mode_is_numP(env->mode)) {
		if (is_arm_Const(op1)) {
			result = new_rd_arm_Add_i(env->dbg, env->irg, env->block, op2, env->mode);
			set_arm_value(result, get_arm_value(op1));
		} else if (is_arm_Const(op2)) {
			result = new_rd_arm_Add_i(env->dbg, env->irg, env->block, op1, env->mode);
			set_arm_value(result, get_arm_value(op2));
		} else {
			result = new_rd_arm_Add(env->dbg, env->irg, env->block, op1, op2, env->mode);
		}
	} else {
		assert(0 && "unknown mode for add");
	}
	return result;
}



/**
 * Creates an arm Mul.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm Mul node
 */
static ir_node *gen_Mul(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	assert(!mode_is_float(env->mode) || (get_irn_mode(op1)->sort == get_irn_mode(op2)->sort));
	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (env->mode == mode_F) {
		return new_rd_arm_fMuls(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	if (env->mode == mode_D) {
		return new_rd_arm_fMuld(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return new_rd_arm_Mul(env->dbg, env->irg, env->block, op1, op2, env->mode);
}


/**
 * Creates an arm floating Div.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm fDiv node
 */
static ir_node *gen_Quot(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	// assert(mode_is_float(env->mode) && "only floating point supported");
	assert(get_irn_mode(op1)->sort == get_irn_mode(op2)->sort);
	assert(mode_is_float(get_irn_mode(op1)));
	assert(get_irn_mode(op1) != mode_E && "IEEE Extended FP not supported");

	if (get_irn_mode(op1) == mode_F) {
		return new_rd_arm_fDivs(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return new_rd_arm_fDivd(env->dbg, env->irg, env->block, op1, op2, env->mode);
}


/**
 * Creates an arm And.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm And node
 */
static ir_node *gen_And(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *result;
	if (is_arm_Const(op1)) {
		result = new_rd_arm_And_i(env->dbg, env->irg, env->block, op2, env->mode);
		set_arm_value(result, get_arm_value(op1));
	} else if (is_arm_Const(op2)) {
		result = new_rd_arm_And_i(env->dbg, env->irg, env->block, op1, env->mode);
		set_arm_value(result, get_arm_value(op2));
	} else {
		result = new_rd_arm_And(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}



/**
 * Creates an arm Or.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm Or node
 */
static ir_node *gen_Or(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *result;
	if (is_arm_Const(op1)) {
		result = new_rd_arm_Or_i(env->dbg, env->irg, env->block, op2, env->mode);
		set_arm_value(result, get_arm_value(op1));
	} else if (is_arm_Const(op2)) {
		result = new_rd_arm_Or_i(env->dbg, env->irg, env->block, op1, env->mode);
		set_arm_value(result, get_arm_value(op2));
	} else {
		result = new_rd_arm_Or(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}



/**
 * Creates an arm Eor.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm Eor node
 */
static ir_node *gen_Eor(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *result;
	if (is_arm_Const(op1)) {
		result = new_rd_arm_Eor_i(env->dbg, env->irg, env->block, op2, env->mode);
		set_arm_value(result, get_arm_value(op1));
	} else if (is_arm_Const(op2)) {
		result = new_rd_arm_Eor_i(env->dbg, env->irg, env->block, op1, env->mode);
		set_arm_value(result, get_arm_value(op2));
	} else {
		result = new_rd_arm_Eor(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}



/**
 * Creates an arm Sub.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm Sub node
 */
static ir_node *gen_Sub(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *result = NULL;
	assert(!mode_is_float(env->mode) || (get_irn_mode(op1)->sort == get_irn_mode(op2)->sort));
	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (env->mode == mode_F) {
		result = new_rd_arm_fSubs(env->dbg, env->irg, env->block, op1, op2, env->mode);
	} else if (env->mode == mode_D) {
		result = new_rd_arm_fSubd(env->dbg, env->irg, env->block, op1, op2, env->mode);
	} else if (mode_is_numP(env->mode)) {
		if (is_arm_Const(op2)) {
			result = new_rd_arm_Sub_i(env->dbg, env->irg, env->block, op1, env->mode);
			set_arm_value(result, get_arm_value(op2));
		} else {
			result = new_rd_arm_Sub(env->dbg, env->irg, env->block, op1, op2, env->mode);
		}
	} else {
		assert(0 && "unknown mode for sub");
	}
	return result;
}

/**
 * Creates an arm Shl.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm Shl node
 */
static ir_node *gen_Shl(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *result;
	if (is_arm_Const(op2)) {
		result = new_rd_arm_Shl_i(env->dbg, env->irg, env->block, op1, env->mode);
		set_arm_value(result, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shl(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}



/**
 * Creates an arm Shr.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm Shr node
 */
static ir_node *gen_Shr(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *result;
	if (is_arm_Const(op2)) {
		result = new_rd_arm_Shr_i(env->dbg, env->irg, env->block, op1, env->mode);
		set_arm_value(result, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shr(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}

/**
 * Creates an arm Shrs.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created arm Shrs node
 */
static ir_node *gen_Shrs(arm_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *result;
	if (is_arm_Const(op2)) {
		result = new_rd_arm_Shrs_i(env->dbg, env->irg, env->block, op1, env->mode);
		set_arm_value(result, get_arm_value(op2));
	} else {
		result = new_rd_arm_Shrs(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return result;
}

/**
 * Transforms a Not node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Not node
 * @param op      operator
 * @param mode    node mode
 * @return the created arm Not node
 */
static ir_node *gen_Not(arm_transform_env_t *env, ir_node *op) {
	return new_rd_arm_Not(env->dbg, env->irg, env->block, op, env->mode);
}


static ir_node *gen_Abs(arm_transform_env_t *env, ir_node *op) {
	assert(env->mode != mode_E && "IEEE Extended FP not supported");

	if (env->mode == mode_F) {
		return new_rd_arm_fAbss(env->dbg, env->irg, env->block, op, env->mode);
	}
	if (env->mode == mode_D) {
		return new_rd_arm_fAbsd(env->dbg, env->irg, env->block, op, env->mode);
	}

	return new_rd_arm_Abs(env->dbg, env->irg, env->block, op, env->mode);
}


/**
 * Transforms a Minus node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Minus node
 * @param op      operator
 * @param mode    node mode
 * @return the created arm Minus node
 */
static ir_node *gen_Minus(arm_transform_env_t *env, ir_node *op) {
	if (mode_is_float(env->mode)) {
		return new_rd_arm_fMinus(env->dbg, env->irg, env->block, op, env->mode);
	}
	return new_rd_arm_Minus(env->dbg, env->irg, env->block, op, env->mode);
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

//	const ir_edge_t *edge;
//
// 	foreach_out_edge(node, edge) {
// 		ir_node* proj = get_edge_src_irn(edge);
// 		long nr = get_Proj_proj(proj);
// 		if ( nr == pn_Load_res )
// 			mode = proj->mode;
// 	}
//     besser: get_Load_mode() verwenden!

	assert(mode!=NULL && "irgendwie hat das load kein proj fuers result");
	if (mode==mode_F) {
		return new_rd_arm_fLoads(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	if (mode==mode_D) {
		return new_rd_arm_fLoadd(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
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

	if (mode == mode_F) {
		return new_rd_arm_fStores(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	if (mode == mode_D) {
		return new_rd_arm_fStored(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
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
		sub = new_rd_arm_Sub(env->dbg, env->irg, env->block, op, const_graph, get_irn_mode(op));
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
	ir_node *result;
	const char *str = get_sc_name(env->irn);
	result = new_rd_arm_SymConst(env->dbg, env->irg, env->block, env->mode);
	set_arm_symconst_label(result, str);
	return result;
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

	src_copy = be_new_Copy(&arm_reg_classes[CLASS_arm_general_purpose], irg, block, src);
	dst_copy = be_new_Copy(&arm_reg_classes[CLASS_arm_general_purpose], irg, block, dst);

 	res = new_rd_arm_CopyB( dbg, irg, block, dst_copy, src_copy, new_rd_arm_EmptyReg(dbg, irg, block, mode_Iu), new_rd_arm_EmptyReg(dbg, irg, block, mode_Iu), new_rd_arm_EmptyReg(dbg, irg, block, mode_Iu), mem, mode);
	set_arm_value(res, new_tarval_from_long(size, mode_Iu));

	return res;
}





// /************************************************************************/
// /* be transforms                                                        */
// /************************************************************************/
//
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
	int i;
	if (is_Block(node))
		return;
	if (is_Phi(node)) {
		for (i=0; i<get_irn_arity(node); i++) {
			ir_node *pred = get_irn_n(node,i);
			opcode pred_code = get_irn_opcode(pred);
			if (pred_code == iro_Const) {
				ir_node *const_graph;
				arm_transform_env_t tenv;
				tenv.block    = get_nodes_block(get_irn_n(get_nodes_block(node),i));
				tenv.dbg      = get_irn_dbg_info(pred);
				tenv.irg      = current_ir_graph;
				tenv.irn      = pred;
				DEBUG_ONLY(tenv.mod      = cgenv->mod;)
				tenv.mode     = get_irn_mode(pred);
				const_graph = create_const_graph(&tenv);
				set_irn_n(node, i, const_graph);
			} else if (pred_code == iro_SymConst) {
				const char *str = get_sc_name(pred);
				ir_node *symconst_node;
				symconst_node = new_rd_arm_SymConst(get_irn_dbg_info(pred),
					current_ir_graph, get_nodes_block(get_irn_n(get_nodes_block(node),i)), get_irn_mode(pred));
				set_arm_symconst_label(symconst_node, str);
				set_irn_n(node, i, symconst_node);
			}
		}
		return;
	}
	for (i=0; i<get_irn_arity(node); i++) {
		ir_node *pred = get_irn_n(node,i);
		opcode pred_code = get_irn_opcode(pred);
		if (pred_code == iro_Const) {
			ir_node *const_graph;
			arm_transform_env_t tenv;
			tenv.block    = get_nodes_block(node);
			tenv.dbg      = get_irn_dbg_info(pred);
			tenv.irg      = current_ir_graph;
			tenv.irn      = pred;
			DEBUG_ONLY(tenv.mod      = cgenv->mod;)
			tenv.mode     = get_irn_mode(pred);
			const_graph = create_const_graph(&tenv);
			set_irn_n(node, i, const_graph);
		} else if (pred_code == iro_SymConst) {
			const char *str = get_sc_name(pred);
			ir_node *symconst_node;
			symconst_node = new_rd_arm_SymConst(get_irn_dbg_info(pred),
				current_ir_graph, get_nodes_block(node), get_irn_mode(pred));
			set_arm_symconst_label(symconst_node, str);
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

	for (i = 0; i  < get_irn_arity(node); i++) {
		ir_node *pred      = get_irn_n(node,i);
		opcode   pred_code = get_irn_opcode(pred);

		if (pred_code == iro_SymConst) {
			const char *str = get_sc_name(pred);
			ir_node    *symconst_node;

			symconst_node = new_rd_arm_SymConst(get_irn_dbg_info(pred),
				current_ir_graph, get_nodes_block(node), get_irn_mode(pred));

			set_arm_symconst_label(symconst_node, str);
			set_irn_n(node, i, symconst_node);
		}
	}
}


/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void arm_transform_node(ir_node *node, void *env) {
	arm_code_gen_t *cgenv = (arm_code_gen_t *)env;
	opcode  code               = get_irn_opcode(node);
	ir_node *asm_node          = NULL;
	arm_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block    = get_nodes_block(node);
	tenv.dbg      = get_irn_dbg_info(node);
	tenv.irg      = current_ir_graph;
	tenv.irn      = node;
	DEBUG_ONLY(tenv.mod      = cgenv->mod;)
	tenv.mode     = get_irn_mode(node);

#define UNOP(a)        case iro_##a: asm_node = gen_##a(&tenv, get_##a##_op(node)); break
#define BINOP(a)       case iro_##a: asm_node = gen_##a(&tenv, get_##a##_left(node), get_##a##_right(node)); break
#define GEN(a)         case iro_##a: asm_node = gen_##a(&tenv); break
#define IGN(a)         case iro_##a: break
#define BAD(a)         case iro_##a: goto bad

	DBG((tenv.mod, LEVEL_1, "check %+F ... ", node));

	switch (code) {
		BINOP(Add);  // done
		BINOP(Mul);  // done
		BINOP(Quot); // done
		BINOP(And);  // done
		BINOP(Or);   // done
		BINOP(Eor);  // done

		BINOP(Sub);  // done
		BINOP(Shl);  // done
		BINOP(Shr);  // done
		BINOP(Shrs); // done

		UNOP(Minus); // done
		UNOP(Not);   // done
		UNOP(Abs);   // done

		GEN(CopyB); // done
		GEN(Const); // TODO: floating point consts
		UNOP(Conv); // TODO: floating point conversions

		GEN(Load);   // done
		GEN(Store);  // done

		GEN(SymConst);
		GEN(Cond);	  // integer done

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

		default:
			if (get_irn_op(node) == get_op_Max() ||
				get_irn_op(node) == get_op_Min() ||
				get_irn_op(node) == get_op_Mulh())
			{
				/* TODO: implement */
				/* ignore for now  */
			}
			break;
bad:
		fprintf(stderr, "Not implemented: %s\n", get_irn_opname(node));
		assert(0);
	}

	if (asm_node) {
		exchange(node, asm_node);
		DB((tenv.mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DB((tenv.mod, LEVEL_1, "ignored\n"));
	}
}
