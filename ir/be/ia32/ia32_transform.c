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
#include "bearch_ia32_t.h"

#include "ia32_nodes_attr.h"
#include "../arch/archop.h"     /* we need this for Min and Max nodes */
#include "ia32_transform.h"
#include "ia32_new_nodes.h"
#include "ia32_map_regs.h"

#include "gen_ia32_regalloc_if.h"

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



/* determine if one operator is an Imm */
static ir_node *get_immediate_op(ir_node *op1, ir_node *op2) {
	if (op1)
		return is_ia32_Const(op1) ? op1 : (is_ia32_Const(op2) ? op2 : NULL);
	else return is_ia32_Const(op2) ? op2 : NULL;
}

/* determine if one operator is not an Imm */
static ir_node *get_expr_op(ir_node *op1, ir_node *op2) {
	return !is_ia32_Const(op1) ? op1 : (!is_ia32_Const(op2) ? op2 : NULL);
}



/**
 * Creates an ia32 Add with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Add_i node
 */
static ir_node *gen_imm_Add(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	ir_node *new_op;
	tarval  *tv         = get_ia32_Immop_tarval(const_op);
	int      normal_add = 0;
	tarval_classification_t class_tv, class_negtv;
	firm_dbg_module_t *mod   = env->mod;
	dbg_info          *dbg   = env->dbg;
	ir_mode           *mode  = env->mode;
	ir_graph          *irg   = env->irg;
	ir_node           *block = env->block;

	/* const_op: tarval or SymConst? */
	if (tv) {
		/* optimize tarvals */
		class_tv    = classify_tarval(tv);
		class_negtv = classify_tarval(tarval_neg(tv));

		if (class_tv == TV_CLASSIFY_ONE) { /* + 1 == INC */
			DB((env->mod, LEVEL_2, "Add(1) to Inc ... "));
			new_op = new_rd_ia32_Inc(dbg, irg, block, expr_op, mode);
		}
		else if (class_tv == TV_CLASSIFY_ALL_ONE || class_negtv == TV_CLASSIFY_ONE) { /* + (-1) == DEC */
			DB((mod, LEVEL_2, "Add(-1) to Dec ... "));
			new_op = new_rd_ia32_Dec(dbg, irg, block, expr_op, mode);
		}
		else
			normal_add = 1;
	}
	else
		normal_add = 1;

	if (normal_add)
		new_op = new_rd_ia32_Lea_i(dbg, irg, block, expr_op, mode);

	return new_op;
}

/**
 * Creates an ia32 Add.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Add node
 */
static ir_node *gen_Add(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *shli_op;
	ir_node *expr_op;
	ir_node *new_op;
	int normal_add  = 0;
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;

	if (mode_is_float(mode)) {
		return new_rd_ia32_fAdd(dbg, irg, block, op1, op2, mode);
	}

	/* try to optimize with LEA */

	shli_op = is_ia32_Shl_i(op1) ? op1 : (is_ia32_Shl_i(op2) ? op2 : NULL);
	expr_op = shli_op == op1 ? op2 : (shli_op == op2 ? op1 : NULL);

	if (shli_op) {
		tarval *tv   = get_ia32_Immop_tarval(shli_op);
		tarval *offs = NULL;
		if (tv) {
			switch (get_tarval_long(tv)) {
				case 1:
				case 2:
				case 3:
					// If the other operand of the LEA is an LEA_i (that means LEA ofs(%regop1)),
					// we can skip it and transform the whole sequence into LEA ofs(%regop1, %regop2, shl_val),
					if (is_ia32_Lea_i(expr_op)) {
						offs    = get_ia32_Immop_tarval(expr_op);
						expr_op = get_irn_n(expr_op, 0);
					}

					new_op = new_rd_ia32_Lea(dbg, irg, block, expr_op, get_irn_n(shli_op, 0), mode);
					set_ia32_Immop_tarval(new_op, tv);
					set_ia32_am_offs(new_op, offs);

					break;
				default:
					normal_add = 1;
					break;
			}
		}
		else
			normal_add = 1;
	}
	else
		normal_add = 1;

	if (normal_add) {
		new_op = new_rd_ia32_Lea(dbg, irg, block, op1, op2, mode);
		set_ia32_Immop_tarval(new_op, get_tarval_null(mode_Iu));
		set_ia32_am_offs(new_op, NULL);
	}

	return new_op;
}



/**
 * Generates an ia32 Mul node.
 *
 * @param env      The transformation environment
 * @param op1      The first faktor
 * @param op2      The second factor
 * @param mul_flav flavour_Mul/Mulh
 * @return The ready-to-go Mul node
 */
ir_node *generate_Mul(ia32_transform_env_t *env, ir_node *op1, ir_node *op2, op_flavour_t mul_flav, int is_imm_op) {
	ir_node    *in_keep[1], *res;
	long        pn_good, pn_bad;
	dbg_info   *dbg   = env->dbg;
	ir_graph   *irg   = env->irg;
	ir_node    *block = env->block;
	ir_mode    *mode  = env->mode;
	ir_node    *mul;

	/* create the mul */
	if (is_imm_op) {
		mul = new_rd_ia32_Mul_i(dbg, irg, block, op1, mode_T);
		set_ia32_Immop_attr(mul, op2);
	}
	else {
		mul = new_rd_ia32_Mul(dbg, irg, block, op1, op2, mode_T);
	}
	set_ia32_flavour(mul, mul_flav);

	/* create the mul infrastructure */
	if (mul_flav == flavour_Mul) {
		pn_good = pn_EAX;
		pn_bad  = pn_EDX;
	}
	else { /* Mulh */
		pn_good = pn_EDX;
		pn_bad  = pn_EAX;
	}

	res        = new_rd_Proj(dbg, irg, block, mul, mode, pn_good);
	in_keep[0] = new_rd_Proj(dbg, irg, block, mul, mode, pn_bad);

	be_new_Keep(&ia32_reg_classes[CLASS_ia32_general_purpose], irg, block, 1, in_keep);

	return res;
}



/**
 * Creates an ia32 Mul with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Mul_i node
 */
static ir_node *gen_imm_Mul(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	/* Mul with immediate only possible with int, so we don't need to check for float */
	return generate_Mul(env, expr_op, const_op, flavour_Mul, 1);
}

/**
 * Creates an ia32 Mul.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Mul node
 */
ir_node *gen_Mul(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	if (mode_is_float(env->mode)) {
		return new_rd_ia32_fMul(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	else {
		return generate_Mul(env, op1, op2, flavour_Mul, 0);
	}
}



/**
 * Creates an ia32 Mulh with immediate.
 * Note: Mul produces a 64Bit result and Mulh returns the upper 32 bit of
 * this result while Mul returns the lower 32 bit.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Mulh_i node
 */
static ir_node *gen_imm_Mulh(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	return generate_Mul(env, expr_op, const_op, flavour_Mulh, 1);
}

/**
 * Creates an ia32 Mulh.
 * Note: Mul produces a 64Bit result and Mulh returns the upper 32 bit of
 * this result while Mul returns the lower 32 bit.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Mulh node
 */
static ir_node *gen_Mulh(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return generate_Mul(env, op1, op2, flavour_Mulh, 0);
}



/**
 * Creates an ia32 And with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 And_i node
 */
static ir_node *gen_imm_And(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	return new_rd_ia32_And_i(env->dbg, env->irg, env->block, expr_op, env->mode);
}

/**
 * Creates an ia32 And.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 And node
 */
static ir_node *gen_And(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_And(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 Or with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Or_i node
 */
static ir_node *gen_imm_Or(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	return new_rd_ia32_Or_i(env->dbg, env->irg, env->block, expr_op, env->mode);
}

/**
 * Creates an ia32 Or.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Or node
 */
static ir_node *gen_Or(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_Or(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 Eor with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Eor_i node
 */
static ir_node *gen_imm_Eor(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	return new_rd_ia32_Eor_i(env->dbg, env->irg, env->block, expr_op, env->mode);
}

/**
 * Creates an ia32 Eor.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Eor node
 */
static ir_node *gen_Eor(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_Eor(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 Max.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Max node
 */
static ir_node *gen_Max(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_Max(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 Min.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Min node
 */
static ir_node *gen_Min(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_Min(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 Sub with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Sub_i node
 */
static ir_node *gen_imm_Sub(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	ir_node *new_op;
	tarval  *tv        = get_ia32_Immop_tarval(const_op);
	int     normal_sub = 0;
	tarval_classification_t class_tv, class_negtv;
	firm_dbg_module_t *mod   = env->mod;
	dbg_info          *dbg   = env->dbg;
	ir_mode           *mode  = env->mode;
	ir_graph          *irg   = env->irg;
	ir_node           *block = env->block;

	/* const_op: tarval or SymConst? */
	if (tv) {
		/* optimize tarvals */
		class_tv    = classify_tarval(tv);
		class_negtv = classify_tarval(tarval_neg(tv));

		if (class_tv == TV_CLASSIFY_ONE) { /* - 1 == DEC */
			DB((mod, LEVEL_2, "Sub(1) to Dec ... "));
			new_op = new_rd_ia32_Dec(dbg, irg, block, expr_op, mode);
		}
		else if (class_negtv == TV_CLASSIFY_ONE) { /* - (-1) == Sub */
			DB((mod, LEVEL_2, "Sub(-1) to Inc ... "));
			new_op = new_rd_ia32_Inc(dbg, irg, block, expr_op, mode);
		}
		else
			normal_sub = 1;
	}
	else
		normal_sub = 1;

	if (normal_sub)
		new_op = new_rd_ia32_Sub_i(dbg, irg, block, expr_op, mode);

	return new_op;
}

/**
 * Creates an ia32 Sub.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Sub node
 */
static ir_node *gen_Sub(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	if (mode_is_float(env->mode)) {
		return new_rd_ia32_fSub(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	return new_rd_ia32_Sub(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Generates an ia32 DivMod with additional infrastructure for the
 * register allocator if needed.
 *
 * @param env      The transformation environment
 * @param dividend -no comment- :)
 * @param divisor  -no comment- :)
 * @param dm_flav  flavour_Div/Mod/DivMod
 * @return The created ia32 DivMod node
 */
static ir_node *generate_DivMod(ia32_transform_env_t *env, ir_node *dividend, ir_node *divisor, op_flavour_t dm_flav) {
	ir_node  *res, *proj;
	ir_node  *edx_node, *cltd;
	ir_node  *in_keep[1];
	dbg_info *dbg   = env->dbg;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;
	ir_mode  *mode  = env->mode;
	ir_node  *irn   = env->irn;
	ir_node  *mem;

	switch (dm_flav) {
		case flavour_Div:
			mem = get_Div_mem(irn);
			break;
		case flavour_Mod:
			mem = get_Mod_mem(irn);
			break;
		case flavour_DivMod:
			mem = get_DivMod_mem(irn);
			break;
		default:
			assert(0);
	}

	if (mode_is_signed(mode)) {
		/* in signed mode, we need to sign extend the dividend */
		cltd     = new_rd_ia32_Cltd(dbg, irg, block, dividend, mode_T);
		dividend = new_rd_Proj(dbg, irg, block, cltd, mode_Is, pn_EAX);
		edx_node = new_rd_Proj(dbg, irg, block, cltd, mode_Is, pn_EDX);
	}
	else {
		edx_node = new_rd_ia32_Const(dbg, irg, block, mode_Iu);
		set_ia32_Const_type(edx_node, asmop_Const);
		set_ia32_Immop_tarval(edx_node, get_tarval_null(mode_Iu));
	}

	res = new_rd_ia32_DivMod(dbg, irg, block, dividend, divisor, edx_node, mem, mode);

	set_ia32_flavour(res, dm_flav);
	set_ia32_n_res(res, 2);

	/* Only one proj is used -> We must add a second proj and */
	/* connect this one to a Keep node to eat up the second   */
	/* destroyed register.                                    */
	if (get_irn_n_edges(irn) == 1) {
		proj = get_edge_src_irn(get_irn_out_edge_first(irn));
		assert(is_Proj(proj) && "non-Proj to Div/Mod node");

		if (get_Proj_proj(proj) == pn_DivMod_res_div) {
			in_keep[0] = new_rd_Proj(dbg, irg, block, res, mode_Is, pn_DivMod_res_mod);
		}
		else {
			in_keep[0] = new_rd_Proj(dbg, irg, block, res, mode_Is, pn_DivMod_res_div);
		}

		be_new_Keep(&ia32_reg_classes[CLASS_ia32_general_purpose], irg, block, 1, in_keep);
	}

	return res;
}


/**
 * Wrapper for generate_DivMod. Sets flavour_Mod.
 */
static ir_node *gen_Mod(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return generate_DivMod(env, op1, op2, flavour_Mod);
}



/**
 * Wrapper for generate_DivMod. Sets flavour_Div.
 */
static ir_node *gen_Div(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return generate_DivMod(env, op1, op2, flavour_Div);
}



/**
 * Wrapper for generate_DivMod. Sets flavour_DivMod.
 */
static ir_node *gen_DivMod(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return generate_DivMod(env, op1, op2, flavour_DivMod);
}



/**
 * Creates an ia32 floating Div.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 fDiv node
 */
static ir_node *gen_Quot(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_fDiv(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 Shl with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Shl_i node
 */
static ir_node *gen_imm_Shl(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	return new_rd_ia32_Shl_i(env->dbg, env->irg, env->block, expr_op, env->mode);
}

/**
 * Creates an ia32 Shl.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Shl node
 */
static ir_node *gen_Shl(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_Shl(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 Shr with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Shr_i node
 */
static ir_node *gen_imm_Shr(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	return new_rd_ia32_Shr_i(env->dbg, env->irg, env->block, expr_op, env->mode);
}

/**
 * Creates an ia32 Shr.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Shr node
 */
static ir_node *gen_Shr(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_Shr(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 Shrs with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Shrs_i node
 */
static ir_node *gen_imm_Shrs(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	return new_rd_ia32_Shrs_i(env->dbg, env->irg, env->block, expr_op, env->mode);
}

/**
 * Creates an ia32 Shrs.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Shrs node
 */
static ir_node *gen_Shrs(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_Shrs(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 RotL.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 RotL node
 */
static ir_node *gen_RotL(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_RotL(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an ia32 RotR.
 * NOTE: There is no RotR with immediate because this would always be a RotL
 *       "imm-mode_size_bits" which can be pre-calculated.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 RotR node
 */
static ir_node *gen_RotR(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_ia32_RotR(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Transforms a Rot with immediate into an ia32 RotL with immediate
 * as the Firm Rot is a RotL (see NOTE on RotR with immediate above).
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 RotL node
 */
static ir_node *gen_imm_Rot(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	return new_rd_ia32_RotL_i(env->dbg, env->irg, env->block, expr_op, env->mode);
}

/**
 * Creates an ia32 RotR or RotL (depending on the found pattern).
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 RotL or RotR node
 */
static ir_node *gen_Rot(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *rotate = NULL;

	/* Firm has only Rot (which is a RotL), so we are looking for a right (op2)
		 operand "-e+mode_size_bits" (it's an already modified "mode_size_bits-e",
		 that means we can create a RotR instead of an Add and a RotL */

	if (is_ia32_Add_i(op2)) {
		ir_node *minus = get_irn_n(op2, 0); // is there an op_Minus?

		if (is_ia32_Minus(minus)) {
			tarval *tv = get_ia32_Immop_tarval(op2);
			long bits  = get_mode_size_bits(env->mode);

			if (tarval_is_long(tv) && get_tarval_long(tv) == bits) {
				DB((env->mod, LEVEL_1, "RotL into RotR ... "));
				rotate = gen_RotR(env, op1, get_irn_n(minus, 0));
			}
		}
	}

	if (!rotate)
		rotate = gen_RotL(env, op1, op2);

	return rotate;
}



/**
 * Transforms commutative operations (op_Add, op_And, op_Or, op_Eor)
 * and non-commutative operations with com == 0 (op_Sub, op_Shl, op_Shr, op_Shrs, op_Rot)
 *
 * @param mod       the debug module
 * @param block     the block node belongs to
 * @param node      the node to transform
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @param com       flag if op is commutative
 * @return the created assembler node
 */
static ir_node *gen_arith_Op(ia32_transform_env_t *env, ir_node *op1, ir_node *op2, int com) {
	firm_dbg_module_t *mod      = env->mod;
	ir_node           *node     = env->irn;
	ir_node           *imm_op   = NULL;
	ir_node           *expr_op  = NULL;
	ir_node           *asm_node = NULL;
	opcode             opc      = get_irn_opcode(node);
	ir_op             *op       = get_irn_op(node);

#undef GENOP
#undef GENOPI
#undef GENOPI_SETATTR
#define GENOP(a)          case iro_##a: asm_node = gen_##a(env, op1, op2); break
#define GENOPI(a)         case iro_##a: asm_node = gen_imm_##a(env, expr_op, imm_op); break
#define GENOPI_SETATTR(a) case iro_##a: asm_node = gen_imm_##a(env, expr_op, imm_op); set_ia32_Immop_attr(asm_node, imm_op); break

	if (com)
		imm_op  = get_immediate_op(op1, op2);
	else
		imm_op  = get_immediate_op(NULL, op2);

	expr_op = get_expr_op(op1, op2);

	/* TODO: Op(Const, Const) support */
	if (is_ia32_Const(op1) && is_ia32_Const(op2)) {
		DB((mod, LEVEL_2, "%+F(Const, Const) -> binop ... ", get_irn_opname(node)));
		imm_op = NULL;
	}

	/* There are arithmetic operations which can't take an immediate */
	switch(opc) {
		case iro_Div:
		case iro_Mod:
		case iro_DivMod:
			DB((mod, LEVEL_2, "Div/Mod/DivMod imm -> binop ... "));
			imm_op = NULL;
			break;
		default:
			if (op == get_op_Min() || op == get_op_Max()) {
				DB((mod, LEVEL_2, "MIN/MAX imm -> binop ... "));
				imm_op = NULL;
			}
			break;
	}

	DB((mod, LEVEL_1, "(%+F -- %+F) ... ", op1, op2));

	if (!mode_is_float(env->mode) && imm_op) {
		DB((mod, LEVEL_1, "immop ... "));

		switch(opc) {
			GENOPI_SETATTR(Add);
			GENOPI(Mul);
			GENOPI_SETATTR(And);
			GENOPI_SETATTR(Or);
			GENOPI_SETATTR(Eor);

			GENOPI_SETATTR(Sub);
			GENOPI_SETATTR(Shl);
			GENOPI_SETATTR(Shr);
			GENOPI_SETATTR(Shrs);
			GENOPI_SETATTR(Rot);
			default:
				if (op == get_op_Mulh()) {
					asm_node = gen_imm_Mulh(env, expr_op, imm_op);
				}
				else {
					assert("binop_i: THIS SHOULD NOT HAPPEN");
				}
		}
	}
	else {
		DB((mod, LEVEL_1, "binop ... "));

		switch(opc) {
			GENOP(Add);
			GENOP(Mul);
			GENOP(And);
			GENOP(Or);
			GENOP(Eor);

			GENOP(Quot);

			GENOP(Div);
			GENOP(Mod);
			GENOP(DivMod);

			GENOP(Sub);
			GENOP(Shl);
			GENOP(Shr);
			GENOP(Shrs);
			GENOP(Rot);
			default:
				if (op == get_op_Max()) {
					asm_node = gen_Max(env, op1, op2);
				}
				else if (op == get_op_Min()) {
					asm_node = gen_Min(env, op1, op2);
				}
				else if (op == get_op_Mulh()) {
					asm_node = gen_Mulh(env, op1, op2);
				}
				else {
					assert("binop: THIS SHOULD NOT HAPPEN");
				}
		}
	}

	return asm_node;
}
#undef GENOP
#undef GENOPI
#undef GENOPI_SETATTR



/**
 * Transforms a Minus node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Minus node
 * @param op      operator
 * @param mode    node mode
 * @return the created ia32 Minus node
 */
static ir_node *gen_Minus(ia32_transform_env_t *env, ir_node *op) {
	if (is_ia32_Minus(op) || is_ia32_fMinus(op)) {
		DB((env->mod, LEVEL_1, "--(e) to e ..."));
		return get_irn_n(op, 0);
	}
	else {
		if (mode_is_float(env->mode)) {
			return new_rd_ia32_fMinus(env->dbg, env->irg, env->block, op, env->mode);
		}
		return new_rd_ia32_Minus(env->dbg, env->irg, env->block, op, env->mode);
	}
}



/**
 * Transforms a Conv node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Conv node
 * @param op      operator
 * @param mode    node mode
 * @return the created ia32 Conv node
 */
static ir_node *gen_Conv(ia32_transform_env_t *env, ir_node *op) {
	return new_rd_ia32_Conv(env->dbg, env->irg, env->block, op, env->mode);
}



/**
 * Transforms a Not node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Not node
 * @param op      operator
 * @param mode    node mode
 * @return the created ia32 Not node
 */
static ir_node *gen_Not(ia32_transform_env_t *env, ir_node *op) {
	return new_rd_ia32_Not(env->dbg, env->irg, env->block, op, env->mode);
}



/**
 * Transforms an Abs node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Abs node
 * @param op      operator
 * @param mode    node mode
 * @return the created ia32 Abs node
 */
static ir_node *gen_Abs(ia32_transform_env_t *env, ir_node *op) {
	ir_node  *res, *p_eax, *p_edx;
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;

	res   = new_rd_ia32_Cltd(dbg, irg, block, op, mode_T);
	p_eax = new_rd_Proj(dbg, irg, block, res, mode, pn_EAX);
	p_edx = new_rd_Proj(dbg, irg, block, res, mode, pn_EDX);
	res   = new_rd_ia32_Eor(dbg, irg, block, p_eax, p_edx, mode);
	res   = new_rd_ia32_Sub(dbg, irg, block, res, p_edx, mode);

	return res;
}



/**
 * Transforms a Load.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Load node
 * @param mode    node mode
 * @return the created ia32 Load node
 */
static ir_node *gen_Load(ia32_transform_env_t *env) {
	ir_node *node = env->irn;

	if (mode_is_float(env->mode)) {
		return new_rd_ia32_fLoad(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	return new_rd_ia32_Load(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
}



/**
 * Transforms a Store.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Store node
 * @param mode    node mode
 * @return the created ia32 Store node
 */
ir_node *gen_Store(ia32_transform_env_t *env) {
	ir_node *node = env->irn;

	if (mode_is_float(env->mode)) {
		return new_rd_ia32_fStore(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	return new_rd_ia32_Store(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
}



/**
 * Transforms a Call and its arguments corresponding to the calling convention.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Call node
 * @param dummy   mode doesn't matter
 * @return the created ia32 Call node
 */
static ir_node *gen_Call(ia32_transform_env_t *env) {
	const ia32_register_req_t **in_req;
	ir_node          **in;
	ir_node           *new_call, *sync;
	int                i, j, n_new_call_in, ignore = 0;
	asmop_attr        *attr;
	dbg_info          *dbg          = env->dbg;
	ir_graph          *irg          = env->irg;
	ir_node           *block        = env->block;
	ir_node           *call         = env->irn;
	ir_node          **stack_param  = NULL;
	ir_node          **param        = get_Call_param_arr(call);
	ir_node           *call_Mem     = get_Call_mem(call);
	unsigned           cc           = get_method_calling_convention(get_Call_type(call));
	int                n            = get_Call_n_params(call);
	int                stack_idx    = 0;
	int                biggest_n    = -1;
	int                n_res        = get_method_n_ress(get_Call_type(call));

	assert(n_res <= 2 && "method with more than two results not supported");

	if (cc & cc_reg_param)
		biggest_n = ia32_get_n_regparam_class(n, param, &ignore, &ignore);

	/* remember: biggest_n = x means we can pass (x + 1) parameters in register */

	/* do we need to pass arguments on stack? */
	if (biggest_n + 1 < n)
		stack_param = xcalloc(n - biggest_n - 1, sizeof(ir_node *));

	/* we need at least one in, either for the stack params or the call_Mem */
	n_new_call_in = biggest_n + 2;

	/* the call has one IN for all stack parameter and one IN for each reg param */
	in     = xcalloc(n_new_call_in, sizeof(ir_node *));
	in_req = xcalloc(n_new_call_in, sizeof(arch_register_req_t *));

	/* loop over all parameters and set the register requirements */
	for (i = 0; i <= biggest_n && (cc & cc_reg_param); i++) {
		in_req[i] = ia32_get_RegParam_req(n, param, i, cc);
	}
	stack_idx = i;

	/* create remaining stack parameters */
	if (cc & cc_last_on_top) {
		for (i = stack_idx; i < n; i++) {
			/* pass it on stack */
			if (mode_is_float(get_irn_mode(param[i]))) {
				stack_param[i - stack_idx] = new_rd_ia32_fStackArg(get_irn_dbg_info(param[i]), irg,
														block, call_Mem, param[i], mode_M);
			}
			else {
				stack_param[i - stack_idx] = new_rd_ia32_StackArg(get_irn_dbg_info(param[i]), irg,
														block, call_Mem, param[i], mode_M);
			}
			/* set the argument number for later lowering */
			set_ia32_pncode(stack_param[i - stack_idx], i - stack_idx);
		}
	}
	else {
		for (i = n - 1, j = 0; i >= stack_idx; i--, j++) {
			/* pass it on stack */
			if (mode_is_float(get_irn_mode(param[i]))) {
				stack_param[j] = new_rd_ia32_fStackArg(get_irn_dbg_info(param[i]), irg,
														block, call_Mem, param[i], mode_M);
			}
			else {
				stack_param[j] = new_rd_ia32_StackArg(get_irn_dbg_info(param[i]), irg,
														block, call_Mem, param[i], mode_M);
			}
			/* set the argument number for later lowering */
			set_ia32_pncode(stack_param[j], j);
		}
	}

	if (stack_param) {
		sync = new_r_Sync(irg, block, n - biggest_n - 1, stack_param);
		in[n_new_call_in - 1] = sync;
	}
	else {
		in[n_new_call_in - 1] = call_Mem;
	}

	/* create the new node */
	new_call = new_rd_ia32_Call(dbg, irg, block, n_new_call_in, in);
	set_ia32_Immop_attr(new_call, get_Call_ptr(call));

	/* set register requirements for in and out */
	attr             = get_ia32_attr(new_call);
	attr->in_req     = in_req;

	set_ia32_n_res(new_call, n_res);

	if (n_res > 0) {
		attr->out_req    = xcalloc(n_res, sizeof(ia32_register_req_t *));
		attr->slots      = xcalloc(n_res, sizeof(arch_register_t *));
	}

	/* two results only appear when a 64bit int result is broken up into two 32bit results */
	if (n_res == 1) {
		if (mode_is_float(get_type_mode(get_method_res_type(get_Call_type(call), 0))))
			attr->out_req[0] = &ia32_default_req_ia32_floating_point_xmm0;
		else
			attr->out_req[0] = &ia32_default_req_ia32_general_purpose_eax;
	}
	else if (n_res == 2) {
		attr->out_req[0] = &ia32_default_req_ia32_general_purpose_eax;
		attr->out_req[1] = &ia32_default_req_ia32_general_purpose_edx;
	}

	/* stack parameter has no OUT register */
	attr->in_req[n_new_call_in - 1] = &ia32_default_req_none;

	return new_call;
}



/**
 * Transforms a Cond -> Proj[b] -> Cmp into a CondJmp or CondJmp_i
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Cond node
 * @param mode    mode of the Cond
 * @return The transformed node.
 */
static ir_node *gen_Cond(ia32_transform_env_t *env) {
	dbg_info          *dbg      = env->dbg;
	ir_graph          *irg      = env->irg;
	ir_node           *block    = env->block;
	ir_node           *node     = env->irn;
	ir_node	          *sel      = get_Cond_selector(node);
	ir_mode           *sel_mode = get_irn_mode(sel);
	ir_node           *res      = NULL;
	ir_node           *pred     = NULL;
	ir_node           *cmp_a, *cmp_b, *cnst, *expr;

	if (is_Proj(sel) && sel_mode == mode_b) {
		pred  = get_Proj_pred(sel);

		/* get both compare operators */
		cmp_a = get_Cmp_left(pred);
		cmp_b = get_Cmp_right(pred);

		/* check if we can use a CondJmp with immediate */
		cnst = get_immediate_op(cmp_a, cmp_b);
		expr = get_expr_op(cmp_a, cmp_b);

		if (cnst && expr) {
			res = new_rd_ia32_CondJmp_i(dbg, irg, block, expr, mode_T);
			set_ia32_Immop_attr(res, cnst);
		}
		else {
			res = new_rd_ia32_CondJmp(dbg, irg, block, cmp_a, cmp_b, mode_T);
		}

		set_ia32_pncode(res, get_Proj_proj(sel));
	}
	else {
		res = new_rd_ia32_SwitchJmp(dbg, irg, block, sel, mode_T);
		set_ia32_pncode(res, get_Cond_defaultProj(node));
	}

	return res;
}



/**
 * Transform the argument projs from a start node corresponding to the
 * calling convention.
 * It transforms "Proj Arg x -> ProjT -> Start <- ProjM" into
 * "RegParam x   -> ProjT -> Start" OR
 * "StackParam x -> ProjM -> Start"
 * whether parameter is passed in register or on stack.
 *
 * @param mod     the debug module
 * @param block   the block the nodes should belong to
 * @param proj    the ProjT node which points to Start
 * @param start   the Start node
 * @return Should be always NULL
 */
static ir_node *gen_Proj_Start(ia32_transform_env_t *env, ir_node *proj, ir_node *start) {
	const ia32_register_req_t *temp_req;
	const ir_edge_t   *edge;
	ir_node           *succ, *irn;
	ir_node          **projargs;
	ir_mode           *mode;
	int                n, i;
	unsigned           cc;
	ir_node           *proj_M     = get_irg_initial_mem(current_ir_graph);
	entity            *irg_ent    = get_irg_entity(current_ir_graph);
	ir_type           *tp         = get_entity_type(irg_ent);
	int                cur_pn     = 0;
	ir_graph          *irg        = env->irg;
	ir_node           *block      = env->block;

	assert(is_Method_type(tp) && "irg type is not a method");

	switch(get_Proj_proj(proj)) {
		case pn_Start_T_args:
			/* We cannot use get_method_n_params here as the function might
			   be variadic or one argument is not used. */
			n = get_irn_n_edges(proj);

			/* Allocate memory for all non variadic parameters in advance to be on the save side */
			env->cg->reg_param_req = xcalloc(get_method_n_params(tp), sizeof(ia32_register_req_t *));

			/* we are done here when there are no parameters */
			if (n < 1)
				break;

			/* temporary remember all proj arg x */
			projargs = xcalloc(n, sizeof(ir_node *));

			i = 0;
			foreach_out_edge((const ir_node *)proj, edge) {
				succ = get_edge_src_irn(edge);
				assert(is_Proj(succ) && "non-Proj from a Proj_T (pn_Start_T_args).");
				projargs[i++] = succ;
			}

			cc = get_method_calling_convention(tp);

			/* loop over all parameters and check whether its a int or float */
			for (i = 0; i < n; i++) {
				mode   = get_irn_mode(projargs[i]);
				cur_pn = get_Proj_proj(projargs[i]);

				if (cc & cc_reg_param) {
					temp_req = ia32_get_RegParam_req(n, projargs, cur_pn, cc);
				}
				else {
					temp_req = NULL;
				}

				if (temp_req) {
					/* passed in register */
					env->cg->reg_param_req[cur_pn] = temp_req;
				}
				else {
					/* passed on stack */
					if (mode_is_float(mode))
						irn = new_rd_ia32_fStackParam(get_irn_dbg_info(projargs[i]), irg, block, proj_M, mode);
					else
						irn = new_rd_ia32_StackParam(get_irn_dbg_info(projargs[i]), irg, block, proj_M, mode);

					set_ia32_pncode(irn, cur_pn);

					/* kill the old "Proj Arg" and replace with the new stack param */
					exchange(projargs[i], irn);
				}
			}

			free(projargs);

			break;
		case pn_Start_P_frame_base:
		case pn_Start_X_initial_exec:
		case pn_Start_M:
		case pn_Start_P_globals:
		case pn_Start_P_value_arg_base:
			break;
		default:
			assert(0 && "unsupported Proj(Start)");
	}

	return NULL;
}

/**
 * Transform some Proj's (Proj_Proj, Proj_Start, Proj_Cmp, Proj_Cond, Proj_Call).
 * All others are ignored.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Proj node
 * @param mode    mode of the Proj
 * @return The transformed node.
 */
static ir_node *gen_Proj(ia32_transform_env_t *env) {
	ir_node *new_node  = NULL;
	ir_node *pred      = get_Proj_pred(env->irn);

	if (env->mode == mode_M)
		return NULL;

	if (get_irn_op(pred) == op_Start) {
		new_node = gen_Proj_Start(env, env->irn, pred);
	}

	return new_node;
}



/**
 * Transforms an Alloc node into either ia32_Alloca or ia32_Malloc.
 */
static ir_node *gen_Alloc(ia32_transform_env_t *env) {
	dbg_info *dbg   = env->dbg;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;
	ir_node  *irn   = env->irn;
	ir_mode  *mode  = env->mode;
	ir_node  *size  = get_Alloc_size(irn);
	ir_node  *mem   = get_Alloc_mem(irn);
	ir_node  *res;

	if (get_Alloc_where(irn) == stack_alloc) {
		if (is_ia32_Const(size)) {
			res = new_rd_ia32_Alloca_i(dbg, irg, block, mem, mode);
			set_ia32_Immop_attr(res, size);
		}
		else {
			res = new_rd_ia32_Alloca(dbg, irg, block, size, mem, mode);
		}
	}
	else {
		assert(0 && "malloc should be already lowered");
		res = NULL;
	}

	return res;
}

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
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void ia32_transform_node(ir_node *node, void *env) {
	ia32_code_gen_t *cgenv = (ia32_code_gen_t *)env;
	opcode  code           = get_irn_opcode(node);
	ir_node *asm_node      = NULL;
	ia32_transform_env_t  tenv;

	if (is_Block(node))
		return;

	tenv.arch_env = cgenv->arch_env;
	tenv.block    = get_nodes_block(node);
	tenv.dbg      = get_irn_dbg_info(node);
	tenv.irg      = current_ir_graph;
	tenv.irn      = node;
	tenv.mod      = cgenv->mod;
	tenv.mode     = get_irn_mode(node);
	tenv.cg       = cgenv;

#define UNOP(a)        case iro_##a: asm_node = gen_##a(&tenv, get_##a##_op(node)); break
#define BINOP(a)       case iro_##a: asm_node = gen_##a(&tenv, get_##a##_left(node), get_##a##_right(node)); break
#define BINOP_COM(a)   case iro_##a: asm_node = gen_arith_Op(&tenv, get_##a##_left(node), get_##a##_right(node), 1); break
#define BINOP_NCOM(a)  case iro_##a: asm_node = gen_arith_Op(&tenv, get_##a##_left(node), get_##a##_right(node), 0); break
#define GEN(a)         case iro_##a: asm_node = gen_##a(&tenv); break
#define IGN(a)         case iro_##a: break
#define BAD(a)         case iro_##a: goto bad

	DBG((tenv.mod, LEVEL_1, "check %+F ... ", node));

	switch (code) {
		BINOP_COM(Add);
		BINOP_COM(Mul);
		BINOP_COM(And);
		BINOP_COM(Or);
		BINOP_COM(Eor);

		BINOP_NCOM(Sub);
		BINOP_NCOM(Shl);
		BINOP_NCOM(Shr);
		BINOP_NCOM(Shrs);
		BINOP_NCOM(Quot);
		BINOP_NCOM(Div);
		BINOP_NCOM(Mod);
		BINOP_NCOM(DivMod);

		UNOP(Minus);
		UNOP(Conv);
		UNOP(Abs);
		UNOP(Not);

		GEN(Load);
		GEN(Store);
		GEN(Call);
		GEN(Cond);
		GEN(Proj);
		GEN(Alloc);

		IGN(Block);
		IGN(Start);
		IGN(End);
		IGN(NoMem);
		IGN(Phi);
		IGN(IJmp);
		IGN(Break);
		IGN(Cmp);
		IGN(Unknown);
		/* constant transformation happens earlier */
		IGN(Const);
		IGN(SymConst);

		BAD(Raise);
		BAD(Sel);
		BAD(InstOf);
		BAD(Cast);
		BAD(Free);
		BAD(Sync);
		BAD(Tuple);
		BAD(Id);
		BAD(Bad);
		BAD(Confirm);
		BAD(Filter);
		BAD(CallBegin);
		BAD(EndReg);
		BAD(EndExcept);
		BAD(Mux);
		BAD(CopyB);

		default:
			if (get_irn_op(node) == get_op_Max() ||
				get_irn_op(node) == get_op_Min() ||
				get_irn_op(node) == get_op_Mulh())
			{
				asm_node = gen_arith_Op(&tenv, get_irn_n(node, 0), get_irn_n(node, 1), 1);
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
