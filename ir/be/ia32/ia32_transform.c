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

#include "gen_ia32_regalloc_if.h"

extern ir_op *get_op_Mulh(void);

static int maxnum_gpreg_args = 3;   /* maximum number of int arguments passed in registers; default 3 */
static int maxnum_fpreg_args = 5;   /* maximum number of float arguments passed in registers; default 5 */

static const ia32_register_req_t **current_gpreg_param_req;
static const ia32_register_req_t **current_fpreg_param_req;

/* this is the order of the assigned registers usesd for parameter passing */

const ia32_register_req_t *gpreg_param_req_std[] = {
	&ia32_default_req_ia32_general_purpose_eax,
	&ia32_default_req_ia32_general_purpose_ecx,
	&ia32_default_req_ia32_general_purpose_edx,
	&ia32_default_req_ia32_general_purpose_ebx,
	&ia32_default_req_ia32_general_purpose_edi,
	&ia32_default_req_ia32_general_purpose_esi
};

const ia32_register_req_t *gpreg_param_req_this[] = {
	&ia32_default_req_ia32_general_purpose_ecx,
	&ia32_default_req_ia32_general_purpose_eax,
	&ia32_default_req_ia32_general_purpose_edx,
	&ia32_default_req_ia32_general_purpose_ebx,
	&ia32_default_req_ia32_general_purpose_edi,
	&ia32_default_req_ia32_general_purpose_esi
};

const ia32_register_req_t *fpreg_param_req_std[] = {
	&ia32_default_req_ia32_floating_point_xmm0,
	&ia32_default_req_ia32_floating_point_xmm1,
	&ia32_default_req_ia32_floating_point_xmm2,
	&ia32_default_req_ia32_floating_point_xmm3,
	&ia32_default_req_ia32_floating_point_xmm4,
	&ia32_default_req_ia32_floating_point_xmm5,
	&ia32_default_req_ia32_floating_point_xmm6,
	&ia32_default_req_ia32_floating_point_xmm7
};

const ia32_register_req_t *fpreg_param_req_this[] = {
	NULL,  /* in case of a "this" pointer, the first parameter must not be a float */
	&ia32_default_req_ia32_floating_point_xmm0,
	&ia32_default_req_ia32_floating_point_xmm1,
	&ia32_default_req_ia32_floating_point_xmm2,
	&ia32_default_req_ia32_floating_point_xmm3,
	&ia32_default_req_ia32_floating_point_xmm4,
	&ia32_default_req_ia32_floating_point_xmm5,
	&ia32_default_req_ia32_floating_point_xmm6,
	&ia32_default_req_ia32_floating_point_xmm7
};

/* this is a struct to minimize the number of parameters
   passed to each gen_xxx function */
typedef struct _transform_env_t {
	const arch_env_t  *arch_env;   /**<< The arch_env */
	firm_dbg_module_t *mod;        /**<< The firm debugger */
	dbg_info          *dbg;        /**<< The node debug info */
	ir_graph          *irg;        /**<< The irg, the node should be created in */
	ir_node           *block;      /**<< The block, the node should belong to */
	ir_node           *irn;        /**<< The irn, to be transformed */
	ir_mode           *mode;       /**<< The mode of the irn */
} transform_env_t;


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
static ir_node *gen_imm_Add(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
			DBG((env->mod, LEVEL_2, "optimizing Add(1) to Inc ... "));
			new_op = new_rd_ia32_Inc(dbg, irg, block, expr_op, mode);
		}
		else if (class_tv == TV_CLASSIFY_ALL_ONE || class_negtv == TV_CLASSIFY_ONE) { /* + (-1) == DEC */
			DBG((mod, LEVEL_2, "optimizing Add(-1) to Dec ... "));
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
static ir_node *gen_Add(transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *shli_op;
	ir_node *expr_op;
	ir_node *new_op;
	int normal_add  = 0;
	firm_dbg_module_t *mod   = env->mod;
	dbg_info          *dbg   = env->dbg;
	ir_mode           *mode  = env->mode;
	ir_graph          *irg   = env->irg;
	ir_node           *block = env->block;

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
ir_node *generate_Mul(transform_env_t *env, ir_node *op1, ir_node *op2, op_flavour_t mul_flav, int is_imm_op) {
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
		set_ia32_Immop_attr(res, op2);
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
static ir_node *gen_imm_Mul(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
ir_node *gen_Mul(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_Mulh(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
static ir_node *gen_Mulh(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_And(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
static ir_node *gen_And(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_Or(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
static ir_node *gen_Or(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_Eor(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
static ir_node *gen_Eor(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_Max(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_Min(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_Sub(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
			DBG((mod, LEVEL_2, "optimizing Sub(1) to Dec ... "));
			new_op = new_rd_ia32_Dec(dbg, irg, block, expr_op, mode);
		}
		else if (class_negtv == TV_CLASSIFY_ONE) { /* - (-1) == Sub */
			DBG((mod, LEVEL_2, "optimizing Sub(-1) to Inc ... "));
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
static ir_node *gen_Sub(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *generate_DivMod(transform_env_t *env, ir_node *dividend, ir_node *divisor, op_flavour_t dm_flav) {
	ir_node  *res, *proj;
	ir_node  *edx_node, *cltd;
	ir_node  *in_keep[1];
	dbg_info *dbg   = env->dbg;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;
	ir_mode  *mode  = env->mode;
	ir_node  *irn   = env->irn;
	ir_node  *mem   = get_DivMod_mem(irn);

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
static ir_node *gen_Mod(transform_env_t *env, ir_node *op1, ir_node *op2) {
	return generate_DivMod(env, op1, op2, flavour_Mod);
}



/**
 * Wrapper for generate_DivMod. Sets flavour_Div.
 */
static ir_node *gen_Div(transform_env_t *env, ir_node *op1, ir_node *op2) {
	return generate_DivMod(env, op1, op2, flavour_Div);
}



/**
 * Wrapper for generate_DivMod. Sets flavour_DivMod.
 */
static ir_node *gen_DivMod(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_Quot(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_Shl(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
static ir_node *gen_Shl(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_Shr(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
static ir_node *gen_Shr(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_Shrs(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
static ir_node *gen_Shrs(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_RotL(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_RotR(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
static ir_node *gen_imm_Rot(transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
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
static ir_node *gen_Rot(transform_env_t *env, ir_node *op1, ir_node *op2) {
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
				DBG((env->mod, LEVEL_1, "optimizing RotL into RotR ... "));
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
static ir_node *gen_arith_Op(transform_env_t *env, ir_node *op1, ir_node *op2, int com) {
	firm_dbg_module_t *mod      = env->mod;
	dbg_info          *dbg      = env->dbg;
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
		DBG((mod, LEVEL_2, "found unexpected %s(Const, Const), creating binop ... ", get_irn_opname(node)));
		imm_op = NULL;
	}

	/* There are arithmetic operations which can't take an immediate */
	switch(opc) {
		case iro_Div:
		case iro_Mod:
		case iro_DivMod:
			DBG((mod, LEVEL_2, "Div/Mod/DivMod imm not available, creating binop ... "));
			imm_op = NULL;
			break;
		default:
			if (op == get_op_Min() || op == get_op_Max()) {
				DBG((mod, LEVEL_2, "MIN/MAX imm not available, creating binop ... "));
				imm_op = NULL;
			}
			break;
	}

	DBG((mod, LEVEL_1, "(op1: %s -- op2: %s) ... ", get_irn_opname(op1), get_irn_opname(op2)));

	if (!mode_is_float(env->mode) && imm_op) {
		DBG((mod, LEVEL_1, "%s with imm ... ", get_irn_opname(node)));

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
		DBG((mod, LEVEL_1, "%s as binop ... ", get_irn_opname(node)));

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
static ir_node *gen_Minus(transform_env_t *env, ir_node *op) {
	if (is_ia32_Minus(op) || is_ia32_fMinus(op)) {
		DBG((env->mod, LEVEL_1, "optimizing --(e) to e ..."));
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
static ir_node *gen_Conv(transform_env_t *env, ir_node *op) {
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
static ir_node *gen_Not(transform_env_t *env, ir_node *op) {
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
static ir_node *gen_Abs(transform_env_t *env, ir_node *op) {
	ir_node           *res, *p_eax, *p_edx;
	firm_dbg_module_t *mod   = env->mod;
	dbg_info          *dbg   = env->dbg;
	ir_mode           *mode  = env->mode;
	ir_graph          *irg   = env->irg;
	ir_node           *block = env->block;

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
static ir_node *gen_Load(transform_env_t *env) {
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
ir_node *gen_Store(transform_env_t *env) {
	ir_node *node = env->irn;

	if (mode_is_float(env->mode)) {
		return new_rd_ia32_fStore(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	return new_rd_ia32_Store(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
}



/**
 * Check all parameters and determine the maximum number of parameters
 * to pass in gp regs resp. in fp regs.
 */
static void get_n_regparam_class(int n, ir_node **param, int *n_int, int *n_float) {
	int i;

	for (i = 0; i < n; i++) {
		if (mode_is_int(get_irn_mode(param[i])))
			*n_int = *n_int + 1;
		else if (mode_is_float(get_irn_mode(param[i])))
			*n_float = *n_float + 1;

		/* test for maximum */
		if (*n_int == maxnum_gpreg_args)
			break;

		if (*n_float == maxnum_fpreg_args)
			break;
	}
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
static ir_node *gen_Call(transform_env_t *env) {
	const ia32_register_req_t **in_req;
	ir_node          **in;
	ir_node           *new_call, *sync;
	ir_mode           *mode;
	int                i, j, n_new_call_in;
	asmop_attr        *attr;
	firm_dbg_module_t *mod          = env->mod;
	dbg_info          *dbg          = env->dbg;
	ir_graph          *irg          = env->irg;
	ir_node           *block        = env->block;
	ir_node           *call         = env->irn;
	ir_node          **stack_param  = NULL;
	ir_node          **param        = get_Call_param_arr(call);
	ir_node           *call_Mem     = get_Call_mem(call);
	unsigned           cc           = get_method_calling_convention(get_Call_type(call));
	int                n            = get_Call_n_params(call);
	int                n_gpregparam = 0;
	int                n_fpregparam = 0;
	int                cur_gp_idx   = 0;
	int                cur_fp_idx   = 0;
	int                stack_idx    = 0;
	int                done         = 0;

	if (cc & cc_reg_param)
		get_n_regparam_class(n, param, &n_gpregparam, &n_fpregparam);

	/* do we need to pass arguments on stack? */
	if (n - n_gpregparam - n_fpregparam > 0)
		stack_param = calloc(n - n_gpregparam - n_fpregparam, sizeof(ir_node *));

	/* we need at least one in, either for the stack params or the call_Mem */
	n_new_call_in = 1 + n_gpregparam + n_fpregparam;

	current_gpreg_param_req = gpreg_param_req_std;
	current_fpreg_param_req = fpreg_param_req_std;

	if (cc & cc_this_call) {
		current_gpreg_param_req = gpreg_param_req_this;
		current_fpreg_param_req = fpreg_param_req_this;
	}

	/* the call has one IN for all stack parameter and one IN for each reg param */
	in     = calloc(n_new_call_in, sizeof(ir_node *));
	in_req = calloc(n_new_call_in, sizeof(arch_register_req_t *));

	/* loop over all parameters and determine whether its a int or float register parameter */
	for (i = 0; i < n && !done && (cc & cc_reg_param); i++) {
		mode = get_irn_mode(param[i]);

		if (mode_is_int(mode) && cur_gp_idx < maxnum_gpreg_args) {
			/* param can be passed in general purpose register and we have some registers left */
			in[cur_gp_idx + cur_fp_idx] = param[i];
			in_req[cur_gp_idx] = current_gpreg_param_req[cur_gp_idx];
			cur_gp_idx++;
		}
		else if (mode_is_float(mode) && cur_fp_idx < maxnum_fpreg_args) {
			/* param can be passed in floating point register and we have some registers left */
			assert(current_gpreg_param_req[cur_fp_idx] && "'this' pointer cannot be passed as float");
			in[cur_gp_idx + cur_fp_idx] = param[i];
			in_req[cur_fp_idx] = current_gpreg_param_req[cur_fp_idx];
			cur_fp_idx++;
		}

		/* maximum number of register parameters in one class reached? */
		if (cur_gp_idx >= maxnum_gpreg_args || cur_fp_idx >= maxnum_fpreg_args) {
			done      = 1;
		}
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
		}
	}

	if (stack_param) {
		sync = new_r_Sync(irg, block, n - n_gpregparam - n_fpregparam, stack_param);
		in[n_new_call_in - 1] = sync;
	}
	else {
		in[n_new_call_in - 1] = call_Mem;
	}

	/* create the new node */
	new_call = new_rd_ia32_Call(dbg, irg, block, n_new_call_in, in);
	set_ia32_Immop_attr(new_call, get_Call_ptr(call));
	set_ia32_n_res(new_call, 1);

	/* set register requirements for in and out */
	attr             = get_ia32_attr(new_call);
	attr->in_req     = in_req;
	attr->out_req    = calloc(1, sizeof(ia32_register_req_t *));
	attr->out_req[0] = &ia32_default_req_ia32_general_purpose_eax;
	attr->slots      = calloc(1, sizeof(arch_register_t *));

	/* stack parameter has no OUT register */
	attr->in_req[n_new_call_in - 1] = &ia32_default_req_none;

	return new_call;
}



/**
 * creates a unique ident by adding a number to a tag
 *
 * @param tag   the tag string, must contain a %d if a number
 *              should be added
 */
static ident *unique_id(const char *tag)
{
  static unsigned id = 0;
  char str[256];

  snprintf(str, sizeof(str), tag, ++id);
  return new_id_from_str(str);
}

/**
 * Transforms a SymConst.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir SymConst node
 * @param mode    mode of the SymConst
 * @return the created ia32 Const node
 */
static ir_node *gen_SymConst(transform_env_t *env) {
	ir_node           *cnst;
	firm_dbg_module_t *mod   = env->mod;
	dbg_info          *dbg   = env->dbg;
	ir_mode           *mode  = env->mode;
	ir_graph          *irg   = env->irg;
	ir_node           *block = env->block;

	if (mode_is_float(mode)) {
		cnst = new_rd_ia32_fConst(dbg, irg, block, mode);

	}
	else {
		cnst = new_rd_ia32_Const(dbg, irg, block, mode);
	}

	set_ia32_Const_attr(cnst, env->irn);
	return cnst;
}

/**
 * Transforms a Const.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Const node
 * @param mode    mode of the Const
 * @return the created ia32 Const node
 */
static ir_node *gen_Const(transform_env_t *env) {
	ir_node *cnst;
	entity  *ent;
	type    *tp;
	symconst_symbol sym;
	firm_dbg_module_t *mod   = env->mod;
	dbg_info          *dbg   = env->dbg;
	ir_mode           *mode  = env->mode;
	ir_graph          *irg   = env->irg;
	ir_node           *block = env->block;
	ir_node           *node  = env->irn;

	if (mode_is_float(mode)) {
		tp  = get_Const_type(node);
		if (tp == firm_unknown_type) {
			tp = new_type_primitive(unique_id("tp_ia32_float_%u"), mode);
		}

		ent = new_entity(get_glob_type(), unique_id("ia32FloatCnst_%u"), tp);

		set_entity_ld_ident(ent, get_entity_ident(ent));
		set_entity_visibility(ent, visibility_local);
		set_entity_variability(ent, variability_constant);
		set_entity_allocation(ent, allocation_static);

		set_atomic_ent_value(ent, node);

		sym.entity_p = ent;

		cnst = new_rd_SymConst(dbg, irg, block, sym, symconst_addr_ent);
		env->irn = cnst;
		cnst = gen_SymConst(env);
	}
	else {
		cnst = new_rd_ia32_Const(dbg, irg, block, mode);
		set_ia32_Const_attr(cnst, node);
	}

	return cnst;
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
static ir_node *gen_Cond(transform_env_t *env) {
	firm_dbg_module_t *mod      = env->mod;
	dbg_info          *dbg      = env->dbg;
	ir_mode           *mode     = env->mode;
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
static ir_node *gen_Proj_Start(transform_env_t *env, ir_node *proj, ir_node *start) {
	const ir_edge_t   *edge;
	ir_node           *succ, *irn;
	ir_node          **projargs;
	ir_mode           *mode;
	int                n, i;
	unsigned           cc;
	ir_node           *proj_M     = get_irg_initial_mem(current_ir_graph);
	entity            *irg_ent    = get_irg_entity(current_ir_graph);
	type              *tp         = get_entity_type(irg_ent);
	int                cur_gp_idx = 0;
	int                cur_fp_idx = 0;
	int                stack_idx  = 0;
	int                done       = 0;
	firm_dbg_module_t *mod        = env->mod;
	dbg_info          *dbg        = env->dbg;
	ir_graph          *irg        = env->irg;
	ir_node           *block      = env->block;

	assert(is_Method_type(tp) && "irg type is not a method");

	switch(get_Proj_proj(proj)) {
		case pn_Start_T_args:
			/* We cannot use get_method_n_params here as the function might
			   be variadic or one argument is not used. */
			n = get_irn_n_edges(proj);

			/* we are done here when there are no parameters */
			if (n < 1)
				break;

			/* temporary remember all proj arg x */
			projargs = calloc(n, sizeof(ir_node *));

			i = 0;
			foreach_out_edge((const ir_node *)proj, edge) {
				succ = get_edge_src_irn(edge);
				assert(is_Proj(succ) && "non-Proj from a Proj_T (pn_Start_T_args).");
				projargs[i++] = succ;
			}

			cc = get_method_calling_convention(tp);

			/* get the correct order in case of 'this' call */
			current_gpreg_param_req = gpreg_param_req_std;
			current_fpreg_param_req = fpreg_param_req_std;
			if (cc & cc_this_call) {
				current_gpreg_param_req = gpreg_param_req_this;
				current_fpreg_param_req = fpreg_param_req_this;
			}

			/* loop over all parameters and check whether its a int or float */
			for (i = 0; i < n && !done && (cc & cc_reg_param); i++) {
				mode = get_irn_mode(projargs[i]);

				if (mode_is_int(mode) && cur_gp_idx < maxnum_gpreg_args) {
					/* parameter got passed in general purpose register */
					irn = new_rd_ia32_RegParam(get_irn_dbg_info(proj), irg, block, proj, mode);
					set_ia32_pncode(irn, get_Proj_proj(projargs[i]));
					set_ia32_req_out(irn, current_gpreg_param_req[cur_gp_idx], 0);
					cur_gp_idx++;
				}
				else if (mode_is_float(mode) && cur_fp_idx < maxnum_fpreg_args) {
					/* parameter got passed in floating point register*/
					irn = new_rd_ia32_RegParam(get_irn_dbg_info(proj), irg, block, proj, mode);
					set_ia32_pncode(irn, get_Proj_proj(projargs[i]));
					set_ia32_req_out(irn, current_fpreg_param_req[cur_fp_idx], 0);
					cur_fp_idx++;
				}

				/* kill the old "Proj Arg" and replace with the new Arg */
				exchange(projargs[i], irn);

				if (cur_gp_idx >= maxnum_gpreg_args || cur_fp_idx >= maxnum_fpreg_args) {
					stack_idx = i;
					done      = 1;
				}
			}

			/* create all remaining stack parameters */
			for (i = stack_idx; i < n; i++) {
				mode = get_irn_mode(projargs[i]);

				if (mode_is_float(mode))
					irn = new_rd_ia32_fStackParam(get_irn_dbg_info(projargs[i]), irg, block, proj_M, mode);
				else
					irn = new_rd_ia32_StackParam(get_irn_dbg_info(projargs[i]), irg, block, proj_M, mode);

				set_ia32_pncode(irn, get_Proj_proj(projargs[i]));

				/* kill the old "Proj Arg" and replace with the new stack param */
				exchange(projargs[i], irn);
			}

			free(projargs);

			break;
		case pn_Start_X_initial_exec:
		case pn_Start_M:
		case pn_Start_P_frame_base:
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
static ir_node *gen_Proj(transform_env_t *env) {
	ir_node *new_node  = NULL;
	ir_node *pred      = get_Proj_pred(env->irn);

	if (env->mode == mode_M)
		return NULL;

	if (get_irn_op(pred) == op_Start) {
		new_node = gen_Proj_Start(env, env->irn, pred);
	}

	return new_node;
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
	transform_env_t  tenv;

	if (is_Block(node))
		return;

	tenv.arch_env = cgenv->arch_env;
	tenv.block    = get_nodes_block(node);
	tenv.dbg      = get_irn_dbg_info(node);
	tenv.irg      = current_ir_graph;
	tenv.irn      = node;
	tenv.mod      = cgenv->mod;
	tenv.mode     = get_irn_mode(node);

#define UNOP(a)        case iro_##a: asm_node = gen_##a(&tenv, get_##a##_op(node)); break
#define BINOP(a)       case iro_##a: asm_node = gen_##a(&tenv, get_##a##_left(node), get_##a##_right(node)); break
#define BINOP_COM(a)   case iro_##a: asm_node = gen_arith_Op(&tenv, get_##a##_left(node), get_##a##_right(node), 1); break
#define BINOP_NCOM(a)  case iro_##a: asm_node = gen_arith_Op(&tenv, get_##a##_left(node), get_##a##_right(node), 0); break
#define GEN(a)         case iro_##a: asm_node = gen_##a(&tenv); break
#define IGN(a)         case iro_##a: break
#define BAD(a)         case iro_##a: goto bad

	DBG((tenv.mod, LEVEL_1, "transforming node %s (%ld) ... ", get_irn_opname(node), get_irn_node_nr(node)));

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
		GEN(Const);
		GEN(SymConst);
		GEN(Cond);

		GEN(Proj);

		IGN(Block);
		IGN(Start);
		IGN(End);
		IGN(NoMem);
		IGN(Phi);
		IGN(IJmp);
		IGN(Break);
		IGN(Cmp);

		BAD(Raise);
		BAD(Sel);
		BAD(InstOf);
		BAD(Cast);
		BAD(Alloc);
		BAD(Free);
		BAD(Sync);
		BAD(Tuple);
		BAD(Id);
		BAD(Bad);
		BAD(Confirm);
		BAD(Unknown);
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
		DBG((tenv.mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DBG((tenv.mod, LEVEL_1, "ignored\n"));
	}
}
