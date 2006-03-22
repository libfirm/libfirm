/**
 * This file implements the IR transformation from firm into
 * ia32-Firm.
 *
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irargs_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "irop_t.h"
#include "irprog_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irvrfy.h"
#include "ircons.h"
#include "dbginfo.h"
#include "debug.h"

#include "../benode_t.h"
#include "../besched.h"

#include "bearch_ia32_t.h"

#include "ia32_nodes_attr.h"
#include "../arch/archop.h"     /* we need this for Min and Max nodes */
#include "ia32_transform.h"
#include "ia32_new_nodes.h"
#include "ia32_map_regs.h"

#include "gen_ia32_regalloc_if.h"

#ifdef NDEBUG
#define SET_IA32_ORIG_NODE(n, o)
#else
#define SET_IA32_ORIG_NODE(n, o) set_ia32_orig_node(n, o);
#endif /* NDEBUG */


#define SFP_SIGN "0x80000000"
#define DFP_SIGN "0x8000000000000000"
#define SFP_ABS  "0x7FFFFFFF"
#define DFP_ABS  "0x7FFFFFFFFFFFFFFF"

#define TP_SFP_SIGN "ia32_sfp_sign"
#define TP_DFP_SIGN "ia32_dfp_sign"
#define TP_SFP_ABS  "ia32_sfp_abs"
#define TP_DFP_ABS  "ia32_dfp_abs"

#define ENT_SFP_SIGN "IA32_SFP_SIGN"
#define ENT_DFP_SIGN "IA32_DFP_SIGN"
#define ENT_SFP_ABS  "IA32_SFP_ABS"
#define ENT_DFP_ABS  "IA32_DFP_ABS"

extern ir_op *get_op_Mulh(void);

typedef ir_node *construct_binop_func(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *base, ir_node *index, \
									  ir_node *op1, ir_node *op2, ir_node *mem, ir_mode *mode);

typedef ir_node *construct_unop_func(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *base, ir_node *index, \
									 ir_node *op, ir_node *mem, ir_mode *mode);

typedef enum {
	ia32_SSIGN, ia32_DSIGN, ia32_SABS, ia32_DABS, ia32_known_const_max
} ia32_known_const_t;

/****************************************************************************************************
 *                  _        _                        __                           _   _
 *                 | |      | |                      / _|                         | | (_)
 *  _ __   ___   __| | ___  | |_ _ __ __ _ _ __  ___| |_ ___  _ __ _ __ ___   __ _| |_ _  ___  _ __
 * | '_ \ / _ \ / _` |/ _ \ | __| '__/ _` | '_ \/ __|  _/ _ \| '__| '_ ` _ \ / _` | __| |/ _ \| '_ \
 * | | | | (_) | (_| |  __/ | |_| | | (_| | | | \__ \ || (_) | |  | | | | | | (_| | |_| | (_) | | | |
 * |_| |_|\___/ \__,_|\___|  \__|_|  \__,_|_| |_|___/_| \___/|_|  |_| |_| |_|\__,_|\__|_|\___/|_| |_|
 *
 ****************************************************************************************************/

/* Generates an entity for a known FP const (used for FP Neg + Abs) */
static const char *gen_fp_known_const(ir_mode *mode, ia32_known_const_t kct) {
	static const struct {
		const char *tp_name;
		const char *ent_name;
		const char *cnst_str;
	} names [ia32_known_const_max] = {
		{ TP_SFP_SIGN, ENT_SFP_SIGN, SFP_SIGN },	/* ia32_SSIGN */
		{ TP_DFP_SIGN, ENT_DFP_SIGN, DFP_SIGN },	/* ia32_DSIGN */
		{ TP_SFP_ABS,  ENT_SFP_ABS,  SFP_ABS },		/* ia32_SABS */
		{ TP_DFP_ABS,  ENT_DFP_ABS,  DFP_ABS }		/* ia32_DABS */
	};
	static struct entity *ent_cache[ia32_known_const_max];

	const char    *tp_name, *ent_name, *cnst_str;
	ir_type       *tp;
	ir_node       *cnst;
	ir_graph      *rem;
	entity        *ent;
	tarval        *tv;

  ent_name = names[kct].ent_name;
	if (! ent_cache[kct]) {
		tp_name  = names[kct].tp_name;
		cnst_str = names[kct].cnst_str;

		tv  = new_tarval_from_str(cnst_str, strlen(cnst_str), mode);
		tp  = new_type_primitive(new_id_from_str(tp_name), mode);
		ent = new_entity(get_glob_type(), new_id_from_str(ent_name), tp);

		set_entity_ld_ident(ent, get_entity_ident(ent));
		set_entity_visibility(ent, visibility_local);
		set_entity_variability(ent, variability_constant);
		set_entity_allocation(ent, allocation_static);

		/* we create a new entity here: It's initialization must resist on the
		    const code irg */
		rem = current_ir_graph;
		current_ir_graph = get_const_code_irg();
		cnst = new_Const(mode, tv);
		current_ir_graph = rem;

		set_atomic_ent_value(ent, cnst);

		/* cache the entry */
		ent_cache[kct] = ent;
	}
	return ent_name;
}

#ifndef NDEBUG
/**
 * Prints the old node name on cg obst and returns a pointer to it.
 */
const char *get_old_node_name(ia32_transform_env_t *env) {
	ia32_isa_t *isa = (ia32_isa_t *)env->cg->arch_env->isa;

	lc_eoprintf(firm_get_arg_env(), isa->name_obst, "%+F", env->irn);
	obstack_1grow(isa->name_obst, 0);
	isa->name_obst_size += obstack_object_size(isa->name_obst);
 	return obstack_finish(isa->name_obst);
}
#endif /* NDEBUG */

/* determine if one operator is an Imm */
static ir_node *get_immediate_op(ir_node *op1, ir_node *op2) {
	if (op1)
		return is_ia32_Cnst(op1) ? op1 : (is_ia32_Cnst(op2) ? op2 : NULL);
	else return is_ia32_Cnst(op2) ? op2 : NULL;
}

/* determine if one operator is not an Imm */
static ir_node *get_expr_op(ir_node *op1, ir_node *op2) {
	return !is_ia32_Cnst(op1) ? op1 : (!is_ia32_Cnst(op2) ? op2 : NULL);
}


/**
 * Construct a standard binary operation, set AM and immediate if required.
 *
 * @param env   The transformation environment
 * @param op1   The first operand
 * @param op2   The second operand
 * @param func  The node constructor function
 * @return The constructed ia32 node.
 */
static ir_node *gen_binop(ia32_transform_env_t *env, ir_node *op1, ir_node *op2, construct_binop_func *func) {
	ir_node           *new_op   = NULL;
	ir_mode           *mode     = env->mode;
	dbg_info          *dbg      = env->dbg;
	ir_graph          *irg      = env->irg;
	ir_node           *block    = env->block;
	firm_dbg_module_t *mod      = env->mod;
	ir_node           *noreg_gp = ia32_new_NoReg_gp(env->cg);
	ir_node           *noreg_fp = ia32_new_NoReg_fp(env->cg);
	ir_node           *nomem    = new_NoMem();
	ir_node           *expr_op, *imm_op;

	/* Check if immediate optimization is on and */
	/* if it's an operation with immediate.      */
	if (! env->cg->opt.immops) {
		expr_op = op1;
		imm_op  = NULL;
	}
	else if (is_op_commutative(get_irn_op(env->irn))) {
		imm_op  = get_immediate_op(op1, op2);
		expr_op = get_expr_op(op1, op2);
	}
	else {
		imm_op  = get_immediate_op(NULL, op2);
		expr_op = get_expr_op(op1, op2);
	}

	assert((expr_op || imm_op) && "invalid operands");

	if (!expr_op) {
		/* We have two consts here: not yet supported */
		imm_op = NULL;
	}

	if (mode_is_float(mode)) {
		/* floating point operations */
		if (imm_op) {
			DB((mod, LEVEL_1, "FP with immediate ..."));
			new_op = func(dbg, irg, block, noreg_gp, noreg_gp, expr_op, noreg_fp, nomem, mode_T);
			set_ia32_Immop_attr(new_op, imm_op);
			set_ia32_am_support(new_op, ia32_am_None);
		}
		else {
			DB((mod, LEVEL_1, "FP binop ..."));
			new_op = func(dbg, irg, block, noreg_gp, noreg_gp, op1, op2, nomem, mode_T);
			set_ia32_am_support(new_op, ia32_am_Source);
		}
	}
	else {
		/* integer operations */
		if (imm_op) {
			/* This is expr + const */
			DB((mod, LEVEL_1, "INT with immediate ..."));
			new_op = func(dbg, irg, block, noreg_gp, noreg_gp, expr_op, noreg_gp, nomem, mode_T);
			set_ia32_Immop_attr(new_op, imm_op);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Dest);
		}
		else {
			DB((mod, LEVEL_1, "INT binop ..."));
			/* This is a normal operation */
			new_op = func(dbg, irg, block, noreg_gp, noreg_gp, op1, op2, nomem, mode_T);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Full);
		}
	}

  SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	set_ia32_res_mode(new_op, mode);

	if (is_op_commutative(get_irn_op(env->irn))) {
		set_ia32_commutative(new_op);
	}

	return new_rd_Proj(dbg, irg, block, new_op, mode, 0);
}



/**
 * Construct a shift/rotate binary operation, sets AM and immediate if required.
 *
 * @param env   The transformation environment
 * @param op1   The first operand
 * @param op2   The second operand
 * @param func  The node constructor function
 * @return The constructed ia32 node.
 */
static ir_node *gen_shift_binop(ia32_transform_env_t *env, ir_node *op1, ir_node *op2, construct_binop_func *func) {
	ir_node           *new_op = NULL;
	ir_mode           *mode   = env->mode;
	dbg_info          *dbg    = env->dbg;
	ir_graph          *irg    = env->irg;
	ir_node           *block  = env->block;
	firm_dbg_module_t *mod    = env->mod;
	ir_node           *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node           *nomem  = new_NoMem();
	ir_node           *expr_op, *imm_op;
	tarval            *tv;

	assert(! mode_is_float(mode) && "Shift/Rotate with float not supported");

	/* Check if immediate optimization is on and */
	/* if it's an operation with immediate.      */
	imm_op  = env->cg->opt.immops ? get_immediate_op(NULL, op2) : NULL;
	expr_op = get_expr_op(op1, op2);

	assert((expr_op || imm_op) && "invalid operands");

	if (!expr_op) {
		/* We have two consts here: not yet supported */
		imm_op = NULL;
	}

	/* Limit imm_op within range imm8 */
	if (imm_op) {
		tv = get_ia32_Immop_tarval(imm_op);

		if (tv) {
			tv = tarval_mod(tv, new_tarval_from_long(32, mode_Iu));
		}
		else {
			imm_op = NULL;
		}
	}

	/* integer operations */
	if (imm_op) {
		/* This is shift/rot with const */
		DB((mod, LEVEL_1, "Shift/Rot with immediate ..."));

		new_op = func(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem, mode_T);
		set_ia32_Immop_attr(new_op, imm_op);
	}
	else {
		/* This is a normal shift/rot */
		DB((mod, LEVEL_1, "Shift/Rot binop ..."));
		new_op = func(dbg, irg, block, noreg, noreg, op1, op2, nomem, mode_T);
	}

	/* set AM support */
	set_ia32_am_support(new_op, ia32_am_Dest);

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	set_ia32_res_mode(new_op, mode);

	return new_rd_Proj(dbg, irg, block, new_op, mode, 0);
}


/**
 * Construct a standard unary operation, set AM and immediate if required.
 *
 * @param env   The transformation environment
 * @param op    The operand
 * @param func  The node constructor function
 * @return The constructed ia32 node.
 */
static ir_node *gen_unop(ia32_transform_env_t *env, ir_node *op, construct_unop_func *func) {
	ir_node           *new_op = NULL;
	ir_mode           *mode   = env->mode;
	dbg_info          *dbg    = env->dbg;
	firm_dbg_module_t *mod    = env->mod;
	ir_graph          *irg    = env->irg;
	ir_node           *block  = env->block;
	ir_node           *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node           *nomem  = new_NoMem();

	new_op = func(dbg, irg, block, noreg, noreg, op, nomem, mode_T);

	if (mode_is_float(mode)) {
		DB((mod, LEVEL_1, "FP unop ..."));
		/* floating point operations don't support implicit store */
		set_ia32_am_support(new_op, ia32_am_None);
	}
	else {
		DB((mod, LEVEL_1, "INT unop ..."));
		set_ia32_am_support(new_op, ia32_am_Dest);
	}

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	set_ia32_res_mode(new_op, mode);

	return new_rd_Proj(dbg, irg, block, new_op, mode, 0);
}



/**
 * Creates an ia32 Add with immediate.
 *
 * @param env       The transformation environment
 * @param expr_op   The expression operator
 * @param const_op  The constant
 * @return the created ia32 Add node
 */
static ir_node *gen_imm_Add(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	ir_node                *new_op     = NULL;
	tarval                 *tv         = get_ia32_Immop_tarval(const_op);
	firm_dbg_module_t      *mod        = env->mod;
	dbg_info               *dbg        = env->dbg;
	ir_graph               *irg        = env->irg;
	ir_node                *block      = env->block;
	ir_node                *noreg      = ia32_new_NoReg_gp(env->cg);
	ir_node                *nomem      = new_NoMem();
	int                     normal_add = 1;
	tarval_classification_t class_tv, class_negtv;

	/* try to optimize to inc/dec  */
	if (env->cg->opt.incdec && tv) {
		/* optimize tarvals */
		class_tv    = classify_tarval(tv);
		class_negtv = classify_tarval(tarval_neg(tv));

		if (class_tv == TV_CLASSIFY_ONE) { /* + 1 == INC */
			DB((env->mod, LEVEL_2, "Add(1) to Inc ... "));
			new_op     = new_rd_ia32_Inc(dbg, irg, block, noreg, noreg, expr_op, nomem, mode_T);
			normal_add = 0;
		}
		else if (class_tv == TV_CLASSIFY_ALL_ONE || class_negtv == TV_CLASSIFY_ONE) { /* + (-1) == DEC */
			DB((mod, LEVEL_2, "Add(-1) to Dec ... "));
			new_op     = new_rd_ia32_Dec(dbg, irg, block, noreg, noreg, expr_op, nomem, mode_T);
			normal_add = 0;
		}
	}

	if (normal_add) {
		new_op = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem, mode_T);
		set_ia32_Immop_attr(new_op, const_op);
	}

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
	ir_node  *new_op = NULL;
	dbg_info *dbg    = env->dbg;
	ir_mode  *mode   = env->mode;
	ir_graph *irg    = env->irg;
	ir_node  *block  = env->block;
	ir_node  *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node  *nomem  = new_NoMem();
	ir_node  *expr_op, *imm_op;

	/* Check if immediate optimization is on and */
	/* if it's an operation with immediate.      */
	imm_op  = env->cg->opt.immops ? get_immediate_op(op1, op2) : NULL;
	expr_op = get_expr_op(op1, op2);

	assert((expr_op || imm_op) && "invalid operands");

	if (mode_is_float(mode)) {
		return gen_binop(env, op1, op2, new_rd_ia32_fAdd);
	}
	else {
		/* integer ADD */
		if (!expr_op) {
			/* No expr_op means, that we have two const - one symconst and */
			/* one tarval or another symconst - because this case is not   */
			/* covered by constant folding                                 */

			new_op = new_rd_ia32_Lea(dbg, irg, block, noreg, noreg, mode);
			add_ia32_am_offs(new_op, get_ia32_cnst(op1));
			add_ia32_am_offs(new_op, get_ia32_cnst(op2));

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Source);
			set_ia32_op_type(new_op, ia32_AddrModeS);
			set_ia32_am_flavour(new_op, ia32_am_O);

			/* Lea doesn't need a Proj */
			return new_op;
		}
		else if (imm_op) {
			/* This is expr + const */
			new_op = gen_imm_Add(env, expr_op, imm_op);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Dest);
		}
		else {
			/* This is a normal add */
			new_op = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, op1, op2, nomem, mode_T);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Full);
		}
	}

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	set_ia32_res_mode(new_op, mode);

	return new_rd_Proj(dbg, irg, block, new_op, mode, 0);
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
static ir_node *gen_Mul(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *new_op;

	if (mode_is_float(env->mode)) {
		new_op = gen_binop(env, op1, op2, new_rd_ia32_fMul);
	}
	else {
		new_op = gen_binop(env, op1, op2, new_rd_ia32_Mul);
	}

	return new_op;
}



/**
 * Creates an ia32 Mulh.
 * Note: Mul produces a 64Bit result and Mulh returns the upper 32 bit of
 * this result while Mul returns the lower 32 bit.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return the created ia32 Mulh node
 */
static ir_node *gen_Mulh(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *proj_EAX, *proj_EDX, *mulh;
	ir_node *in[1];

	assert(!mode_is_float(env->mode) && "Mulh with float not supported");
	proj_EAX = gen_binop(env, op1, op2, new_rd_ia32_Mulh);
	mulh     = get_Proj_pred(proj_EAX);
	proj_EDX = new_rd_Proj(env->dbg, env->irg, env->block, mulh, env->mode, pn_EDX);

	/* to be on the save side */
	set_Proj_proj(proj_EAX, pn_EAX);

	if (get_ia32_cnst(mulh)) {
		/* Mulh with const cannot have AM */
		set_ia32_am_support(mulh, ia32_am_None);
	}
	else {
		/* Mulh cannot have AM for destination */
		set_ia32_am_support(mulh, ia32_am_Source);
	}

	in[0] = proj_EAX;

	/* keep EAX */
	be_new_Keep(&ia32_reg_classes[CLASS_ia32_gp], env->irg, env->block, 1, in);

	return proj_EDX;
}



/**
 * Creates an ia32 And.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 And node
 */
static ir_node *gen_And(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	if (mode_is_float(env->mode)) {
		return gen_binop(env, op1, op2, new_rd_ia32_fAnd);
	}
	else {
		return gen_binop(env, op1, op2, new_rd_ia32_And);
	}
}



/**
 * Creates an ia32 Or.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 Or node
 */
static ir_node *gen_Or(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	if (mode_is_float(env->mode)) {
		return gen_binop(env, op1, op2, new_rd_ia32_fOr);
	}
	else {
		return gen_binop(env, op1, op2, new_rd_ia32_Or);
	}
}



/**
 * Creates an ia32 Eor.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 Eor node
 */
static ir_node *gen_Eor(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	if (mode_is_float(env->mode)) {
		return gen_binop(env, op1, op2, new_rd_ia32_fEor);
	}
	else {
		return gen_binop(env, op1, op2, new_rd_ia32_Eor);
	}
}



/**
 * Creates an ia32 Max.
 *
 * @param env      The transformation environment
 * @param op1      The first operator
 * @param op2      The second operator
 * @return the created ia32 Max node
 */
static ir_node *gen_Max(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *new_op;

	if (mode_is_float(env->mode)) {
		new_op = gen_binop(env, op1, op2, new_rd_ia32_fMax);
	}
	else {
		new_op = new_rd_ia32_Max(env->dbg, env->irg, env->block, op1, op2, env->mode);
		set_ia32_am_support(new_op, ia32_am_None);
		SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));
	}

	return new_op;
}



/**
 * Creates an ia32 Min.
 *
 * @param env      The transformation environment
 * @param op1      The first operator
 * @param op2      The second operator
 * @return the created ia32 Min node
 */
static ir_node *gen_Min(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *new_op;

	if (mode_is_float(env->mode)) {
		new_op = gen_binop(env, op1, op2, new_rd_ia32_fMin);
	}
	else {
		new_op = new_rd_ia32_Min(env->dbg, env->irg, env->block, op1, op2, env->mode);
		set_ia32_am_support(new_op, ia32_am_None);
		SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));
	}

	return new_op;
}



/**
 * Creates an ia32 Sub with immediate.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 Sub node
 */
static ir_node *gen_imm_Sub(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	ir_node                *new_op     = NULL;
	tarval                 *tv         = get_ia32_Immop_tarval(const_op);
	firm_dbg_module_t      *mod        = env->mod;
	dbg_info               *dbg        = env->dbg;
	ir_graph               *irg        = env->irg;
	ir_node                *block      = env->block;
	ir_node                *noreg      = ia32_new_NoReg_gp(env->cg);
	ir_node                *nomem      = new_NoMem();
	int                     normal_sub = 1;
	tarval_classification_t class_tv, class_negtv;

	/* try to optimize to inc/dec  */
	if (env->cg->opt.incdec && tv) {
		/* optimize tarvals */
		class_tv    = classify_tarval(tv);
		class_negtv = classify_tarval(tarval_neg(tv));

		if (class_tv == TV_CLASSIFY_ONE) { /* - 1 == DEC */
			DB((mod, LEVEL_2, "Sub(1) to Dec ... "));
			new_op     = new_rd_ia32_Dec(dbg, irg, block, noreg, noreg, expr_op, nomem, mode_T);
			normal_sub = 0;
		}
		else if (class_negtv == TV_CLASSIFY_ONE) { /* - (-1) == Sub */
			DB((mod, LEVEL_2, "Sub(-1) to Inc ... "));
			new_op     = new_rd_ia32_Inc(dbg, irg, block, noreg, noreg, expr_op, nomem, mode_T);
			normal_sub = 0;
		}
	}

	if (normal_sub) {
		new_op = new_rd_ia32_Sub(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem, mode_T);
		set_ia32_Immop_attr(new_op, const_op);
	}

	return new_op;
}

/**
 * Creates an ia32 Sub.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 Sub node
 */
static ir_node *gen_Sub(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node  *new_op = NULL;
	dbg_info *dbg    = env->dbg;
	ir_mode  *mode   = env->mode;
	ir_graph *irg    = env->irg;
	ir_node  *block  = env->block;
	ir_node  *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node  *nomem  = new_NoMem();
	ir_node  *expr_op, *imm_op;

	/* Check if immediate optimization is on and */
	/* if it's an operation with immediate.      */
	imm_op  = env->cg->opt.immops ? get_immediate_op(NULL, op2) : NULL;
	expr_op = get_expr_op(op1, op2);

	assert((expr_op || imm_op) && "invalid operands");

	if (mode_is_float(mode)) {
		return gen_binop(env, op1, op2, new_rd_ia32_fSub);
	}
	else {
		/* integer SUB */
		if (!expr_op) {
			/* No expr_op means, that we have two const - one symconst and */
			/* one tarval or another symconst - because this case is not   */
			/* covered by constant folding                                 */

			new_op = new_rd_ia32_Lea(dbg, irg, block, noreg, noreg, mode);
			add_ia32_am_offs(new_op, get_ia32_cnst(op1));
			sub_ia32_am_offs(new_op, get_ia32_cnst(op2));

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Source);
			set_ia32_op_type(new_op, ia32_AddrModeS);
			set_ia32_am_flavour(new_op, ia32_am_O);

			/* Lea doesn't need a Proj */
			return new_op;
		}
		else if (imm_op) {
			/* This is expr - const */
			new_op = gen_imm_Sub(env, expr_op, imm_op);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Dest);
		}
		else {
			/* This is a normal sub */
			new_op = new_rd_ia32_Sub(dbg, irg, block, noreg, noreg, op1, op2, nomem, mode_T);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Full);
		}
	}

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	set_ia32_res_mode(new_op, mode);

	return new_rd_Proj(dbg, irg, block, new_op, mode, 0);
}

static ir_node *get_proj_for_pn(const ir_node *irn, long pn) {
	const ir_edge_t *edge;
	ir_node   *proj;
	assert(get_irn_mode(irn) == mode_T && "need mode_T");

	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);

		if (get_Proj_proj(proj) == pn)
			return proj;
	}

	return NULL;
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
static ir_node *generate_DivMod(ia32_transform_env_t *env, ir_node *dividend, ir_node *divisor, ia32_op_flavour_t dm_flav) {
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
			mem  = get_Div_mem(irn);
			mode = get_irn_mode(get_proj_for_pn(irn, pn_Div_res));
			break;
		case flavour_Mod:
			mem  = get_Mod_mem(irn);
			mode = get_irn_mode(get_proj_for_pn(irn, pn_Mod_res));
			break;
		case flavour_DivMod:
			mem  = get_DivMod_mem(irn);
			mode = get_irn_mode(get_proj_for_pn(irn, pn_DivMod_res_div));
			break;
		default:
			assert(0);
	}

	if (mode_is_signed(mode)) {
		/* in signed mode, we need to sign extend the dividend */
		cltd     = new_rd_ia32_Cdq(dbg, irg, block, dividend, mode_T);
		dividend = new_rd_Proj(dbg, irg, block, cltd, mode_Is, pn_EAX);
		edx_node = new_rd_Proj(dbg, irg, block, cltd, mode_Is, pn_EDX);
	}
	else {
		edx_node = new_rd_ia32_Const(dbg, irg, block, mode_Iu);
		set_ia32_Const_type(edx_node, ia32_Const);
		set_ia32_Immop_tarval(edx_node, get_tarval_null(mode_Iu));
	}

	res = new_rd_ia32_DivMod(dbg, irg, block, dividend, divisor, edx_node, mem, mode_T);

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

		be_new_Keep(&ia32_reg_classes[CLASS_ia32_gp], irg, block, 1, in_keep);
	}

	SET_IA32_ORIG_NODE(res, get_old_node_name(env));

	set_ia32_res_mode(res, mode_Is);

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
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 fDiv node
 */
static ir_node *gen_Quot(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node *nomem = new_rd_NoMem(env->irg);
	ir_node *new_op;

	new_op = new_rd_ia32_fDiv(env->dbg, env->irg, env->block, noreg, noreg, op1, op2, nomem, env->mode);
	set_ia32_am_support(new_op, ia32_am_Source);

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	return new_op;
}



/**
 * Creates an ia32 Shl.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 Shl node
 */
static ir_node *gen_Shl(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return gen_shift_binop(env, op1, op2, new_rd_ia32_Shl);
}



/**
 * Creates an ia32 Shr.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 Shr node
 */
static ir_node *gen_Shr(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return gen_shift_binop(env, op1, op2, new_rd_ia32_Shr);
}



/**
 * Creates an ia32 Shrs.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 Shrs node
 */
static ir_node *gen_Shrs(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return gen_shift_binop(env, op1, op2, new_rd_ia32_Shrs);
}



/**
 * Creates an ia32 RotL.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 RotL node
 */
static ir_node *gen_RotL(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return gen_shift_binop(env, op1, op2, new_rd_ia32_RotL);
}



/**
 * Creates an ia32 RotR.
 * NOTE: There is no RotR with immediate because this would always be a RotL
 *       "imm-mode_size_bits" which can be pre-calculated.
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 RotR node
 */
static ir_node *gen_RotR(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return gen_shift_binop(env, op1, op2, new_rd_ia32_RotR);
}



/**
 * Creates an ia32 RotR or RotL (depending on the found pattern).
 *
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 RotL or RotR node
 */
static ir_node *gen_Rot(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	ir_node *rotate = NULL;

	/* Firm has only Rot (which is a RotL), so we are looking for a right (op2)
		 operand "-e+mode_size_bits" (it's an already modified "mode_size_bits-e",
		 that means we can create a RotR instead of an Add and a RotL */

	if (is_Proj(op2)) {
		ir_node *pred = get_Proj_pred(op2);

		if (is_ia32_Add(pred)) {
			ir_node *pred_pred = get_irn_n(pred, 2);
			tarval  *tv        = get_ia32_Immop_tarval(pred);
			long     bits      = get_mode_size_bits(env->mode);

			if (is_Proj(pred_pred)) {
				pred_pred = get_Proj_pred(pred_pred);
			}

			if (is_ia32_Minus(pred_pred) &&
				tarval_is_long(tv)       &&
				get_tarval_long(tv) == bits)
			{
				DB((env->mod, LEVEL_1, "RotL into RotR ... "));
				rotate = gen_RotR(env, op1, get_irn_n(pred_pred, 2));
			}

		}
	}

	if (!rotate) {
		rotate = gen_RotL(env, op1, op2);
	}

	return rotate;
}



/**
 * Transforms a Minus node.
 *
 * @param env   The transformation environment
 * @param op    The operator
 * @return The created ia32 Minus node
 */
static ir_node *gen_Minus(ia32_transform_env_t *env, ir_node *op) {
	const char *name;
	ir_node *new_op;
	ir_node *noreg_gp = ia32_new_NoReg_gp(env->cg);
	ir_node *noreg_fp = ia32_new_NoReg_fp(env->cg);
	ir_node *nomem    = new_rd_NoMem(env->irg);
	int      size;

	if (mode_is_float(env->mode)) {
		new_op = new_rd_ia32_fEor(env->dbg, env->irg, env->block, noreg_gp, noreg_gp, op, noreg_fp, nomem, mode_T);

		size   = get_mode_size_bits(env->mode);
		name   = gen_fp_known_const(env->mode, size == 32 ? ia32_SSIGN : ia32_DSIGN);

		set_ia32_sc(new_op, name);

		SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

		set_ia32_res_mode(new_op, env->mode);

		new_op = new_rd_Proj(env->dbg, env->irg, env->block, new_op, env->mode, 0);
	}
	else {
		new_op = gen_unop(env, op, new_rd_ia32_Minus);
	}

	return new_op;
}



/**
 * Transforms a Not node.
 *
 * @param env   The transformation environment
 * @param op    The operator
 * @return The created ia32 Not node
 */
static ir_node *gen_Not(ia32_transform_env_t *env, ir_node *op) {
	ir_node *new_op;

	if (mode_is_float(env->mode)) {
		assert(0);
	}
	else {
		new_op = gen_unop(env, op, new_rd_ia32_Not);
	}

	return new_op;
}



/**
 * Transforms an Abs node.
 *
 * @param env   The transformation environment
 * @param op    The operator
 * @return The created ia32 Abs node
 */
static ir_node *gen_Abs(ia32_transform_env_t *env, ir_node *op) {
	ir_node  *res, *p_eax, *p_edx;
	dbg_info *dbg      = env->dbg;
	ir_mode  *mode     = env->mode;
	ir_graph *irg      = env->irg;
	ir_node  *block    = env->block;
	ir_node  *noreg_gp = ia32_new_NoReg_gp(env->cg);
	ir_node  *noreg_fp = ia32_new_NoReg_fp(env->cg);
	ir_node  *nomem    = new_NoMem();
	int       size;
	const char *name;

	if (mode_is_float(mode)) {
		res = new_rd_ia32_fAnd(dbg,irg, block, noreg_gp, noreg_gp, op, noreg_fp, nomem, mode_T);

		size   = get_mode_size_bits(mode);
		name   = gen_fp_known_const(mode, size == 32 ? ia32_SABS : ia32_DABS);

		set_ia32_sc(res, name);

		SET_IA32_ORIG_NODE(res, get_old_node_name(env));

		set_ia32_res_mode(res, mode);

		res = new_rd_Proj(dbg, irg, block, res, mode, 0);
	}
	else {
		res   = new_rd_ia32_Cdq(dbg, irg, block, op, mode_T);
		SET_IA32_ORIG_NODE(res, get_old_node_name(env));
		set_ia32_res_mode(res, mode);

		p_eax = new_rd_Proj(dbg, irg, block, res, mode, pn_EAX);
		p_edx = new_rd_Proj(dbg, irg, block, res, mode, pn_EDX);

		res   = new_rd_ia32_Eor(dbg, irg, block, noreg_gp, noreg_gp, p_eax, p_edx, nomem, mode_T);
		SET_IA32_ORIG_NODE(res, get_old_node_name(env));
		set_ia32_res_mode(res, mode);

		res   = new_rd_Proj(dbg, irg, block, res, mode, 0);

		res   = new_rd_ia32_Sub(dbg, irg, block, noreg_gp, noreg_gp, res, p_edx, nomem, mode_T);
		SET_IA32_ORIG_NODE(res, get_old_node_name(env));
		set_ia32_res_mode(res, mode);

		res   = new_rd_Proj(dbg, irg, block, res, mode, 0);
	}

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
	ir_node *node  = env->irn;
	ir_node *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node *new_op;

	if (mode_is_float(env->mode)) {
		new_op = new_rd_ia32_fLoad(env->dbg, env->irg, env->block, get_Load_ptr(node), noreg, get_Load_mem(node), env->mode);
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, get_Load_ptr(node), noreg, get_Load_mem(node), env->mode);
	}

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, get_Load_mode(node));

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	return new_op;
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
static ir_node *gen_Store(ia32_transform_env_t *env) {
	ir_node *node  = env->irn;
	ir_node *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node *val   = get_Store_value(node);
	ir_node *ptr   = get_Store_ptr(node);
	ir_node *mem   = get_Store_mem(node);
	ir_mode *mode  = get_irn_mode(val);
	ir_node *sval  = val;
	ir_node *new_op;

	/* in case of storing a const (but not a symconst) -> make it an attribute */
	if (is_ia32_Const(val)) {
		sval = noreg;
	}

	if (mode_is_float(mode)) {
		new_op = new_rd_ia32_fStore(env->dbg, env->irg, env->block, ptr, noreg, sval, mem, mode_T);
	}
	else if (get_mode_size_bits(mode) == 8) {
		new_op = new_rd_ia32_Store8Bit(env->dbg, env->irg, env->block, ptr, noreg, sval, mem, mode_T);
	}
	else {
		new_op = new_rd_ia32_Store(env->dbg, env->irg, env->block, ptr, noreg, sval, mem, mode_T);
	}

	/* stored const is an attribute (saves a register) */
	if (is_ia32_Const(val)) {
		set_ia32_Immop_attr(new_op, val);
	}

	set_ia32_am_support(new_op, ia32_am_Dest);
	set_ia32_op_type(new_op, ia32_AddrModeD);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, get_irn_mode(val));

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	return new_op;
}



/**
 * Transforms a Cond -> Proj[b] -> Cmp into a CondJmp, CondJmp_i or TestJmp
 *
 * @param env   The transformation environment
 * @return The transformed node.
 */
static ir_node *gen_Cond(ia32_transform_env_t *env) {
	dbg_info *dbg      = env->dbg;
	ir_graph *irg      = env->irg;
	ir_node  *block    = env->block;
	ir_node  *node     = env->irn;
	ir_node	 *sel      = get_Cond_selector(node);
	ir_mode  *sel_mode = get_irn_mode(sel);
	ir_node  *res      = NULL;
	ir_node  *pred     = NULL;
	ir_node  *noreg    = ia32_new_NoReg_gp(env->cg);
	ir_node  *cmp_a, *cmp_b, *cnst, *expr;

	if (is_Proj(sel) && sel_mode == mode_b) {
		ir_node  *nomem = new_NoMem();

		pred  = get_Proj_pred(sel);

		/* get both compare operators */
		cmp_a = get_Cmp_left(pred);
		cmp_b = get_Cmp_right(pred);

		/* check if we can use a CondJmp with immediate */
		cnst = env->cg->opt.immops ? get_immediate_op(cmp_a, cmp_b) : NULL;
		expr = get_expr_op(cmp_a, cmp_b);

		if (cnst && expr) {
			if (mode_is_int(get_irn_mode(expr))) {
				if (classify_tarval(get_ia32_Immop_tarval(cnst)) == TV_CLASSIFY_NULL) {
					/* a Cmp A, 0 */
					ir_node *op1 = expr;
					ir_node *op2 = expr;
					ir_node *and = skip_Proj(expr);
					char *cnst = NULL;

					/* check, if expr is an only once used And operation */
					if (get_irn_n_edges(expr) == 1 && is_ia32_And(and)) {
						op1 = get_irn_n(and, 2);
						op2 = get_irn_n(and, 3);

						cnst = get_ia32_cnst(and);
					}
					res = new_rd_ia32_TestJmp(dbg, irg, block, op1, op2, mode_T);
					set_ia32_pncode(res, get_Proj_proj(sel));

					if (cnst) {
						copy_ia32_Immop_attr(res, and);
					}

					SET_IA32_ORIG_NODE(res, get_old_node_name(env));
					return res;
				}
			}
			res = new_rd_ia32_CondJmp(dbg, irg, block, noreg, noreg, expr, noreg, nomem, mode_T);
			set_ia32_Immop_attr(res, cnst);
		}
		else {
			res = new_rd_ia32_CondJmp(dbg, irg, block, noreg, noreg, cmp_a, cmp_b, nomem, mode_T);
		}

		set_ia32_pncode(res, get_Proj_proj(sel));
		set_ia32_am_support(res, ia32_am_Source);
	}
	else {
		res = new_rd_ia32_SwitchJmp(dbg, irg, block, sel, mode_T);
		set_ia32_pncode(res, get_Cond_defaultProj(node));
	}

	SET_IA32_ORIG_NODE(res, get_old_node_name(env));
	return res;
}



/**
 * Transforms a CopyB node.
 *
 * @param env   The transformation environment
 * @return The transformed node.
 */
static ir_node *gen_CopyB(ia32_transform_env_t *env) {
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
	int       rem;

	/* If we have to copy more than 16 bytes, we use REP MOVSx and */
	/* then we need the size explicitly in ECX.                    */
	if (size >= 16 * 4) {
		rem = size & 0x3; /* size % 4 */
		size >>= 2;

		res = new_rd_ia32_Const(dbg, irg, block, mode_Is);
		set_ia32_op_type(res, ia32_Const);
		set_ia32_Immop_tarval(res, new_tarval_from_long(size, mode_Is));

		res = new_rd_ia32_CopyB(dbg, irg, block, dst, src, res, mem, mode);
		set_ia32_Immop_tarval(res, new_tarval_from_long(rem, mode_Is));
	}
	else {
		res = new_rd_ia32_CopyB_i(dbg, irg, block, dst, src, mem, mode);
		set_ia32_Immop_tarval(res, new_tarval_from_long(size, mode_Is));
	}

	SET_IA32_ORIG_NODE(res, get_old_node_name(env));

	return res;
}



/**
 * Transforms a Mux node into CMov.
 *
 * @param env   The transformation environment
 * @return The transformed node.
 */
static ir_node *gen_Mux(ia32_transform_env_t *env) {
	ir_node *node   = env->irn;
	ir_node *new_op = new_rd_ia32_CMov(env->dbg, env->irg, env->block, \
		get_Mux_sel(node), get_Mux_false(node), get_Mux_true(node), env->mode);

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	return new_op;
}


/**
 * Following conversion rules apply:
 *
 *  INT -> INT
 * ============
 *  1) n bit -> m bit   n > m (downscale)
 *     a) target is signed:    movsx
 *     b) target is unsigned:  and with lower bits sets
 *  2) n bit -> m bit   n == m   (sign change)
 *     always ignored
 *  3) n bit -> m bit   n < m (upscale)
 *     a) source is signed:    movsx
 *     b) source is unsigned:  and with lower bits sets
 *
 *  INT -> FLOAT
 * ==============
 *  SSE(1/2) convert to float or double (cvtsi2ss/sd)
 *
 *  FLOAT -> INT
 * ==============
 *  SSE(1/2) convert from float or double to 32bit int (cvtss/sd2si)
 *  if target mode < 32bit: additional INT -> INT conversion (see above)
 *
 *  FLOAT -> FLOAT
 * ================
 *  SSE(1/2) convert from float or double to double or float (cvtss/sd2sd/ss)
 */

//static ir_node *gen_int_downscale_conv(ia32_transform_env_t *env, ir_node *op,
//									   ir_mode *src_mode, ir_mode *tgt_mode)
//{
//	int       n     = get_mode_size_bits(src_mode);
//	int       m     = get_mode_size_bits(tgt_mode);
//	dbg_info *dbg   = env->dbg;
//	ir_graph *irg   = env->irg;
//	ir_node  *block = env->block;
//	ir_node  *noreg = ia32_new_NoReg_gp(env->cg);
//	ir_node  *nomem = new_rd_NoMem(irg);
//	ir_node  *new_op, *proj;
//	assert(n > m && "downscale expected");
//	if (mode_is_signed(src_mode) && mode_is_signed(tgt_mode)) {
//		/* ASHL Sn, n - m */
//		new_op = new_rd_ia32_Shl(dbg, irg, block, noreg, noreg, op, noreg, nomem, mode_T);
//		proj   = new_rd_Proj(dbg, irg, block, new_op, src_mode, 0);
//		set_ia32_Immop_tarval(new_op, new_tarval_from_long(n - m, mode_Is));
//		set_ia32_am_support(new_op, ia32_am_Source);
//		SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));
//		/* ASHR Sn, n - m */
//		new_op = new_rd_ia32_Shrs(dbg, irg, block, noreg, noreg, proj, noreg, nomem, mode_T);
//		set_ia32_Immop_tarval(new_op, new_tarval_from_long(n - m, mode_Is));
//	}
//	else {
//		new_op = new_rd_ia32_And(dbg, irg, block, noreg, noreg, op, noreg, nomem, mode_T);
//		set_ia32_Immop_tarval(new_op, new_tarval_from_long((1 << m) - 1, mode_Is));
//	}
//	return new_op;
//}

/**
 * Transforms a Conv node.
 *
 * @param env   The transformation environment
 * @param op    The operator
 * @return The created ia32 Conv node
 */
static ir_node *gen_Conv(ia32_transform_env_t *env, ir_node *op) {
	dbg_info          *dbg      = env->dbg;
	ir_graph          *irg      = env->irg;
	ir_mode           *src_mode = get_irn_mode(op);
	ir_mode           *tgt_mode = env->mode;
	ir_node           *block    = env->block;
	ir_node           *new_op   = NULL;
	ir_node           *noreg    = ia32_new_NoReg_gp(env->cg);
	ir_node           *nomem    = new_rd_NoMem(irg);
	firm_dbg_module_t *mod      = env->mod;
	ir_node           *proj;

	if (src_mode == tgt_mode) {
		/* this can happen when changing mode_P to mode_Is */
		DB((mod, LEVEL_1, "killed Conv(mode, mode) ..."));
		edges_reroute(env->irn, op, irg);
	}
	else if (mode_is_float(src_mode)) {
		/* we convert from float ... */
		if (mode_is_float(tgt_mode)) {
			/* ... to float */
			DB((mod, LEVEL_1, "create Conv(float, float) ..."));
			new_op = new_rd_ia32_Conv_FP2FP(dbg, irg, block, noreg, noreg, op, nomem, mode_T);
		}
		else {
			/* ... to int */
			DB((mod, LEVEL_1, "create Conv(float, int) ..."));
			new_op = new_rd_ia32_Conv_FP2I(dbg, irg, block, noreg, noreg, op, nomem, mode_T);
			/* if target mode is not int: add an additional downscale convert */
			if (get_mode_size_bits(tgt_mode) < 32) {
				SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));
				set_ia32_res_mode(new_op, tgt_mode);
				set_ia32_am_support(new_op, ia32_am_Source);

				proj   = new_rd_Proj(dbg, irg, block, new_op, mode_Is, 0);
				new_op = new_rd_ia32_Conv_I2I(dbg, irg, block, noreg, noreg, proj, nomem, mode_T);
			}
		}
	}
	else {
		/* we convert from int ... */
		if (mode_is_float(tgt_mode)) {
			/* ... to float */
			DB((mod, LEVEL_1, "create Conv(int, float) ..."));
			new_op = new_rd_ia32_Conv_I2FP(dbg, irg, block, noreg, noreg, op, nomem, mode_T);
		}
		else {
			/* ... to int */
			if (get_mode_size_bits(src_mode) == get_mode_size_bits(tgt_mode)) {
				DB((mod, LEVEL_1, "omitting equal size Conv(%+F, %+F) ...", src_mode, tgt_mode));
				edges_reroute(env->irn, op, irg);
			}
			else {
				DB((mod, LEVEL_1, "create Conv(int, int) ...", src_mode, tgt_mode));
				new_op = new_rd_ia32_Conv_I2I(dbg, irg, block, noreg, noreg, op, nomem, mode_T);
			}
		}
	}

	if (new_op) {
		SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));
		set_ia32_res_mode(new_op, tgt_mode);

		set_ia32_am_support(new_op, ia32_am_Source);

		new_op = new_rd_Proj(dbg, irg, block, new_op, tgt_mode, 0);
	}

	return new_op;
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

static ir_node *gen_StackParam(ia32_transform_env_t *env) {
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *mem    = new_rd_NoMem(env->irg);
	ir_node *ptr    = get_irn_n(node, 0);
	entity  *ent    = be_get_frame_entity(node);
	ir_mode *mode   = env->mode;

	if (mode_is_float(mode)) {
		new_op = new_rd_ia32_fLoad(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
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

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	return new_rd_Proj(env->dbg, env->irg, env->block, new_op, mode, 0);
}

/**
 * Transforms a FrameAddr into an ia32 Add.
 */
static ir_node *gen_FrameAddr(ia32_transform_env_t *env) {
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *op     = get_irn_n(node, 0);
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *nomem  = new_rd_NoMem(env->irg);

	new_op = new_rd_ia32_Add(env->dbg, env->irg, env->block, noreg, noreg, op, noreg, nomem, mode_T);
	set_ia32_frame_ent(new_op, be_get_frame_entity(node));
	set_ia32_am_support(new_op, ia32_am_Full);
	set_ia32_use_frame(new_op);

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	return new_rd_Proj(env->dbg, env->irg, env->block, new_op, env->mode, 0);
}

/**
 * Transforms a FrameLoad into an ia32 Load.
 */
static ir_node *gen_FrameLoad(ia32_transform_env_t *env) {
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *mem    = get_irn_n(node, 0);
	ir_node *ptr    = get_irn_n(node, 1);
	entity  *ent    = be_get_frame_entity(node);
	ir_mode *mode   = get_type_mode(get_entity_type(ent));

	if (mode_is_float(mode)) {
		new_op = new_rd_ia32_fLoad(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
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

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	return new_op;
}


/**
 * Transforms a FrameStore into an ia32 Store.
 */
static ir_node *gen_FrameStore(ia32_transform_env_t *env) {
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *mem    = get_irn_n(node, 0);
	ir_node *ptr    = get_irn_n(node, 1);
	ir_node *val    = get_irn_n(node, 2);
	entity  *ent    = be_get_frame_entity(node);
	ir_mode *mode   = get_irn_mode(val);

	if (mode_is_float(mode)) {
		new_op = new_rd_ia32_fStore(env->dbg, env->irg, env->block, ptr, noreg, val, mem, mode_T);
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

	SET_IA32_ORIG_NODE(new_op, get_old_node_name(env));

	return new_op;
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
 * Transforms a Sub or fSub into Neg--Add iff OUT_REG == SRC2_REG.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
void ia32_transform_sub_to_neg_add(ir_node *irn, ia32_code_gen_t *cg) {
	ia32_transform_env_t tenv;
	ir_node *in1, *in2, *noreg, *nomem, *res;
	const arch_register_t *in1_reg, *in2_reg, *out_reg, **slots;

	/* Return if AM node or not a Sub or fSub */
	if (get_ia32_op_type(irn) != ia32_Normal || !(is_ia32_Sub(irn) || is_ia32_fSub(irn)))
		return;

	noreg   = ia32_new_NoReg_gp(cg);
	nomem   = new_rd_NoMem(cg->irg);
	in1     = get_irn_n(irn, 2);
	in2     = get_irn_n(irn, 3);
	in1_reg = arch_get_irn_register(cg->arch_env, in1);
	in2_reg = arch_get_irn_register(cg->arch_env, in2);
	out_reg = get_ia32_out_reg(irn, 0);

	tenv.block    = get_nodes_block(irn);
	tenv.dbg      = get_irn_dbg_info(irn);
	tenv.irg      = cg->irg;
	tenv.irn      = irn;
	tenv.mod      = cg->mod;
	tenv.mode     = get_ia32_res_mode(irn);
	tenv.cg       = cg;

	/* in case of sub and OUT == SRC2 we can transform the sequence into neg src2 -- add */
	if (REGS_ARE_EQUAL(out_reg, in2_reg)) {
		/* generate the neg src2 */
		res = gen_Minus(&tenv, in2);
		arch_set_irn_register(cg->arch_env, res, in2_reg);

		/* add to schedule */
		sched_add_before(irn, res);

		/* generate the add */
		if (mode_is_float(tenv.mode)) {
			res = new_rd_ia32_fAdd(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, res, in1, nomem, mode_T);
			set_ia32_am_support(res, ia32_am_Source);
		}
		else {
			res = new_rd_ia32_Add(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, res, in1, nomem, mode_T);
			set_ia32_am_support(res, ia32_am_Full);
		}

		SET_IA32_ORIG_NODE(res, get_old_node_name(&tenv));
		/* copy register */
		slots    = get_ia32_slots(res);
		slots[0] = in2_reg;

		/* add to schedule */
		sched_add_before(irn, res);

		/* remove the old sub */
		sched_remove(irn);

		/* exchange the add and the sub */
		exchange(irn, res);
	}
}

/**
 * Transforms a LEA into an Add if possible
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
void ia32_transform_lea_to_add(ir_node *irn, ia32_code_gen_t *cg) {
	ia32_am_flavour_t am_flav;
	int               imm = 0;
	ir_node          *res = NULL;
	ir_node          *nomem, *noreg, *base, *index, *op1, *op2;
	char             *offs;
	ia32_transform_env_t tenv;
	const arch_register_t *out_reg, *base_reg, *index_reg;

	/* must be a LEA */
	if (! is_ia32_Lea(irn))
		return;

	am_flav = get_ia32_am_flavour(irn);

	/* only some LEAs can be transformed to an Add */
	if (am_flav != ia32_am_B && am_flav != ia32_am_OB && am_flav != ia32_am_OI && am_flav != ia32_am_BI)
		return;

	noreg = ia32_new_NoReg_gp(cg);
	nomem = new_rd_NoMem(cg->irg);
	op1   = noreg;
	op2   = noreg;
	base  = get_irn_n(irn, 0);
	index = get_irn_n(irn,1);

	offs  = get_ia32_am_offs(irn);

	/* offset has a explicit sign -> we need to skip + */
	if (offs && offs[0] == '+')
		offs++;

	out_reg   = arch_get_irn_register(cg->arch_env, irn);
	base_reg  = arch_get_irn_register(cg->arch_env, base);
	index_reg = arch_get_irn_register(cg->arch_env, index);

	tenv.block = get_nodes_block(irn);
	tenv.dbg   = get_irn_dbg_info(irn);
	tenv.irg   = cg->irg;
	tenv.irn   = irn;
	tenv.mod   = cg->mod;
	tenv.mode  = get_irn_mode(irn);
	tenv.cg    = cg;

	switch(get_ia32_am_flavour(irn)) {
		case ia32_am_B:
			/* out register must be same as base register */
			if (! REGS_ARE_EQUAL(out_reg, base_reg))
				return;

			op1 = base;
			break;
		case ia32_am_OB:
			/* out register must be same as base register */
			if (! REGS_ARE_EQUAL(out_reg, base_reg))
				return;

			op1 = base;
			imm = 1;
			break;
		case ia32_am_OI:
			/* out register must be same as index register */
			if (! REGS_ARE_EQUAL(out_reg, index_reg))
				return;

			op1 = index;
			imm = 1;
			break;
		case ia32_am_BI:
			/* out register must be same as one in register */
			if (REGS_ARE_EQUAL(out_reg, base_reg)) {
				op1 = base;
				op2 = index;
			}
			else if (REGS_ARE_EQUAL(out_reg, index_reg)) {
				op1 = index;
				op2 = base;
			}
			else {
				/* in registers a different from out -> no Add possible */
				return;
			}
		default:
			break;
	}

	res = new_rd_ia32_Add(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, op1, op2, nomem, mode_T);
	arch_set_irn_register(cg->arch_env, res, out_reg);
	set_ia32_op_type(res, ia32_Normal);

	if (imm)
		set_ia32_cnst(res, offs);

	SET_IA32_ORIG_NODE(res, get_old_node_name(&tenv));

	/* add Add to schedule */
	sched_add_before(irn, res);

	res = new_rd_Proj(tenv.dbg, tenv.irg, tenv.block, res, tenv.mode, 0);

	/* add result Proj to schedule */
	sched_add_before(irn, res);

	/* remove the old LEA */
	sched_remove(irn);

	/* exchange the Add and the LEA */
	exchange(irn, res);
}

/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void ia32_transform_node(ir_node *node, void *env) {
	ia32_code_gen_t *cgenv = (ia32_code_gen_t *)env;
	opcode  code;
	ir_node *asm_node      = NULL;
	ia32_transform_env_t  tenv;

	if (is_Block(node))
		return;

	tenv.block    = get_nodes_block(node);
	tenv.dbg      = get_irn_dbg_info(node);
	tenv.irg      = current_ir_graph;
	tenv.irn      = node;
	tenv.mod      = cgenv->mod;
	tenv.mode     = get_irn_mode(node);
	tenv.cg       = cgenv;

#define UNOP(a)  case iro_##a: asm_node = gen_##a(&tenv, get_##a##_op(node)); break
#define BINOP(a) case iro_##a: asm_node = gen_##a(&tenv, get_##a##_left(node), get_##a##_right(node)); break
#define GEN(a)   case iro_##a: asm_node = gen_##a(&tenv); break
#define IGN(a)   case iro_##a: break
#define BAD(a)   case iro_##a: goto bad
#define OTHER_BIN(a)                                                       \
	if (get_irn_op(node) == get_op_##a()) {                                \
		asm_node = gen_##a(&tenv, get_irn_n(node, 0), get_irn_n(node, 1)); \
		break;                                                             \
	}
#define BE_GEN(a)                  \
	if (be_is_##a(node)) {         \
		asm_node = gen_##a(&tenv); \
		break;                     \
	}

	DBG((tenv.mod, LEVEL_1, "check %+F ... ", node));

	code = get_irn_opcode(node);
	switch (code) {
		BINOP(Add);
		BINOP(Sub);
		BINOP(Mul);
		BINOP(And);
		BINOP(Or);
		BINOP(Eor);

		BINOP(Shl);
		BINOP(Shr);
		BINOP(Shrs);
		BINOP(Rot);

		BINOP(Quot);

		BINOP(Div);
		BINOP(Mod);
		BINOP(DivMod);

		UNOP(Minus);
		UNOP(Conv);
		UNOP(Abs);
		UNOP(Not);

		GEN(Load);
		GEN(Store);
		GEN(Cond);

		GEN(CopyB);
		GEN(Mux);

		IGN(Call);
		IGN(Alloc);

		IGN(Proj);
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
			OTHER_BIN(Max);
			OTHER_BIN(Min);
			OTHER_BIN(Mulh);

			BE_GEN(FrameAddr);
			BE_GEN(FrameLoad);
			BE_GEN(FrameStore);
			BE_GEN(StackParam);
			break;
bad:
		fprintf(stderr, "Not implemented: %s\n", get_irn_opname(node));
		assert(0);
	}

	/* exchange nodes if a new one was generated */
	if (asm_node) {
		exchange(node, asm_node);
		DB((tenv.mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DB((tenv.mod, LEVEL_1, "ignored\n"));
	}

#undef UNOP
#undef BINOP
#undef GEN
#undef IGN
#undef BAD
#undef OTHER_BIN
#undef BE_GEN
}
