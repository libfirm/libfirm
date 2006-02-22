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
	ia32_SSIGN, ia32_DSIGN, ia32_SABS, ia32_DABS
} ia32_known_const_t;

/* TEMPORARY WORKAROUND */
ir_node *be_new_NoReg(ir_graph *irg) {
	return new_NoMem();
}

/****************************************************************************************************
 *                  _        _                        __                           _   _
 *                 | |      | |                      / _|                         | | (_)
 *  _ __   ___   __| | ___  | |_ _ __ __ _ _ __  ___| |_ ___  _ __ _ __ ___   __ _| |_ _  ___  _ __
 * | '_ \ / _ \ / _` |/ _ \ | __| '__/ _` | '_ \/ __|  _/ _ \| '__| '_ ` _ \ / _` | __| |/ _ \| '_ \
 * | | | | (_) | (_| |  __/ | |_| | | (_| | | | \__ \ || (_) | |  | | | | | | (_| | |_| | (_) | | | |
 * |_| |_|\___/ \__,_|\___|  \__|_|  \__,_|_| |_|___/_| \___/|_|  |_| |_| |_|\__,_|\__|_|\___/|_| |_|
 *
 ****************************************************************************************************/

struct tv_ent {
	entity *ent;
	tarval *tv;
};

/* Compares two (entity, tarval) combinations */
static int cmp_tv_ent(const void *a, const void *b, size_t len) {
	const struct tv_ent *e1 = a;
	const struct tv_ent *e2 = b;

	return !(e1->tv == e2->tv);
}

/* Generates an entity for a known FP const (used for FP Neg + Abs) */
static char *gen_fp_known_const(ir_mode *mode, ia32_known_const_t kct) {
	static set    *const_set = NULL;
	struct tv_ent  key;
	struct tv_ent *entry;
	char          *tp_name;
	char          *ent_name;
	char          *cnst_str;
	ir_type       *tp;
	ir_node       *cnst;
	ir_graph      *rem;
	entity        *ent;

	if (! const_set) {
		const_set = new_set(cmp_tv_ent, 10);
	}

	switch (kct) {
		case ia32_SSIGN:
			tp_name  = TP_SFP_SIGN;
			ent_name = ENT_SFP_SIGN;
			cnst_str = SFP_SIGN;
			break;
		case ia32_DSIGN:
			tp_name  = TP_DFP_SIGN;
			ent_name = ENT_DFP_SIGN;
			cnst_str = DFP_SIGN;
			break;
		case ia32_SABS:
			tp_name  = TP_SFP_ABS;
			ent_name = ENT_SFP_ABS;
			cnst_str = SFP_ABS;
			break;
		case ia32_DABS:
			tp_name  = TP_DFP_ABS;
			ent_name = ENT_DFP_ABS;
			cnst_str = DFP_ABS;
			break;
	}


	key.tv  = new_tarval_from_str(cnst_str, strlen(cnst_str), mode);
	key.ent = NULL;

	entry = set_insert(const_set, &key, sizeof(key), HASH_PTR(key.tv));

	if (! entry->ent) {
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
		cnst = new_Const(mode, key.tv);
		current_ir_graph = rem;

		set_atomic_ent_value(ent, cnst);

		/* set the entry for hashmap */
		entry->ent = ent;
	}

	return ent_name;
}


#undef is_cnst
#define is_cnst(op) (is_ia32_Const(op) || is_ia32_fConst(op))

/* determine if one operator is an Imm */
static ir_node *get_immediate_op(ir_node *op1, ir_node *op2) {
	if (op1)
		return is_cnst(op1) ? op1 : (is_cnst(op2) ? op2 : NULL);
	else return is_cnst(op2) ? op2 : NULL;
}

/* determine if one operator is not an Imm */
static ir_node *get_expr_op(ir_node *op1, ir_node *op2) {
	return !is_cnst(op1) ? op1 : (!is_cnst(op2) ? op2 : NULL);
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
	ir_node           *new_op = NULL;
	ir_mode           *mode   = env->mode;
	dbg_info          *dbg    = env->dbg;
	ir_graph          *irg    = env->irg;
	ir_node           *block  = env->block;
	firm_dbg_module_t *mod    = env->mod;
	ir_node           *noreg  = be_new_NoReg(irg);
	ir_node           *nomem  = new_NoMem();
	ir_node           *expr_op, *imm_op;


	/* check if it's an operation with immediate */
	if (is_op_commutative(get_irn_op(env->irn))) {
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

			new_op = func(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem, mode_T);
			set_ia32_Immop_attr(new_op, imm_op);
			set_ia32_am_support(new_op, ia32_am_None);
		}
		else {
			new_op = func(dbg, irg, block, noreg, noreg, op1, op2, nomem, mode_T);
			set_ia32_am_support(new_op, ia32_am_Source);
		}
	}
	else {
		/* integer operations */
		if (imm_op) {
			/* This is expr + const */
			new_op = func(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem, mode_T);
			set_ia32_Immop_attr(new_op, imm_op);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Dest);
		}
		else {
			/* This is a normal operation */
			new_op = func(dbg, irg, block, noreg, noreg, op1, op2, nomem, mode_T);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Full);
		}
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
	ir_node           *noreg  = be_new_NoReg(irg);
	ir_node           *nomem  = new_NoMem();
	ir_node           *expr_op, *imm_op;
	tarval            *tv;

	assert(! mode_is_float(mode) && "Shift/Rotate with float not supported");

	imm_op  = get_immediate_op(NULL, op2);
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

		new_op = func(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem, mode_T);
		set_ia32_Immop_attr(new_op, imm_op);
	}
	else {
		/* This is a normal shift/rot */
		new_op = func(dbg, irg, block, noreg, noreg, op1, op2, nomem, mode_T);
	}

	/* set AM support */
	set_ia32_am_support(new_op, ia32_am_Dest);

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
	ir_graph          *irg    = env->irg;
	ir_node           *block  = env->block;
	ir_node           *noreg  = be_new_NoReg(irg);
	ir_node           *nomem  = new_NoMem();

	new_op = func(dbg, irg, block, noreg, noreg, op, nomem, mode_T);

	if (mode_is_float(mode)) {
		/* floating point operations don't support implicit store */
		set_ia32_am_support(new_op, ia32_am_None);
	}
	else {
		set_ia32_am_support(new_op, ia32_am_Dest);
	}

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
	ir_mode                *mode       = env->mode;
	ir_graph               *irg        = env->irg;
	ir_node                *block      = env->block;
	ir_node                *noreg      = be_new_NoReg(irg);
	ir_node                *nomem      = new_NoMem();
	int                     normal_add = 1;
	tarval_classification_t class_tv, class_negtv;

	/* const_op: tarval or SymConst? */
	if (tv) {
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
	ir_node  *noreg  = be_new_NoReg(irg);
	ir_node  *nomem  = new_NoMem();
	ir_node  *expr_op, *imm_op;

	imm_op  = get_immediate_op(op1, op2);
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

			new_op = new_rd_ia32_Lea(dbg, irg, block, noreg, noreg, mode_T);
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
ir_node *gen_Mul(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
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

	assert(mode_is_float(env->mode) && "Mulh with float not supported");
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
	ir_mode                *mode       = env->mode;
	ir_graph               *irg        = env->irg;
	ir_node                *block      = env->block;
	ir_node                *noreg      = be_new_NoReg(irg);
	ir_node                *nomem      = new_NoMem();
	int                     normal_sub = 1;
	tarval_classification_t class_tv, class_negtv;

	/* const_op: tarval or SymConst? */
	if (tv) {
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
	ir_node  *noreg  = be_new_NoReg(irg);
	ir_node  *nomem  = new_NoMem();
	ir_node  *expr_op, *imm_op;

	imm_op  = get_immediate_op(NULL, op2);
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

			new_op = new_rd_ia32_Lea(dbg, irg, block, noreg, noreg, mode_T);
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

	return new_rd_Proj(dbg, irg, block, new_op, mode, 0);
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
		cltd     = new_rd_ia32_Cdq(dbg, irg, block, dividend, mode_T);
		dividend = new_rd_Proj(dbg, irg, block, cltd, mode_Is, pn_EAX);
		edx_node = new_rd_Proj(dbg, irg, block, cltd, mode_Is, pn_EDX);
	}
	else {
		edx_node = new_rd_ia32_Const(dbg, irg, block, mode_Iu);
		set_ia32_Const_type(edx_node, ia32_Const);
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

		be_new_Keep(&ia32_reg_classes[CLASS_ia32_gp], irg, block, 1, in_keep);
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
 * @param env   The transformation environment
 * @param op1   The first operator
 * @param op2   The second operator
 * @return The created ia32 fDiv node
 */
static ir_node *gen_Quot(ia32_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return gen_binop(env, op1, op2, new_rd_ia32_fDiv);
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
 * Transforms a Conv node.
 *
 * @param env   The transformation environment
 * @param op    The operator
 * @return The created ia32 Conv node
 */
static ir_node *gen_Conv(ia32_transform_env_t *env, ir_node *op) {
	return new_rd_ia32_Conv(env->dbg, env->irg, env->block, op, env->mode);
}



/**
 * Transforms a Minus node.
 *
 * @param env   The transformation environment
 * @param op    The operator
 * @return The created ia32 Minus node
 */
static ir_node *gen_Minus(ia32_transform_env_t *env, ir_node *op) {
	char    *name;
	ir_node *new_op;
	ir_node *noreg = be_new_NoReg(env->irg);
	ir_node *nomem = new_rd_NoMem(env->irg);
	int      size;

	if (mode_is_float(env->mode)) {
		new_op = new_rd_ia32_fEor(env->dbg, env->irg, env->block, noreg, noreg, op, noreg, nomem, mode_T);

		size   = get_mode_size_bits(env->mode);
		name   = gen_fp_known_const(env->mode, size == 32 ? ia32_SSIGN : ia32_DSIGN);

		set_ia32_sc(new_op, name);

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
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;
	ir_node  *noreg = be_new_NoReg(irg);
	ir_node  *nomem = new_NoMem();
	int       size;
	char     *name;

	if (mode_is_float(mode)) {
		res = new_rd_ia32_fAnd(dbg,irg, block, noreg, noreg, op, noreg, nomem, mode_T);

		size   = get_mode_size_bits(mode);
		name   = gen_fp_known_const(mode, size == 32 ? ia32_SABS : ia32_DABS);

		set_ia32_sc(res, name);

		res = new_rd_Proj(dbg, irg, block, res, mode, 0);
	}
	else {
		res   = new_rd_ia32_Cdq(dbg, irg, block, op, mode_T);
		p_eax = new_rd_Proj(dbg, irg, block, res, mode, pn_EAX);
		p_edx = new_rd_Proj(dbg, irg, block, res, mode, pn_EDX);
		res   = new_rd_ia32_Eor(dbg, irg, block, noreg, noreg, p_eax, p_edx, nomem, mode_T);
		res   = new_rd_Proj(dbg, irg, block, res, mode, 0);
		res   = new_rd_ia32_Sub(dbg, irg, block, noreg, noreg, res, p_edx, nomem, mode_T);
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
	ir_node *noreg = be_new_NoReg(env->irg);

	if (mode_is_float(env->mode)) {
		return new_rd_ia32_fLoad(env->dbg, env->irg, env->block, get_Load_ptr(node), noreg, get_Load_mem(node), env->mode);
	}
	return new_rd_ia32_Load(env->dbg, env->irg, env->block, get_Load_ptr(node), noreg, get_Load_mem(node), env->mode);
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
	ir_node *node  = env->irn;
	ir_node *noreg = be_new_NoReg(env->irg);

	if (mode_is_float(env->mode)) {
		return new_rd_ia32_fStore(env->dbg, env->irg, env->block, get_Store_ptr(node), noreg, get_Store_value(node), get_Store_mem(node), env->mode);
	}
	return new_rd_ia32_Store(env->dbg, env->irg, env->block, get_Store_ptr(node), noreg, get_Store_value(node), get_Store_mem(node), env->mode);
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
#if 0
	const ia32_register_req_t **in_req;
	ir_node          **in;
	ir_node           *new_call, *sync;
	int                i, j, n_new_call_in, ignore = 0;
	ia32_attr_t       *attr;
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
			attr->out_req[0] = &ia32_default_req_ia32_fp_xmm0;
		else
			attr->out_req[0] = &ia32_default_req_ia32_gp_eax;
	}
	else if (n_res == 2) {
		attr->out_req[0] = &ia32_default_req_ia32_gp_eax;
		attr->out_req[1] = &ia32_default_req_ia32_gp_edx;
	}

	/* stack parameter has no OUT register */
	attr->in_req[n_new_call_in - 1] = &ia32_default_req_none;

	return new_call;
#endif
}



/**
 * Transforms a Cond -> Proj[b] -> Cmp into a CondJmp or CondJmp_i
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
	ir_node  *noreg    = be_new_NoReg(irg);
	ir_node  *nomem    = new_NoMem();
	ir_node  *cmp_a, *cmp_b, *cnst, *expr;

	if (is_Proj(sel) && sel_mode == mode_b) {
		pred  = get_Proj_pred(sel);

		/* get both compare operators */
		cmp_a = get_Cmp_left(pred);
		cmp_b = get_Cmp_right(pred);

		/* check if we can use a CondJmp with immediate */
		cnst = get_immediate_op(cmp_a, cmp_b);
		expr = get_expr_op(cmp_a, cmp_b);

		if (cnst && expr) {
			res = new_rd_ia32_CondJmp(dbg, irg, block, noreg, noreg, expr, noreg, nomem, mode_T);
			set_ia32_Immop_attr(res, cnst);
		}
		else {
			res = new_rd_ia32_CondJmp(dbg, irg, block, noreg, noreg, cmp_a, cmp_b, nomem, mode_T);
		}

		set_ia32_pncode(res, get_Proj_proj(sel));
	}
	else {
		res = new_rd_ia32_SwitchJmp(dbg, irg, block, noreg, noreg, sel, nomem, mode_T);
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
#if 0
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
#endif
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

#define UNOP(a)  case iro_##a: asm_node = gen_##a(&tenv, get_##a##_op(node)); break
#define BINOP(a) case iro_##a: asm_node = gen_##a(&tenv, get_##a##_left(node), get_##a##_right(node)); break
#define GEN(a)   case iro_##a: asm_node = gen_##a(&tenv); break
#define IGN(a)   case iro_##a: break
#define BAD(a)   case iro_##a: goto bad

	DBG((tenv.mod, LEVEL_1, "check %+F ... ", node));

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

		IGN(Proj);
		IGN(Call);
		IGN(Alloc);

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
			if (get_irn_op(node) == get_op_Max()) {
				asm_node = gen_Max(&tenv, get_irn_n(node, 0), get_irn_n(node, 1));
			}
			else if (get_irn_op(node) == get_op_Min()) {
				asm_node = gen_Min(&tenv, get_irn_n(node, 0), get_irn_n(node, 1));
			}
			else if (get_irn_op(node) == get_op_Mulh()) {
				asm_node = gen_Mulh(&tenv, get_irn_n(node, 0), get_irn_n(node, 1));
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
