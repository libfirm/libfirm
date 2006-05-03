/**
 * This file implements the IR transformation from firm into
 * ia32-Firm.
 *
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

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
#include "irprintf.h"
#include "debug.h"

#include "../benode_t.h"
#include "../besched.h"
#include "../beabi.h"

#include "bearch_ia32_t.h"

#include "ia32_nodes_attr.h"
#include "../arch/archop.h"     /* we need this for Min and Max nodes */
#include "ia32_transform.h"
#include "ia32_new_nodes.h"
#include "ia32_map_regs.h"
#include "ia32_dbg_stat.h"

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
									  ir_node *op1, ir_node *op2, ir_node *mem);

typedef ir_node *construct_unop_func(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *base, ir_node *index, \
									 ir_node *op, ir_node *mem);

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

/**
 * Returns 1 if irn is a Const representing 0, 0 otherwise
 */
static INLINE int is_ia32_Const_0(ir_node *irn) {
	return is_ia32_Const(irn) ? classify_tarval(get_ia32_Immop_tarval(irn)) == TV_CLASSIFY_NULL : 0;
}

/**
 * Returns 1 if irn is a Const representing 1, 0 otherwise
 */
static INLINE int is_ia32_Const_1(ir_node *irn) {
	return is_ia32_Const(irn) ? classify_tarval(get_ia32_Immop_tarval(irn)) == TV_CLASSIFY_ONE : 0;
}

/**
 * Returns the Proj representing the UNKNOWN register for given mode.
 */
static ir_node *be_get_unknown_for_mode(ia32_code_gen_t *cg, ir_mode *mode) {
	be_abi_irg_t          *babi       = cg->birg->abi;
	const arch_register_t *unknwn_reg = NULL;

	if (mode_is_float(mode)) {
		unknwn_reg = USE_SSE2(cg) ? &ia32_xmm_regs[REG_XMM_UKNWN] : &ia32_vfp_regs[REG_VFP_UKNWN];
	}
	else {
		unknwn_reg = &ia32_gp_regs[REG_GP_UKNWN];
	}

	return be_abi_get_callee_save_irn(babi, unknwn_reg);
}

/**
 * Gets the Proj with number pn from irn.
 */
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

/* Generates an entity for a known FP const (used for FP Neg + Abs) */
static ident *gen_fp_known_const(ir_mode *mode, ia32_known_const_t kct) {
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

	return get_entity_ident(ent_cache[kct]);
}

#ifndef NDEBUG
/**
 * Prints the old node name on cg obst and returns a pointer to it.
 */
const char *ia32_get_old_node_name(ia32_code_gen_t *cg, ir_node *irn) {
	ia32_isa_t *isa = (ia32_isa_t *)cg->arch_env->isa;

	lc_eoprintf(firm_get_arg_env(), isa->name_obst, "%+F", irn);
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
	ir_node           *noreg_gp = ia32_new_NoReg_gp(env->cg);
	ir_node           *noreg_fp = ia32_new_NoReg_fp(env->cg);
	ir_node           *nomem    = new_NoMem();
	ir_node           *expr_op, *imm_op;
	DEBUG_ONLY(firm_dbg_module_t *mod = env->mod;)

	/* Check if immediate optimization is on and */
	/* if it's an operation with immediate.      */
	if (! (env->cg->opt & IA32_OPT_IMMOPS)) {
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
			new_op = func(dbg, irg, block, noreg_gp, noreg_gp, expr_op, noreg_fp, nomem);
			set_ia32_Immop_attr(new_op, imm_op);
			set_ia32_am_support(new_op, ia32_am_None);
		}
		else {
			DB((mod, LEVEL_1, "FP binop ..."));
			new_op = func(dbg, irg, block, noreg_gp, noreg_gp, op1, op2, nomem);
			set_ia32_am_support(new_op, ia32_am_Source);
		}
		set_ia32_ls_mode(new_op, mode);
	}
	else {
		/* integer operations */
		if (imm_op) {
			/* This is expr + const */
			DB((mod, LEVEL_1, "INT with immediate ..."));
			new_op = func(dbg, irg, block, noreg_gp, noreg_gp, expr_op, noreg_gp, nomem);
			set_ia32_Immop_attr(new_op, imm_op);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Dest);
		}
		else {
			DB((mod, LEVEL_1, "INT binop ..."));
			/* This is a normal operation */
			new_op = func(dbg, irg, block, noreg_gp, noreg_gp, op1, op2, nomem);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Full);
		}
	}

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

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
	ir_node           *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node           *nomem  = new_NoMem();
	ir_node           *expr_op, *imm_op;
	tarval            *tv;
	DEBUG_ONLY(firm_dbg_module_t *mod = env->mod;)

	assert(! mode_is_float(mode) && "Shift/Rotate with float not supported");

	/* Check if immediate optimization is on and */
	/* if it's an operation with immediate.      */
	imm_op  = (env->cg->opt & IA32_OPT_IMMOPS) ? get_immediate_op(NULL, op2) : NULL;
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

		new_op = func(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem);
		set_ia32_Immop_attr(new_op, imm_op);
	}
	else {
		/* This is a normal shift/rot */
		DB((mod, LEVEL_1, "Shift/Rot binop ..."));
		new_op = func(dbg, irg, block, noreg, noreg, op1, op2, nomem);
	}

	/* set AM support */
	set_ia32_am_support(new_op, ia32_am_Dest);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	set_ia32_res_mode(new_op, mode);
	set_ia32_emit_cl(new_op);

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
	ir_node           *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node           *nomem  = new_NoMem();
	DEBUG_ONLY(firm_dbg_module_t *mod = env->mod;)

	new_op = func(dbg, irg, block, noreg, noreg, op, nomem);

	if (mode_is_float(mode)) {
		DB((mod, LEVEL_1, "FP unop ..."));
		/* floating point operations don't support implicit store */
		set_ia32_am_support(new_op, ia32_am_None);
	}
	else {
		DB((mod, LEVEL_1, "INT unop ..."));
		set_ia32_am_support(new_op, ia32_am_Dest);
	}

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

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
	dbg_info               *dbg        = env->dbg;
	ir_graph               *irg        = env->irg;
	ir_node                *block      = env->block;
	ir_node                *noreg      = ia32_new_NoReg_gp(env->cg);
	ir_node                *nomem      = new_NoMem();
	int                     normal_add = 1;
	tarval_classification_t class_tv, class_negtv;
	DEBUG_ONLY(firm_dbg_module_t *mod = env->mod;)

	/* try to optimize to inc/dec  */
	if ((env->cg->opt & IA32_OPT_INCDEC) && (get_ia32_op_type(const_op) == ia32_Const)) {
		/* optimize tarvals */
		class_tv    = classify_tarval(tv);
		class_negtv = classify_tarval(tarval_neg(tv));

		if (class_tv == TV_CLASSIFY_ONE) { /* + 1 == INC */
			DB((env->mod, LEVEL_2, "Add(1) to Inc ... "));
			new_op     = new_rd_ia32_Inc(dbg, irg, block, noreg, noreg, expr_op, nomem);
			normal_add = 0;
		}
		else if (class_tv == TV_CLASSIFY_ALL_ONE || class_negtv == TV_CLASSIFY_ONE) { /* + (-1) == DEC */
			DB((mod, LEVEL_2, "Add(-1) to Dec ... "));
			new_op     = new_rd_ia32_Dec(dbg, irg, block, noreg, noreg, expr_op, nomem);
			normal_add = 0;
		}
	}

	if (normal_add) {
		new_op = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem);
		set_ia32_Immop_attr(new_op, const_op);
		set_ia32_commutative(new_op);
	}

	return new_op;
}

/**
 * Creates an ia32 Add.
 *
 * @param env   The transformation environment
 * @return the created ia32 Add node
 */
static ir_node *gen_Add(ia32_transform_env_t *env) {
	ir_node  *new_op = NULL;
	dbg_info *dbg    = env->dbg;
	ir_mode  *mode   = env->mode;
	ir_graph *irg    = env->irg;
	ir_node  *block  = env->block;
	ir_node  *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node  *nomem  = new_NoMem();
	ir_node  *expr_op, *imm_op;
	ir_node  *op1    = get_Add_left(env->irn);
	ir_node  *op2    = get_Add_right(env->irn);

	/* Check if immediate optimization is on and */
	/* if it's an operation with immediate.      */
	imm_op  = (env->cg->opt & IA32_OPT_IMMOPS) ? get_immediate_op(op1, op2) : NULL;
	expr_op = get_expr_op(op1, op2);

	assert((expr_op || imm_op) && "invalid operands");

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			return gen_binop(env, op1, op2, new_rd_ia32_xAdd);
		else
			return gen_binop(env, op1, op2, new_rd_ia32_vfadd);
	}
	else {
		/* integer ADD */
		if (!expr_op) {
			/* No expr_op means, that we have two const - one symconst and */
			/* one tarval or another symconst - because this case is not   */
			/* covered by constant folding                                 */
			/* We need to check for:                                       */
			/*  1) symconst + const    -> becomes a LEA                    */
			/*  2) symconst + symconst -> becomes a const + LEA as the elf */
			/*        linker doesn't support two symconsts                 */

			if (get_ia32_op_type(op1) == ia32_SymConst && get_ia32_op_type(op2) == ia32_SymConst) {
				/* this is the 2nd case */
				new_op = new_rd_ia32_Lea(dbg, irg, block, op1, noreg, mode);
				set_ia32_am_sc(new_op, get_ia32_id_cnst(op2));
				set_ia32_am_flavour(new_op, ia32_am_OB);

				DBG_OPT_LEA1(op2, new_op);
			}
			else {
				/* this is the 1st case */
				new_op = new_rd_ia32_Lea(dbg, irg, block, noreg, noreg, mode);

				DBG_OPT_LEA2(op1, op2, new_op);

				if (get_ia32_op_type(op1) == ia32_SymConst) {
					set_ia32_am_sc(new_op, get_ia32_id_cnst(op1));
					add_ia32_am_offs(new_op, get_ia32_cnst(op2));
				}
				else {
					add_ia32_am_offs(new_op, get_ia32_cnst(op1));
					set_ia32_am_sc(new_op, get_ia32_id_cnst(op2));
				}
				set_ia32_am_flavour(new_op, ia32_am_O);
			}

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Source);
			set_ia32_op_type(new_op, ia32_AddrModeS);

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
			new_op = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, op1, op2, nomem);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Full);
			set_ia32_commutative(new_op);
		}
	}

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	set_ia32_res_mode(new_op, mode);

	return new_rd_Proj(dbg, irg, block, new_op, mode, 0);
}



/**
 * Creates an ia32 Mul.
 *
 * @param env   The transformation environment
 * @return the created ia32 Mul node
 */
static ir_node *gen_Mul(ia32_transform_env_t *env) {
	ir_node *op1 = get_Mul_left(env->irn);
	ir_node *op2 = get_Mul_right(env->irn);
	ir_node *new_op;

	if (mode_is_float(env->mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			new_op = gen_binop(env, op1, op2, new_rd_ia32_xMul);
		else
			new_op = gen_binop(env, op1, op2, new_rd_ia32_vfmul);
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
 * @return the created ia32 Mulh node
 */
static ir_node *gen_Mulh(ia32_transform_env_t *env) {
	ir_node *op1 = get_irn_n(env->irn, 0);
	ir_node *op2 = get_irn_n(env->irn, 1);
	ir_node *proj_EAX, *proj_EDX, *mulh;
	ir_node *in[1];

	assert(!mode_is_float(env->mode) && "Mulh with float not supported");
	proj_EAX = gen_binop(env, op1, op2, new_rd_ia32_Mulh);
	mulh     = get_Proj_pred(proj_EAX);
	proj_EDX = new_rd_Proj(env->dbg, env->irg, env->block, mulh, env->mode, pn_EDX);

	/* to be on the save side */
	set_Proj_proj(proj_EAX, pn_EAX);

	if (is_ia32_ImmConst(mulh) || is_ia32_ImmSymConst(mulh)) {
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
 * @return The created ia32 And node
 */
static ir_node *gen_And(ia32_transform_env_t *env) {
	ir_node *op1 = get_And_left(env->irn);
	ir_node *op2 = get_And_right(env->irn);

	assert (! mode_is_float(env->mode));
	return gen_binop(env, op1, op2, new_rd_ia32_And);
}



/**
 * Creates an ia32 Or.
 *
 * @param env   The transformation environment
 * @return The created ia32 Or node
 */
static ir_node *gen_Or(ia32_transform_env_t *env) {
	ir_node *op1 = get_Or_left(env->irn);
	ir_node *op2 = get_Or_right(env->irn);

	assert (! mode_is_float(env->mode));
	return gen_binop(env, op1, op2, new_rd_ia32_Or);
}



/**
 * Creates an ia32 Eor.
 *
 * @param env   The transformation environment
 * @return The created ia32 Eor node
 */
static ir_node *gen_Eor(ia32_transform_env_t *env) {
	ir_node *op1 = get_Eor_left(env->irn);
	ir_node *op2 = get_Eor_right(env->irn);

	assert(! mode_is_float(env->mode));
	return gen_binop(env, op1, op2, new_rd_ia32_Eor);
}



/**
 * Creates an ia32 Max.
 *
 * @param env      The transformation environment
 * @return the created ia32 Max node
 */
static ir_node *gen_Max(ia32_transform_env_t *env) {
	ir_node *op1 = get_irn_n(env->irn, 0);
	ir_node *op2 = get_irn_n(env->irn, 1);
	ir_node *new_op;

	if (mode_is_float(env->mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			new_op = gen_binop(env, op1, op2, new_rd_ia32_xMax);
		else {
			assert(0);
		}
	}
	else {
		new_op = new_rd_ia32_Max(env->dbg, env->irg, env->block, op1, op2, env->mode);
		set_ia32_am_support(new_op, ia32_am_None);
		SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));
	}

	return new_op;
}



/**
 * Creates an ia32 Min.
 *
 * @param env      The transformation environment
 * @return the created ia32 Min node
 */
static ir_node *gen_Min(ia32_transform_env_t *env) {
	ir_node *op1 = get_irn_n(env->irn, 0);
	ir_node *op2 = get_irn_n(env->irn, 1);
	ir_node *new_op;

	if (mode_is_float(env->mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			new_op = gen_binop(env, op1, op2, new_rd_ia32_xMin);
		else {
			assert(0);
		}
	}
	else {
		new_op = new_rd_ia32_Min(env->dbg, env->irg, env->block, op1, op2, env->mode);
		set_ia32_am_support(new_op, ia32_am_None);
		SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));
	}

	return new_op;
}



/**
 * Creates an ia32 Sub with immediate.
 *
 * @param env        The transformation environment
 * @param expr_op    The first operator
 * @param const_op   The constant operator
 * @return The created ia32 Sub node
 */
static ir_node *gen_imm_Sub(ia32_transform_env_t *env, ir_node *expr_op, ir_node *const_op) {
	ir_node                *new_op     = NULL;
	tarval                 *tv         = get_ia32_Immop_tarval(const_op);
	dbg_info               *dbg        = env->dbg;
	ir_graph               *irg        = env->irg;
	ir_node                *block      = env->block;
	ir_node                *noreg      = ia32_new_NoReg_gp(env->cg);
	ir_node                *nomem      = new_NoMem();
	int                     normal_sub = 1;
	tarval_classification_t class_tv, class_negtv;
	DEBUG_ONLY(firm_dbg_module_t *mod = env->mod;)

	/* try to optimize to inc/dec  */
	if ((env->cg->opt & IA32_OPT_INCDEC) && tv) {
		/* optimize tarvals */
		class_tv    = classify_tarval(tv);
		class_negtv = classify_tarval(tarval_neg(tv));

		if (class_tv == TV_CLASSIFY_ONE) { /* - 1 == DEC */
			DB((mod, LEVEL_2, "Sub(1) to Dec ... "));
			new_op     = new_rd_ia32_Dec(dbg, irg, block, noreg, noreg, expr_op, nomem);
			normal_sub = 0;
		}
		else if (class_negtv == TV_CLASSIFY_ONE) { /* - (-1) == Sub */
			DB((mod, LEVEL_2, "Sub(-1) to Inc ... "));
			new_op     = new_rd_ia32_Inc(dbg, irg, block, noreg, noreg, expr_op, nomem);
			normal_sub = 0;
		}
	}

	if (normal_sub) {
		new_op = new_rd_ia32_Sub(dbg, irg, block, noreg, noreg, expr_op, noreg, nomem);
		set_ia32_Immop_attr(new_op, const_op);
	}

	return new_op;
}

/**
 * Creates an ia32 Sub.
 *
 * @param env   The transformation environment
 * @return The created ia32 Sub node
 */
static ir_node *gen_Sub(ia32_transform_env_t *env) {
	ir_node  *new_op = NULL;
	dbg_info *dbg    = env->dbg;
	ir_mode  *mode   = env->mode;
	ir_graph *irg    = env->irg;
	ir_node  *block  = env->block;
	ir_node  *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node  *nomem  = new_NoMem();
	ir_node  *op1    = get_Sub_left(env->irn);
	ir_node  *op2    = get_Sub_right(env->irn);
	ir_node  *expr_op, *imm_op;

	/* Check if immediate optimization is on and */
	/* if it's an operation with immediate.      */
	imm_op  = (env->cg->opt & IA32_OPT_IMMOPS) ? get_immediate_op(NULL, op2) : NULL;
	expr_op = get_expr_op(op1, op2);

	assert((expr_op || imm_op) && "invalid operands");

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			return gen_binop(env, op1, op2, new_rd_ia32_xSub);
		else
			return gen_binop(env, op1, op2, new_rd_ia32_vfsub);
	}
	else {
		/* integer SUB */
		if (!expr_op) {
			/* No expr_op means, that we have two const - one symconst and */
			/* one tarval or another symconst - because this case is not   */
			/* covered by constant folding                                 */
			/* We need to check for:                                       */
			/*  1) symconst + const    -> becomes a LEA                    */
			/*  2) symconst + symconst -> becomes a const + LEA as the elf */
			/*        linker doesn't support two symconsts                 */

			if (get_ia32_op_type(op1) == ia32_SymConst && get_ia32_op_type(op2) == ia32_SymConst) {
				/* this is the 2nd case */
				new_op = new_rd_ia32_Lea(dbg, irg, block, op1, noreg, mode);
				set_ia32_am_sc(new_op, get_ia32_id_cnst(op2));
				set_ia32_am_sc_sign(new_op);
				set_ia32_am_flavour(new_op, ia32_am_OB);

				DBG_OPT_LEA1(op2, new_op);
			}
			else {
				/* this is the 1st case */
				new_op = new_rd_ia32_Lea(dbg, irg, block, noreg, noreg, mode);

				DBG_OPT_LEA2(op1, op2, new_op);

				if (get_ia32_op_type(op1) == ia32_SymConst) {
					set_ia32_am_sc(new_op, get_ia32_id_cnst(op1));
					sub_ia32_am_offs(new_op, get_ia32_cnst(op2));
				}
				else {
					add_ia32_am_offs(new_op, get_ia32_cnst(op1));
					set_ia32_am_sc(new_op, get_ia32_id_cnst(op2));
					set_ia32_am_sc_sign(new_op);
				}
				set_ia32_am_flavour(new_op, ia32_am_O);
			}

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Source);
			set_ia32_op_type(new_op, ia32_AddrModeS);

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
			new_op = new_rd_ia32_Sub(dbg, irg, block, noreg, noreg, op1, op2, nomem);

			/* set AM support */
			set_ia32_am_support(new_op, ia32_am_Full);
		}
	}

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	set_ia32_res_mode(new_op, mode);

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
		cltd     = new_rd_ia32_Cdq(dbg, irg, block, dividend);
		dividend = new_rd_Proj(dbg, irg, block, cltd, mode_Is, pn_ia32_Cdq_EAX);
		edx_node = new_rd_Proj(dbg, irg, block, cltd, mode_Is, pn_ia32_Cdq_EDX);
	}
	else {
		edx_node = new_rd_ia32_Const(dbg, irg, block, get_irg_no_mem(irg), mode_Iu);
		set_ia32_Const_type(edx_node, ia32_Const);
		set_ia32_Immop_tarval(edx_node, get_tarval_null(mode_Iu));
	}

	res = new_rd_ia32_DivMod(dbg, irg, block, dividend, divisor, edx_node, mem, dm_flav);

	set_ia32_n_res(res, 2);

	/* Only one proj is used -> We must add a second proj and */
	/* connect this one to a Keep node to eat up the second   */
	/* destroyed register.                                    */
	if (get_irn_n_edges(irn) == 1) {
		proj = get_edge_src_irn(get_irn_out_edge_first(irn));
		assert(is_Proj(proj) && "non-Proj to Div/Mod node");

		if (get_irn_op(irn) == op_Div) {
			set_Proj_proj(proj, pn_DivMod_res_div);
			in_keep[0] = new_rd_Proj(dbg, irg, block, res, mode_Is, pn_DivMod_res_mod);
		}
		else {
			set_Proj_proj(proj, pn_DivMod_res_mod);
			in_keep[0] = new_rd_Proj(dbg, irg, block, res, mode_Is, pn_DivMod_res_div);
		}

		be_new_Keep(&ia32_reg_classes[CLASS_ia32_gp], irg, block, 1, in_keep);
	}

	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));

	set_ia32_res_mode(res, mode_Is);

	return res;
}


/**
 * Wrapper for generate_DivMod. Sets flavour_Mod.
 *
 * @param env      The transformation environment
 */
static ir_node *gen_Mod(ia32_transform_env_t *env) {
	return generate_DivMod(env, get_Mod_left(env->irn), get_Mod_right(env->irn), flavour_Mod);
}

/**
 * Wrapper for generate_DivMod. Sets flavour_Div.
 *
 * @param env      The transformation environment
 */
static ir_node *gen_Div(ia32_transform_env_t *env) {
	return generate_DivMod(env, get_Div_left(env->irn), get_Div_right(env->irn), flavour_Div);
}

/**
 * Wrapper for generate_DivMod. Sets flavour_DivMod.
 */
static ir_node *gen_DivMod(ia32_transform_env_t *env) {
	return generate_DivMod(env, get_DivMod_left(env->irn), get_DivMod_right(env->irn), flavour_DivMod);
}



/**
 * Creates an ia32 floating Div.
 *
 * @param env   The transformation environment
 * @return The created ia32 xDiv node
 */
static ir_node *gen_Quot(ia32_transform_env_t *env) {
	ir_node *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node *new_op;
	ir_node *nomem = new_rd_NoMem(env->irg);
	ir_node *op1   = get_Quot_left(env->irn);
	ir_node *op2   = get_Quot_right(env->irn);

	FP_USED(env->cg);
	if (USE_SSE2(env->cg)) {
		if (is_ia32_xConst(op2)) {
			new_op = new_rd_ia32_xDiv(env->dbg, env->irg, env->block, noreg, noreg, op1, noreg, nomem);
			set_ia32_am_support(new_op, ia32_am_None);
			set_ia32_Immop_attr(new_op, op2);
		}
		else {
			new_op = new_rd_ia32_xDiv(env->dbg, env->irg, env->block, noreg, noreg, op1, op2, nomem);
			set_ia32_am_support(new_op, ia32_am_Source);
		}
	}
	else {
		new_op = new_rd_ia32_vfdiv(env->dbg, env->irg, env->block, noreg, noreg, op1, op2, nomem);
		set_ia32_am_support(new_op, ia32_am_Source);
	}
	set_ia32_res_mode(new_op, get_irn_mode(get_proj_for_pn(env->irn, pn_Quot_res)));
	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_op;
}



/**
 * Creates an ia32 Shl.
 *
 * @param env   The transformation environment
 * @return The created ia32 Shl node
 */
static ir_node *gen_Shl(ia32_transform_env_t *env) {
	return gen_shift_binop(env, get_Shl_left(env->irn), get_Shl_right(env->irn), new_rd_ia32_Shl);
}



/**
 * Creates an ia32 Shr.
 *
 * @param env   The transformation environment
 * @return The created ia32 Shr node
 */
static ir_node *gen_Shr(ia32_transform_env_t *env) {
	return gen_shift_binop(env, get_Shr_left(env->irn), get_Shr_right(env->irn), new_rd_ia32_Shr);
}



/**
 * Creates an ia32 Shrs.
 *
 * @param env   The transformation environment
 * @return The created ia32 Shrs node
 */
static ir_node *gen_Shrs(ia32_transform_env_t *env) {
	return gen_shift_binop(env, get_Shrs_left(env->irn), get_Shrs_right(env->irn), new_rd_ia32_Shrs);
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
 * @return The created ia32 RotL or RotR node
 */
static ir_node *gen_Rot(ia32_transform_env_t *env) {
	ir_node *rotate = NULL;
	ir_node *op1    = get_Rot_left(env->irn);
	ir_node *op2    = get_Rot_right(env->irn);

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
 * @param op    The Minus operand
 * @return The created ia32 Minus node
 */
static ir_node *gen_Minus_ex(ia32_transform_env_t *env, ir_node *op) {
	ident   *name;
	ir_node *new_op;
	int      size;

	if (mode_is_float(env->mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg)) {
			ir_node *noreg_gp = ia32_new_NoReg_gp(env->cg);
			ir_node *noreg_fp = ia32_new_NoReg_fp(env->cg);
			ir_node *nomem    = new_rd_NoMem(env->irg);

			new_op = new_rd_ia32_xEor(env->dbg, env->irg, env->block, noreg_gp, noreg_gp, op, noreg_fp, nomem);

			size   = get_mode_size_bits(env->mode);
			name   = gen_fp_known_const(env->mode, size == 32 ? ia32_SSIGN : ia32_DSIGN);

			set_ia32_sc(new_op, name);

			SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

			set_ia32_res_mode(new_op, env->mode);
			set_ia32_immop_type(new_op, ia32_ImmSymConst);

			new_op = new_rd_Proj(env->dbg, env->irg, env->block, new_op, env->mode, pn_ia32_xEor_res);
		}
		else {
			new_op = new_rd_ia32_vfchs(env->dbg, env->irg, env->block, op, env->mode);
			SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));
		}
	}
	else {
		new_op = gen_unop(env, op, new_rd_ia32_Minus);
	}

	return new_op;
}

/**
 * Transforms a Minus node.
 *
 * @param env   The transformation environment
 * @return The created ia32 Minus node
 */
static ir_node *gen_Minus(ia32_transform_env_t *env) {
	return gen_Minus_ex(env, get_Minus_op(env->irn));
}


/**
 * Transforms a Not node.
 *
 * @param env   The transformation environment
 * @return The created ia32 Not node
 */
static ir_node *gen_Not(ia32_transform_env_t *env) {
	assert (! mode_is_float(env->mode));
	return gen_unop(env, get_Not_op(env->irn), new_rd_ia32_Not);
}



/**
 * Transforms an Abs node.
 *
 * @param env   The transformation environment
 * @return The created ia32 Abs node
 */
static ir_node *gen_Abs(ia32_transform_env_t *env) {
	ir_node  *res, *p_eax, *p_edx;
	dbg_info *dbg      = env->dbg;
	ir_mode  *mode     = env->mode;
	ir_graph *irg      = env->irg;
	ir_node  *block    = env->block;
	ir_node  *noreg_gp = ia32_new_NoReg_gp(env->cg);
	ir_node  *noreg_fp = ia32_new_NoReg_fp(env->cg);
	ir_node  *nomem    = new_NoMem();
	ir_node  *op       = get_Abs_op(env->irn);
	int       size;
	ident    *name;

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg)) {
			res = new_rd_ia32_xAnd(dbg,irg, block, noreg_gp, noreg_gp, op, noreg_fp, nomem);

			size   = get_mode_size_bits(mode);
			name   = gen_fp_known_const(mode, size == 32 ? ia32_SABS : ia32_DABS);

			set_ia32_sc(res, name);

			SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));

			set_ia32_res_mode(res, mode);
			set_ia32_immop_type(res, ia32_ImmSymConst);

			res = new_rd_Proj(dbg, irg, block, res, mode, pn_ia32_xAnd_res);
		}
		else {
			res = new_rd_ia32_vfabs(dbg, irg, block, op, mode);
			SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));
		}
	}
	else {
		res   = new_rd_ia32_Cdq(dbg, irg, block, op);
		SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));
		set_ia32_res_mode(res, mode);

		p_eax = new_rd_Proj(dbg, irg, block, res, mode, pn_ia32_Cdq_EAX);
		p_edx = new_rd_Proj(dbg, irg, block, res, mode, pn_ia32_Cdq_EDX);

		res   = new_rd_ia32_Eor(dbg, irg, block, noreg_gp, noreg_gp, p_eax, p_edx, nomem);
		SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));
		set_ia32_res_mode(res, mode);

		res   = new_rd_Proj(dbg, irg, block, res, mode, pn_ia32_Eor_res);

		res   = new_rd_ia32_Sub(dbg, irg, block, noreg_gp, noreg_gp, res, p_edx, nomem);
		SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));
		set_ia32_res_mode(res, mode);

		res   = new_rd_Proj(dbg, irg, block, res, mode, pn_ia32_Sub_res);
	}

	return res;
}



/**
 * Transforms a Load.
 *
 * @param env   The transformation environment
 * @return the created ia32 Load node
 */
static ir_node *gen_Load(ia32_transform_env_t *env) {
	ir_node    *node  = env->irn;
	ir_node    *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node    *ptr   = get_Load_ptr(node);
	ir_node    *lptr  = ptr;
	ir_mode    *mode  = get_Load_mode(node);
	int        is_imm = 0;
	ir_node *new_op;
	ia32_am_flavour_t am_flav = ia32_B;

	/* address might be a constant (symconst or absolute address) */
	if (is_ia32_Const(ptr)) {
		lptr   = noreg;
		is_imm = 1;
	}

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_xLoad(env->dbg, env->irg, env->block, lptr, noreg, get_Load_mem(node));
		else
			new_op = new_rd_ia32_vfld(env->dbg, env->irg, env->block, lptr, noreg, get_Load_mem(node));
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, lptr, noreg, get_Load_mem(node));
	}

	/* base is an constant address */
	if (is_imm) {
		if (get_ia32_immop_type(ptr) == ia32_ImmSymConst) {
			set_ia32_am_sc(new_op, get_ia32_id_cnst(ptr));
		}
		else {
			add_ia32_am_offs(new_op, get_ia32_cnst(ptr));
		}

		am_flav = ia32_O;
	}

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, am_flav);
	set_ia32_ls_mode(new_op, mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_op;
}



/**
 * Transforms a Store.
 *
 * @param env   The transformation environment
 * @return the created ia32 Store node
 */
static ir_node *gen_Store(ia32_transform_env_t *env) {
	ir_node *node    = env->irn;
	ir_node *noreg   = ia32_new_NoReg_gp(env->cg);
	ir_node *val     = get_Store_value(node);
	ir_node *ptr     = get_Store_ptr(node);
	ir_node *sptr    = ptr;
	ir_node *mem     = get_Store_mem(node);
	ir_mode *mode    = get_irn_mode(val);
	ir_node *sval    = val;
	int      is_imm  = 0;
	ir_node *new_op;
	ia32_am_flavour_t am_flav = ia32_B;
	ia32_immop_type_t immop   = ia32_ImmNone;

	if (! mode_is_float(mode)) {
		/* in case of storing a const (but not a symconst) -> make it an attribute */
		if (is_ia32_Cnst(val)) {
			switch (get_ia32_op_type(val)) {
			case ia32_Const:
				immop = ia32_ImmConst;
				break;
			case ia32_SymConst:
				immop = ia32_ImmSymConst;
				break;
			default:
				assert(0 && "unsupported Const type");
			}
			sval = noreg;
		}
	}

	/* address might be a constant (symconst or absolute address) */
	if (is_ia32_Const(ptr)) {
		sptr   = noreg;
		is_imm = 1;
	}

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_xStore(env->dbg, env->irg, env->block, sptr, noreg, sval, mem);
		else
			new_op = new_rd_ia32_vfst(env->dbg, env->irg, env->block, sptr, noreg, sval, mem);
	}
	else if (get_mode_size_bits(mode) == 8) {
		new_op = new_rd_ia32_Store8Bit(env->dbg, env->irg, env->block, sptr, noreg, sval, mem);
	}
	else {
		new_op = new_rd_ia32_Store(env->dbg, env->irg, env->block, sptr, noreg, sval, mem);
	}

	/* stored const is an attribute (saves a register) */
	if (! mode_is_float(mode) && is_ia32_Cnst(val)) {
		set_ia32_Immop_attr(new_op, val);
	}

	/* base is an constant address */
	if (is_imm) {
		if (get_ia32_immop_type(ptr) == ia32_ImmSymConst) {
			set_ia32_am_sc(new_op, get_ia32_id_cnst(ptr));
		}
		else {
			add_ia32_am_offs(new_op, get_ia32_cnst(ptr));
		}

		am_flav = ia32_O;
	}

	set_ia32_am_support(new_op, ia32_am_Dest);
	set_ia32_op_type(new_op, ia32_AddrModeD);
	set_ia32_am_flavour(new_op, am_flav);
	set_ia32_ls_mode(new_op, get_irn_mode(val));
	set_ia32_immop_type(new_op, immop);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

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
		cnst = (env->cg->opt & IA32_OPT_IMMOPS) ? get_immediate_op(cmp_a, cmp_b) : NULL;
		expr = get_expr_op(cmp_a, cmp_b);

		if (cnst && expr) {
			pn_Cmp pnc = get_Proj_proj(sel);

			if ((pnc == pn_Cmp_Eq || pnc == pn_Cmp_Lg) && mode_is_int(get_irn_mode(expr))) {
				if (classify_tarval(get_ia32_Immop_tarval(cnst)) == TV_CLASSIFY_NULL) {
					/* a Cmp A =/!= 0 */
					ir_node    *op1  = expr;
					ir_node    *op2  = expr;
					ir_node    *and  = skip_Proj(expr);
					const char *cnst = NULL;

					/* check, if expr is an only once used And operation */
					if (get_irn_n_edges(expr) == 1 && is_ia32_And(and)) {
						op1 = get_irn_n(and, 2);
						op2 = get_irn_n(and, 3);

						cnst = (is_ia32_ImmConst(and) || is_ia32_ImmSymConst(and)) ? get_ia32_cnst(and) : NULL;
					}
					res = new_rd_ia32_TestJmp(dbg, irg, block, op1, op2);
					set_ia32_pncode(res, get_Proj_proj(sel));
					set_ia32_res_mode(res, get_irn_mode(op1));

					if (cnst) {
						copy_ia32_Immop_attr(res, and);
					}

					SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));
					return res;
				}
			}

			if (mode_is_float(get_irn_mode(expr))) {
				FP_USED(env->cg);
				if (USE_SSE2(env->cg))
					res = new_rd_ia32_xCondJmp(dbg, irg, block, noreg, noreg, expr, noreg, nomem);
				else {
					assert(0);
				}
			}
			else {
				res = new_rd_ia32_CondJmp(dbg, irg, block, noreg, noreg, expr, noreg, nomem);
			}
			set_ia32_Immop_attr(res, cnst);
			set_ia32_res_mode(res, get_irn_mode(expr));
		}
		else {
			if (mode_is_float(get_irn_mode(cmp_a))) {
				FP_USED(env->cg);
				if (USE_SSE2(env->cg))
					res = new_rd_ia32_xCondJmp(dbg, irg, block, noreg, noreg, cmp_a, cmp_b, nomem);
				else {
					ir_node *proj_eax;
					res = new_rd_ia32_vfCondJmp(dbg, irg, block, noreg, noreg, cmp_a, cmp_b, nomem);
					proj_eax = new_r_Proj(irg, block, res, mode_Is, pn_ia32_vfCondJmp_temp_reg_eax);
					be_new_Keep(&ia32_reg_classes[CLASS_ia32_gp], irg, block, 1, &proj_eax);
				}
			}
			else {
				res = new_rd_ia32_CondJmp(dbg, irg, block, noreg, noreg, cmp_a, cmp_b, nomem);
				set_ia32_commutative(res);
			}
			set_ia32_res_mode(res, get_irn_mode(cmp_a));
		}

		set_ia32_pncode(res, get_Proj_proj(sel));
		//set_ia32_am_support(res, ia32_am_Source);
	}
	else {
		/* determine the smallest switch case value */
		int switch_min = INT_MAX;
		const ir_edge_t *edge;
		char buf[64];

		foreach_out_edge(node, edge) {
			int pn = get_Proj_proj(get_edge_src_irn(edge));
			switch_min = pn < switch_min ? pn : switch_min;
		}

		if (switch_min) {
			/* if smallest switch case is not 0 we need an additional sub */
			snprintf(buf, sizeof(buf), "%d", switch_min);
			res = new_rd_ia32_Lea(dbg, irg, block, sel, noreg, mode_Is);
			SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));
			sub_ia32_am_offs(res, buf);
			set_ia32_am_flavour(res, ia32_am_OB);
			set_ia32_am_support(res, ia32_am_Source);
			set_ia32_op_type(res, ia32_AddrModeS);
		}

		res = new_rd_ia32_SwitchJmp(dbg, irg, block, switch_min ? res : sel, mode_T);
		set_ia32_pncode(res, get_Cond_defaultProj(node));
		set_ia32_res_mode(res, get_irn_mode(sel));
	}

	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));
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

		res = new_rd_ia32_Const(dbg, irg, block, get_irg_no_mem(irg), mode_Is);
		set_ia32_op_type(res, ia32_Const);
		set_ia32_Immop_tarval(res, new_tarval_from_long(size, mode_Is));

		res = new_rd_ia32_CopyB(dbg, irg, block, dst, src, res, mem, mode);
		set_ia32_Immop_tarval(res, new_tarval_from_long(rem, mode_Is));
	}
	else {
		res = new_rd_ia32_CopyB_i(dbg, irg, block, dst, src, mem, mode);
		set_ia32_Immop_tarval(res, new_tarval_from_long(size, mode_Is));
		set_ia32_immop_type(res, ia32_ImmConst);
	}

	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env->cg, env->irn));

	return res;
}



/**
 * Transforms a Mux node into CMov.
 *
 * @param env   The transformation environment
 * @return The transformed node.
 */
static ir_node *gen_Mux(ia32_transform_env_t *env) {
#if 0
	ir_node *node   = env->irn;
	ir_node *new_op = new_rd_ia32_CMov(env->dbg, env->irg, env->block, \
		get_Mux_sel(node), get_Mux_false(node), get_Mux_true(node), env->mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_op;
#endif
	return NULL;
}


/**
 * Transforms a Psi node into CMov.
 *
 * @param env   The transformation environment
 * @return The transformed node.
 */
static ir_node *gen_Psi(ia32_transform_env_t *env) {
	ir_node *node     = env->irn;
	ir_node *cmp_proj = get_Mux_sel(node);
	ir_node *cmp, *cmp_a, *cmp_b, *new_op = NULL;

	assert(get_irn_mode(cmp_proj) == mode_b && "Condition for Psi must have mode_b");

	if (mode_is_float(env->mode)) {
		/* floating point psi */

		if (USE_SSE2(env->cg)) {
			/* unfortunately there is no conditional move for SSE */
		}
		else {
		}
	}
	else {
		ir_node *psi_true    = get_Psi_val(node, 0);
		ir_node *psi_default = get_Psi_default(node);

		/* integer psi */
		cmp   = get_Proj_pred(cmp_proj);
		cmp_a = get_Cmp_left(cmp);
		cmp_b = get_Cmp_right(cmp);

		if (is_ia32_Const_1(psi_true) && is_ia32_Const_0(psi_default)) {
			/* first case for SETcc: default is 0, set to 1 iff condition is true */
			new_op = new_rd_ia32_Set(env->dbg, env->irg, env->block, cmp_a, cmp_b, env->mode);
			set_ia32_pncode(new_op, get_Proj_proj(cmp_proj));
		}
		else if (is_ia32_Const_0(psi_true) && is_ia32_Const_1(psi_default)) {
			/* second case for SETcc: default is 1, set to 0 iff condition is true: */
			/*                        we invert condition and set default to 0      */
			new_op = new_rd_ia32_Set(env->dbg, env->irg, env->block, cmp_a, cmp_b, env->mode);
			set_ia32_pncode(new_op, get_inversed_pnc(get_Proj_proj(cmp_proj)));
		}
		else {
			/* otherwise: use CMOVcc */
			new_op = new_rd_ia32_CMov(env->dbg, env->irg, env->block, cmp_a, cmp_b, psi_true, psi_default, env->mode);
			set_ia32_pncode(new_op, get_Proj_proj(cmp_proj));
		}

		SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));
	}

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
 *  x87 is mode_E internally, conversions happen only at load and store
 *  in non-strict semantic
 */

/**
 * Create a conversion from x87 state register to general purpose.
 */
static ir_node *gen_x87_fp_to_gp(ia32_transform_env_t *env, ir_mode *tgt_mode) {
	ia32_code_gen_t *cg = env->cg;
	entity   *ent = cg->fp_to_gp;
	ir_graph *irg = env->irg;
	ir_node  *block = env->block;
	ir_node  *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node  *op = get_Conv_op(env->irn);
	ir_node  *fist, *mem, *load;

	if (! ent) {
		int size = get_mode_size_bytes(ia32_reg_classes[CLASS_ia32_vfp].mode);
		ent = cg->fp_to_gp =
			frame_alloc_area(get_irg_frame_type(env->irg), size, 16, 0);
	}

	/* do a fist */
	fist = new_rd_ia32_vfist(env->dbg, irg, block, get_irg_frame(irg), noreg, op, get_irg_no_mem(irg));

	set_ia32_frame_ent(fist, ent);
	set_ia32_use_frame(fist);
	set_ia32_am_support(fist, ia32_am_Dest);
	set_ia32_op_type(fist, ia32_AddrModeD);
	set_ia32_am_flavour(fist, ia32_B);
	set_ia32_ls_mode(fist, mode_E);

	mem  = new_r_Proj(irg, block, fist, mode_M, pn_ia32_vfist_M);

	/* do a Load */
	load = new_rd_ia32_Load(env->dbg, irg, block, get_irg_frame(irg), noreg, mem);

	set_ia32_frame_ent(load, ent);
	set_ia32_use_frame(load);
	set_ia32_am_support(load, ia32_am_Source);
	set_ia32_op_type(load, ia32_AddrModeS);
	set_ia32_am_flavour(load, ia32_B);
	set_ia32_ls_mode(load, tgt_mode);

	return new_r_Proj(irg, block, load, tgt_mode, pn_ia32_Load_res);
}

/**
 * Create a conversion from x87 state register to general purpose.
 */
static ir_node *gen_x87_gp_to_fp(ia32_transform_env_t *env, ir_mode *src_mode) {
	ia32_code_gen_t *cg = env->cg;
	entity   *ent = cg->gp_to_fp;
	ir_graph *irg = env->irg;
	ir_node  *block = env->block;
	ir_node  *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *op = get_Conv_op(env->irn);
	ir_node  *fild, *store, *mem;
	int src_bits;

	if (! ent) {
		int size = get_mode_size_bytes(ia32_reg_classes[CLASS_ia32_gp].mode);
		ent = cg->gp_to_fp =
			frame_alloc_area(get_irg_frame_type(env->irg), size, size, 0);
	}

	/* first convert to 32 bit */
	src_bits = get_mode_size_bits(src_mode);
	if (src_bits == 8) {
		op = new_rd_ia32_Conv_I2I8Bit(env->dbg, irg, block, noreg, noreg, op, nomem);
		op = new_r_Proj(irg, block, op, mode_Is, 0);
	}
	else if (src_bits < 32) {
		op = new_rd_ia32_Conv_I2I(env->dbg, irg, block, noreg, noreg, op, nomem);
		op = new_r_Proj(irg, block, op, mode_Is, 0);
	}

	/* do a store */
	store = new_rd_ia32_Store(env->dbg, irg, block, get_irg_frame(irg), noreg, op, nomem);

	set_ia32_frame_ent(store, ent);
	set_ia32_use_frame(store);

	set_ia32_am_support(store, ia32_am_Dest);
	set_ia32_op_type(store, ia32_AddrModeD);
	set_ia32_am_flavour(store, ia32_B);
	set_ia32_ls_mode(store, mode_Is);

	mem = new_r_Proj(irg, block, store, mode_M, 0);

	/* do a fild */
	fild = new_rd_ia32_vfild(env->dbg, irg, block, get_irg_frame(irg), noreg, mem);

	set_ia32_frame_ent(fild, ent);
	set_ia32_use_frame(fild);
	set_ia32_am_support(fild, ia32_am_Source);
	set_ia32_op_type(fild, ia32_AddrModeS);
	set_ia32_am_flavour(fild, ia32_B);
	set_ia32_ls_mode(fild, mode_E);

	return new_r_Proj(irg, block, fild, mode_E, 0);
}

/**
 * Transforms a Conv node.
 *
 * @param env   The transformation environment
 * @return The created ia32 Conv node
 */
static ir_node *gen_Conv(ia32_transform_env_t *env) {
	dbg_info          *dbg      = env->dbg;
	ir_graph          *irg      = env->irg;
	ir_node           *op       = get_Conv_op(env->irn);
	ir_mode           *src_mode = get_irn_mode(op);
	ir_mode           *tgt_mode = env->mode;
	int                src_bits = get_mode_size_bits(src_mode);
	int                tgt_bits = get_mode_size_bits(tgt_mode);
	ir_node           *block    = env->block;
	ir_node           *new_op   = NULL;
	ir_node           *noreg    = ia32_new_NoReg_gp(env->cg);
	ir_node           *nomem    = new_rd_NoMem(irg);
	ir_node           *proj;
	DEBUG_ONLY(firm_dbg_module_t *mod = env->mod;)

	if (src_mode == tgt_mode) {
		/* this can happen when changing mode_P to mode_Is */
		DB((mod, LEVEL_1, "killed Conv(mode, mode) ..."));
		edges_reroute(env->irn, op, irg);
	}
	else if (mode_is_float(src_mode)) {
		/* we convert from float ... */
		if (mode_is_float(tgt_mode)) {
			/* ... to float */
			if (USE_SSE2(env->cg)) {
				DB((mod, LEVEL_1, "create Conv(float, float) ..."));
				new_op = new_rd_ia32_Conv_FP2FP(dbg, irg, block, noreg, noreg, op, nomem);
			}
			else {
				DB((mod, LEVEL_1, "killed Conv(float, float) ..."));
				edges_reroute(env->irn, op, irg);
			}
		}
		else {
			/* ... to int */
			DB((mod, LEVEL_1, "create Conv(float, int) ..."));
			if (USE_SSE2(env->cg))
				new_op = new_rd_ia32_Conv_FP2I(dbg, irg, block, noreg, noreg, op, nomem);
			else
				return gen_x87_fp_to_gp(env, tgt_mode);

			/* if target mode is not int: add an additional downscale convert */
			if (tgt_bits < 32) {
				SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));
				set_ia32_am_support(new_op, ia32_am_Source);
				set_ia32_tgt_mode(new_op, tgt_mode);
				set_ia32_src_mode(new_op, src_mode);

				proj = new_rd_Proj(dbg, irg, block, new_op, mode_Is, 0);

				if (tgt_bits == 8 || src_bits == 8) {
					new_op = new_rd_ia32_Conv_I2I8Bit(dbg, irg, block, noreg, noreg, proj, nomem);
				}
				else {
					new_op = new_rd_ia32_Conv_I2I(dbg, irg, block, noreg, noreg, proj, nomem);
				}
			}
		}
	}
	else {
		/* we convert from int ... */
		if (mode_is_float(tgt_mode)) {
			FP_USED(env->cg);
			/* ... to float */
			DB((mod, LEVEL_1, "create Conv(int, float) ..."));
			if (USE_SSE2(env->cg))
				new_op = new_rd_ia32_Conv_I2FP(dbg, irg, block, noreg, noreg, op, nomem);
			else
				return gen_x87_gp_to_fp(env, src_mode);
		}
		else {
			/* ... to int */
			if (get_mode_size_bits(src_mode) == tgt_bits) {
				DB((mod, LEVEL_1, "omitting equal size Conv(%+F, %+F) ...", src_mode, tgt_mode));
				edges_reroute(env->irn, op, irg);
			}
			else {
				DB((mod, LEVEL_1, "create Conv(int, int) ...", src_mode, tgt_mode));
				if (tgt_bits == 8 || src_bits == 8) {
					new_op = new_rd_ia32_Conv_I2I8Bit(dbg, irg, block, noreg, noreg, op, nomem);
				}
				else {
					new_op = new_rd_ia32_Conv_I2I(dbg, irg, block, noreg, noreg, op, nomem);
				}
			}
		}
	}

	if (new_op) {
		SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));
		set_ia32_tgt_mode(new_op, tgt_mode);
		set_ia32_src_mode(new_op, src_mode);

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

static ir_node *gen_be_StackParam(ia32_transform_env_t *env) {
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
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_xLoad(env->dbg, env->irg, env->block, ptr, noreg, mem);
		else
			new_op = new_rd_ia32_vfld(env->dbg, env->irg, env->block, ptr, noreg, mem);
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, ptr, noreg, mem);
	}

	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_rd_Proj(env->dbg, env->irg, env->block, new_op, mode, pn_ia32_Load_res);
}

/**
 * Transforms a FrameAddr into an ia32 Add.
 */
static ir_node *gen_be_FrameAddr(ia32_transform_env_t *env) {
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *op     = get_irn_n(node, 0);
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *nomem  = new_rd_NoMem(env->irg);

	new_op = new_rd_ia32_Add(env->dbg, env->irg, env->block, noreg, noreg, op, noreg, nomem);
	set_ia32_frame_ent(new_op, be_get_frame_entity(node));
	set_ia32_am_support(new_op, ia32_am_Full);
	set_ia32_use_frame(new_op);
	set_ia32_immop_type(new_op, ia32_ImmConst);
	set_ia32_commutative(new_op);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_rd_Proj(env->dbg, env->irg, env->block, new_op, env->mode, pn_ia32_Add_res);
}

/**
 * Transforms a FrameLoad into an ia32 Load.
 */
static ir_node *gen_be_FrameLoad(ia32_transform_env_t *env) {
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *mem    = get_irn_n(node, 0);
	ir_node *ptr    = get_irn_n(node, 1);
	entity  *ent    = be_get_frame_entity(node);
	ir_mode *mode   = get_type_mode(get_entity_type(ent));

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_xLoad(env->dbg, env->irg, env->block, ptr, noreg, mem);
		else
			new_op = new_rd_ia32_vfld(env->dbg, env->irg, env->block, ptr, noreg, mem);
	}
	else
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, ptr, noreg, mem);

	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, env->irn));

	return new_op;
}


/**
 * Transforms a FrameStore into an ia32 Store.
 */
static ir_node *gen_be_FrameStore(ia32_transform_env_t *env) {
	ir_node *new_op = NULL;
	ir_node *node   = env->irn;
	ir_node *noreg  = ia32_new_NoReg_gp(env->cg);
	ir_node *mem    = get_irn_n(node, 0);
	ir_node *ptr    = get_irn_n(node, 1);
	ir_node *val    = get_irn_n(node, 2);
	entity  *ent    = be_get_frame_entity(node);
	ir_mode *mode   = get_irn_mode(val);

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_xStore(env->dbg, env->irg, env->block, ptr, noreg, val, mem);
		else
			new_op = new_rd_ia32_vfst(env->dbg, env->irg, env->block, ptr, noreg, val, mem);
	}
	else if (get_mode_size_bits(mode) == 8) {
		new_op = new_rd_ia32_Store8Bit(env->dbg, env->irg, env->block, ptr, noreg, val, mem);
	}
	else {
		new_op = new_rd_ia32_Store(env->dbg, env->irg, env->block, ptr, noreg, val, mem);
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

/**
 * This function just sets the register for the Unknown node
 * as this is not done during register allocation because Unknown
 * is an "ignore" node.
 */
static ir_node *gen_Unknown(ia32_transform_env_t *env) {
	ir_mode *mode = env->mode;
	ir_node *irn  = env->irn;

	if (mode_is_float(mode)) {
		if (USE_SSE2(env->cg))
			arch_set_irn_register(env->cg->arch_env, irn, &ia32_xmm_regs[REG_XMM_UKNWN]);
		else
			arch_set_irn_register(env->cg->arch_env, irn, &ia32_vfp_regs[REG_VFP_UKNWN]);
	}
	else if (mode_is_int(mode) || mode_is_reference(mode)) {
		arch_set_irn_register(env->cg->arch_env, irn, &ia32_gp_regs[REG_GP_UKNWN]);
	}
	else {
		assert(0 && "unsupported Unknown-Mode");
	}

	return NULL;
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
 * Transforms a Sub or xSub into Neg--Add iff OUT_REG == SRC2_REG.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
void ia32_transform_sub_to_neg_add(ir_node *irn, ia32_code_gen_t *cg) {
	ia32_transform_env_t tenv;
	ir_node *in1, *in2, *noreg, *nomem, *res;
	const arch_register_t *in1_reg, *in2_reg, *out_reg, **slots;

	/* Return if AM node or not a Sub or xSub */
	if (get_ia32_op_type(irn) != ia32_Normal || !(is_ia32_Sub(irn) || is_ia32_xSub(irn)))
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
	tenv.mode     = get_ia32_res_mode(irn);
	tenv.cg       = cg;
	DEBUG_ONLY(tenv.mod      = cg->mod;)

	/* in case of sub and OUT == SRC2 we can transform the sequence into neg src2 -- add */
	if (REGS_ARE_EQUAL(out_reg, in2_reg)) {
		/* generate the neg src2 */
		res = gen_Minus_ex(&tenv, in2);
		arch_set_irn_register(cg->arch_env, res, in2_reg);

		/* add to schedule */
		sched_add_before(irn, res);

		/* generate the add */
		if (mode_is_float(tenv.mode)) {
			res = new_rd_ia32_xAdd(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, res, in1, nomem);
			set_ia32_am_support(res, ia32_am_Source);
		}
		else {
			res = new_rd_ia32_Add(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, res, in1, nomem);
			set_ia32_am_support(res, ia32_am_Full);
			set_ia32_commutative(res);
		}
	    set_ia32_res_mode(res, tenv.mode);

		SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(tenv.cg, irn));
		/* copy register */
		slots    = get_ia32_slots(res);
		slots[0] = in2_reg;

		/* add to schedule */
		sched_add_before(irn, res);

		/* remove the old sub */
		sched_remove(irn);

		DBG_OPT_SUB2NEGADD(irn, res);

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
	DEBUG_ONLY(tenv.mod   = cg->mod;)
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

	res = new_rd_ia32_Add(tenv.dbg, tenv.irg, tenv.block, noreg, noreg, op1, op2, nomem);
	arch_set_irn_register(cg->arch_env, res, out_reg);
	set_ia32_op_type(res, ia32_Normal);
	set_ia32_commutative(res);
	set_ia32_res_mode(res, tenv.mode);

	if (imm) {
		set_ia32_cnst(res, offs);
		set_ia32_immop_type(res, ia32_ImmConst);
	}

	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(cg, irn));

	/* add Add to schedule */
	sched_add_before(irn, res);

	DBG_OPT_LEA2ADD(irn, res);

	res = new_rd_Proj(tenv.dbg, tenv.irg, tenv.block, res, tenv.mode, 0);

	/* add result Proj to schedule */
	sched_add_before(irn, res);

	/* remove the old LEA */
	sched_remove(irn);

	/* exchange the Add and the LEA */
	exchange(irn, res);
}

/**
 * the BAD transformer.
 */
static ir_node *bad_transform(ia32_transform_env_t *env) {
	ir_fprintf(stderr, "Not implemented: %+F\n", env->irn);
	assert(0);
	return NULL;
}

/**
 * Enters all transform functions into the generic pointer
 */
void ia32_register_transformers(void) {
	ir_op *op_Max, *op_Min, *op_Mulh;

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

#define GEN(a)   op_##a->ops.generic = (op_func)gen_##a
#define BAD(a)   op_##a->ops.generic = (op_func)bad_transform
#define IGN(a)

	GEN(Add);
	GEN(Sub);
	GEN(Mul);
	GEN(And);
	GEN(Or);
	GEN(Eor);

	GEN(Shl);
	GEN(Shr);
	GEN(Shrs);
	GEN(Rot);

	GEN(Quot);

	GEN(Div);
	GEN(Mod);
	GEN(DivMod);

	GEN(Minus);
	GEN(Conv);
	GEN(Abs);
	GEN(Not);

	GEN(Load);
	GEN(Store);
	GEN(Cond);

	GEN(CopyB);
	GEN(Mux);
	GEN(Psi);

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

	GEN(be_FrameAddr);
	GEN(be_FrameLoad);
	GEN(be_FrameStore);
	GEN(be_StackParam);

	/* set the register for all Unknown nodes */
	GEN(Unknown);

	op_Max = get_op_Max();
	if (op_Max)
		GEN(Max);
	op_Min = get_op_Min();
	if (op_Min)
		GEN(Min);
	op_Mulh = get_op_Mulh();
	if (op_Mulh)
		GEN(Mulh);

#undef GEN
#undef BAD
#undef IGN
}

typedef ir_node *(transform_func)(ia32_transform_env_t *env);

/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void ia32_transform_node(ir_node *node, void *env) {
	ia32_code_gen_t *cg = (ia32_code_gen_t *)env;
	ir_op *op           = get_irn_op(node);
	ir_node *asm_node   = NULL;
	int i;

	if (is_Block(node))
		return;

	/* link arguments pointing to Unknown to the UNKNOWN Proj */
	for (i = get_irn_arity(node) - 1; i >= 0; i--) {
		if (is_Unknown(get_irn_n(node, i)))
			set_irn_n(node, i, be_get_unknown_for_mode(cg, get_irn_mode(get_irn_n(node, i))));
	}

	DBG((cg->mod, LEVEL_1, "check %+F ... ", node));
	if (op->ops.generic) {
		ia32_transform_env_t  tenv;
		transform_func *transform = (transform_func *)op->ops.generic;

		tenv.block    = get_nodes_block(node);
		tenv.dbg      = get_irn_dbg_info(node);
		tenv.irg      = current_ir_graph;
		tenv.irn      = node;
		tenv.mode     = get_irn_mode(node);
		tenv.cg       = cg;
		DEBUG_ONLY(tenv.mod = cg->mod;)

		asm_node = (*transform)(&tenv);
	}

	/* exchange nodes if a new one was generated */
	if (asm_node) {
		exchange(node, asm_node);
		DB((cg->mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DB((cg->mod, LEVEL_1, "ignored\n"));
	}
}
