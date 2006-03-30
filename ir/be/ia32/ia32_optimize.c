#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode.h"
#include "irprog_t.h"
#include "ircons.h"
#include "firm_types.h"
#include "iredges.h"
#include "tv.h"
#include "irgmod.h"

#include "../be_t.h"
#include "../beabi.h"
#include "../benode_t.h"
#include "../besched_t.h"

#include "ia32_new_nodes.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"     /* the generated interface (register type and class defenitions) */

#undef is_NoMem
#define is_NoMem(irn) (get_irn_op(irn) == op_NoMem)

typedef int is_op_func_t(const ir_node *n);

/**
 * checks if a node represents the NOREG value
 */
static int be_is_NoReg(ia32_code_gen_t *cg, const ir_node *irn) {
  be_abi_irg_t *babi = cg->birg->abi;
	const arch_register_t *fp_noreg = USE_SSE2(cg) ?
		&ia32_xmm_regs[REG_XMM_NOREG] : &ia32_vfp_regs[REG_VFP_NOREG];

	return (be_abi_get_callee_save_irn(babi, &ia32_gp_regs[REG_GP_NOREG]) == irn) ||
	       (be_abi_get_callee_save_irn(babi, fp_noreg) == irn);
}



/*************************************************
 *   _____                _              _
 *  / ____|              | |            | |
 * | |     ___  _ __  ___| |_ __ _ _ __ | |_ ___
 * | |    / _ \| '_ \/ __| __/ _` | '_ \| __/ __|
 * | |___| (_) | | | \__ \ || (_| | | | | |_\__ \
 *  \_____\___/|_| |_|___/\__\__,_|_| |_|\__|___/
 *
 *************************************************/

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
static ir_node *gen_SymConst(ia32_transform_env_t *env) {
	ir_node  *cnst;
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;

	if (mode_is_float(mode)) {
		if (USE_SSE2(env->cg))
			cnst = new_rd_ia32_fConst(dbg, irg, block, mode);
		else
			cnst = new_rd_ia32_vfConst(dbg, irg, block, mode);
	}
	else {
		cnst = new_rd_ia32_Const(dbg, irg, block, mode);
	}
	set_ia32_Const_attr(cnst, env->irn);
	return cnst;
}

/**
 * Get a primitive type for a mode.
 */
static ir_type *get_prim_type(pmap *types, ir_mode *mode)
{
	pmap_entry *e = pmap_find(types, mode);
	ir_type *res;

	if (! e) {
		char buf[64];
		snprintf(buf, sizeof(buf), "prim_type_%s", get_mode_name(mode));
		res = new_type_primitive(new_id_from_str(buf), mode);
		pmap_insert(types, mode, res);
	}
	else
		res = e->value;
	return res;
}

/**
 * Get an entity that is initialized with a tarval
 */
static entity *get_entity_for_tv(ia32_code_gen_t *cg, ir_node *cnst)
{
	tarval *tv    = get_Const_tarval(cnst);
	pmap_entry *e = pmap_find(cg->tv_ent, tv);
	entity *res;
	ir_graph *rem;

	if (! e) {
		ir_mode *mode = get_irn_mode(cnst);
		ir_type *tp = get_Const_type(cnst);
		if (tp == firm_unknown_type)
			tp = get_prim_type(cg->types, mode);

		res = new_entity(get_glob_type(), unique_id("ia32FloatCnst_%u"), tp);

		set_entity_ld_ident(res, get_entity_ident(res));
		set_entity_visibility(res, visibility_local);
		set_entity_variability(res, variability_constant);
		set_entity_allocation(res, allocation_static);

		 /* we create a new entity here: It's initialization must resist on the
		    const code irg */
		rem = current_ir_graph;
		current_ir_graph = get_const_code_irg();
		set_atomic_ent_value(res, new_Const_type(tv, tp));
		current_ir_graph = rem;
	}
	else
		res = e->value;
	return res;
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
static ir_node *gen_Const(ia32_transform_env_t *env) {
	ir_node *cnst;
	symconst_symbol sym;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;
	ir_node  *node  = env->irn;
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;

	if (mode_is_float(mode)) {
		if (! USE_SSE2(env->cg)) {
			cnst_classify_t clss = classify_Const(node);

			if (clss == CNST_NULL)
				return new_rd_ia32_vfldz(dbg, irg, block, mode);
			else if (clss == CNST_ONE)
				return new_rd_ia32_vfld1(dbg, irg, block, mode);
		}
		sym.entity_p = get_entity_for_tv(env->cg, node);

		cnst = new_rd_SymConst(dbg, irg, block, sym, symconst_addr_ent);
		env->irn = cnst;
		cnst = gen_SymConst(env);
	}
	else {
		cnst = new_rd_ia32_Const(dbg, irg, block, get_irn_mode(node));
		set_ia32_Const_attr(cnst, node);
	}
	return cnst;
}



/**
 * Transforms (all) Const's into ia32_Const and places them in the
 * block where they are used (or in the cfg-pred Block in case of Phi's).
 * Additionally all reference nodes are changed into mode_Is nodes.
 */
void ia32_place_consts_set_modes(ir_node *irn, void *env) {
	ia32_code_gen_t      *cg = env;
	ia32_transform_env_t  tenv;
	ir_mode              *mode;
	ir_node              *pred, *cnst;
	int                   i;
	opcode                opc;

	if (is_Block(irn))
		return;

	mode = get_irn_mode(irn);

	/* transform all reference nodes into mode_Is nodes */
	if (mode_is_reference(mode)) {
		mode = mode_Is;
		set_irn_mode(irn, mode);
	}

	tenv.block    = get_nodes_block(irn);
	tenv.cg       = cg;
	tenv.irg      = cg->irg;
	DEBUG_ONLY(tenv.mod      = cg->mod;)

	/* Loop over all predecessors and check for Sym/Const nodes */
	for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
		pred      = get_irn_n(irn, i);
		cnst      = NULL;
		opc       = get_irn_opcode(pred);
		tenv.irn  = pred;
		tenv.mode = get_irn_mode(pred);
		tenv.dbg  = get_irn_dbg_info(pred);

		/* If it's a Phi, then we need to create the */
		/* new Const in it's predecessor block       */
		if (is_Phi(irn)) {
			tenv.block = get_Block_cfgpred_block(get_nodes_block(irn), i);
		}

		/* put the const into the block where the original const was */
		if (! cg->opt.placecnst) {
			tenv.block = get_nodes_block(pred);
		}

		switch (opc) {
			case iro_Const:
				cnst = gen_Const(&tenv);
				break;
			case iro_SymConst:
				cnst = gen_SymConst(&tenv);
				break;
			default:
				break;
		}

		/* if we found a const, then set it */
		if (cnst) {
			set_irn_n(irn, i, cnst);
		}
	}
}



/********************************************************************************************************
 *  _____                _           _         ____        _   _           _          _   _
 * |  __ \              | |         | |       / __ \      | | (_)         (_)        | | (_)
 * | |__) |__  ___ _ __ | |__   ___ | | ___  | |  | |_ __ | |_ _ _ __ ___  _ ______ _| |_ _  ___  _ __
 * |  ___/ _ \/ _ \ '_ \| '_ \ / _ \| |/ _ \ | |  | | '_ \| __| | '_ ` _ \| |_  / _` | __| |/ _ \| '_ \
 * | |  |  __/  __/ |_) | | | | (_) | |  __/ | |__| | |_) | |_| | | | | | | |/ / (_| | |_| | (_) | | | |
 * |_|   \___|\___| .__/|_| |_|\___/|_|\___|  \____/| .__/ \__|_|_| |_| |_|_/___\__,_|\__|_|\___/|_| |_|
 *                | |                               | |
 *                |_|                               |_|
 ********************************************************************************************************/

/**
 * NOTE: THESE PEEPHOLE OPTIMIZATIONS MUST BE CALLED AFTER SCHEDULING AND REGISTER ALLOCATION.
 */

static int ia32_cnst_compare(ir_node *n1, ir_node *n2) {
	return get_ia32_id_cnst(n1) == get_ia32_id_cnst(n2);
}

/**
 * Checks for potential CJmp/CJmpAM optimization candidates.
 */
static ir_node *ia32_determine_cjmp_cand(ir_node *irn, is_op_func_t *is_op_func) {
	ir_node *cand = NULL;
	ir_node *prev = sched_prev(irn);

	if (is_Block(prev)) {
		if (get_Block_n_cfgpreds(prev) == 1)
			prev = get_Block_cfgpred(prev, 0);
		else
			prev = NULL;
	}

	/* The predecessor must be a ProjX. */
	if (prev && is_Proj(prev) && get_irn_mode(prev) == mode_X) {
		prev = get_Proj_pred(prev);

		if (is_op_func(prev))
			cand = prev;
	}

	return cand;
}

static int is_TestJmp_cand(const ir_node *irn) {
	return is_ia32_TestJmp(irn) || is_ia32_And(irn);
}

/**
 * Checks if two consecutive arguments of cand matches
 * the two arguments of irn (TestJmp).
 */
static int is_TestJmp_replacement(ir_node *cand, ir_node *irn) {
	ir_node *in1       = get_irn_n(irn, 0);
	ir_node *in2       = get_irn_n(irn, 1);
	int      i, n      = get_irn_arity(cand);
	int      same_args = 0;

	for (i = 0; i < n - 1; i++) {
		if (get_irn_n(cand, i)     == in1 &&
			get_irn_n(cand, i + 1) == in2)
		{
			same_args = 1;
			break;
		}
	}

	if (same_args)
		return ia32_cnst_compare(cand, irn);

	return 0;
}

/**
 * Tries to replace a TestJmp by a CJmp or CJmpAM (in case of And)
 */
static void ia32_optimize_TestJmp(ir_node *irn, ia32_code_gen_t *cg) {
	ir_node *cand    = ia32_determine_cjmp_cand(irn, is_TestJmp_cand);
	int      replace = 0;

	/* we found a possible candidate */
	replace = cand ? is_TestJmp_replacement(cand, irn) : 0;

	if (replace) {
		DBG((cg->mod, LEVEL_1, "replacing %+F by ", irn));

		if (is_ia32_And(cand))
			set_irn_op(irn, op_ia32_CJmpAM);
		else
			set_irn_op(irn, op_ia32_CJmp);

		DB((cg->mod, LEVEL_1, "%+F\n", irn));
	}
}

static int is_CondJmp_cand(const ir_node *irn) {
	return is_ia32_CondJmp(irn) || is_ia32_Sub(irn);
}

/**
 * Checks if the arguments of cand are the same of irn.
 */
static int is_CondJmp_replacement(ir_node *cand, ir_node *irn) {
	int i, n      = get_irn_arity(cand);
	int same_args = 1;

	for (i = 0; i < n; i++) {
		if (get_irn_n(cand, i) == get_irn_n(irn, i)) {
			same_args = 0;
			break;
		}
	}

	if (same_args)
		return ia32_cnst_compare(cand, irn);

	return 0;
}

/**
 * Tries to replace a CondJmp by a CJmpAM
 */
static void ia32_optimize_CondJmp(ir_node *irn, ia32_code_gen_t *cg) {
	ir_node *cand    = ia32_determine_cjmp_cand(irn, is_CondJmp_cand);
	int      replace = 0;

	/* we found a possible candidate */
	replace = cand ? is_CondJmp_replacement(cand, irn) : 0;

	if (replace) {
		DBG((cg->mod, LEVEL_1, "replacing %+F by ", irn));

		set_irn_op(irn, op_ia32_CJmp);

		DB((cg->mod, LEVEL_1, "%+F\n", irn));
	}
}

/**
 * Performs Peephole Optimizations
 */
void ia32_peephole_optimization(ir_node *irn, void *env) {
	if (is_ia32_TestJmp(irn)) {
		ia32_optimize_TestJmp(irn, env);
	}
	else if (is_ia32_CondJmp(irn)) {
		ia32_optimize_CondJmp(irn, env);
	}
}



/******************************************************************
 *              _     _                   __  __           _
 *     /\      | |   | |                 |  \/  |         | |
 *    /  \   __| | __| |_ __ ___  ___ ___| \  / | ___   __| | ___
 *   / /\ \ / _` |/ _` | '__/ _ \/ __/ __| |\/| |/ _ \ / _` |/ _ \
 *  / ____ \ (_| | (_| | | |  __/\__ \__ \ |  | | (_) | (_| |  __/
 * /_/    \_\__,_|\__,_|_|  \___||___/___/_|  |_|\___/ \__,_|\___|
 *
 ******************************************************************/

static int node_is_ia32_comm(const ir_node *irn) {
	return is_ia32_irn(irn) ? is_ia32_commutative(irn) : 0;
}

static int ia32_get_irn_n_edges(const ir_node *irn) {
	const ir_edge_t *edge;
	int cnt = 0;

	foreach_out_edge(irn, edge) {
		cnt++;
	}

	return cnt;
}

/**
 * Returns the first mode_M Proj connected to irn.
 */
static ir_node *get_mem_proj(const ir_node *irn) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");

		if (get_irn_mode(src) == mode_M)
			return src;
	}

	return NULL;
}

/**
 * Returns the first Proj with mode != mode_M connected to irn.
 */
static ir_node *get_res_proj(const ir_node *irn) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");

		if (get_irn_mode(src) != mode_M)
			return src;
	}

	return NULL;
}

/**
 * Determines if pred is a Proj and if is_op_func returns true for it's predecessor.
 *
 * @param pred       The node to be checked
 * @param is_op_func The check-function
 * @return 1 if conditions are fulfilled, 0 otherwise
 */
static int pred_is_specific_node(const ir_node *pred, is_op_func_t *is_op_func) {
	if (is_Proj(pred) && is_op_func(get_Proj_pred(pred))) {
		return 1;
	}

	return 0;
}

/**
 * Determines if pred is a Proj and if is_op_func returns true for it's predecessor
 * and if the predecessor is in block bl.
 *
 * @param bl         The block
 * @param pred       The node to be checked
 * @param is_op_func The check-function
 * @return 1 if conditions are fulfilled, 0 otherwise
 */
static int pred_is_specific_nodeblock(const ir_node *bl, const ir_node *pred,
	int (*is_op_func)(const ir_node *n))
{
	if (is_Proj(pred)) {
		pred = get_Proj_pred(pred);
		if ((bl == get_nodes_block(pred)) && is_op_func(pred)) {
			return 1;
		}
	}

	return 0;
}



/**
 * Checks if irn is a candidate for address calculation or address mode.
 *
 * address calculation (AC):
 * - none of the operand must be a Load  within the same block OR
 * - all Loads must have more than one user                    OR
 * - the irn has a frame entity (it's a former FrameAddr)
 *
 * address mode (AM):
 * - at least one operand has to be a Load within the same block AND
 * - the load must not have other users than the irn             AND
 * - the irn must not have a frame entity set
 *
 * @param block       The block the Loads must/not be in
 * @param irn         The irn to check
 * @param check_addr  1 if to check for address calculation, 0 otherwise
 * return 1 if irn is a candidate for AC or AM, 0 otherwise
 */
static int is_candidate(const ir_node *block, const ir_node *irn, int check_addr) {
	ir_node *in;
	int      n, is_cand = check_addr;

	in = get_irn_n(irn, 2);

	if (pred_is_specific_nodeblock(block, in, is_ia32_Ld)) {
		n         = ia32_get_irn_n_edges(in);
		is_cand   = check_addr ? (n == 1 ? 0 : is_cand) : (n == 1 ? 1 : is_cand);
	}

	in = get_irn_n(irn, 3);

	if (pred_is_specific_nodeblock(block, in, is_ia32_Ld)) {
		n         = ia32_get_irn_n_edges(in);
		is_cand   = check_addr ? (n == 1 ? 0 : is_cand) : (n == 1 ? 1 : is_cand);
	}

	is_cand = get_ia32_frame_ent(irn) ? (check_addr ? 1 : 0) : is_cand;

	return is_cand;
}

/**
 * Compares the base and index addr and the load/store entities
 * and returns 1 if they are equal.
 */
static int load_store_addr_is_equal(const ir_node *load, const ir_node *store,
									const ir_node *addr_b, const ir_node *addr_i)
{
	int     is_equal = (addr_b == get_irn_n(load, 0)) && (addr_i == get_irn_n(load, 1));
	entity *lent     = get_ia32_frame_ent(load);
	entity *sent     = get_ia32_frame_ent(store);
	ident  *lid      = get_ia32_am_sc(load);
	ident  *sid      = get_ia32_am_sc(store);
	char   *loffs    = get_ia32_am_offs(load);
	char   *soffs    = get_ia32_am_offs(store);

	/* are both entities set and equal? */
	if (is_equal && (lent || sent))
		is_equal = lent && sent && (lent == sent);

	/* are address mode idents set and equal? */
	if (is_equal && (lid || sid))
		is_equal = lid && sid && (lid == sid);

	/* are offsets set and equal */
	if (is_equal && (loffs || soffs))
		is_equal = loffs && soffs && strcmp(loffs, soffs) == 0;

	/* are the load and the store of the same mode? */
	is_equal = is_equal ? get_ia32_ls_mode(load) == get_ia32_ls_mode(store) : 0;

	return is_equal;
}



/**
 * Folds Add or Sub to LEA if possible
 */
static ir_node *fold_addr(ia32_code_gen_t *cg, ir_node *irn, ir_node *noreg) {
	ir_graph   *irg        = get_irn_irg(irn);
	dbg_info   *dbg        = get_irn_dbg_info(irn);
	ir_node    *block      = get_nodes_block(irn);
	ir_node    *res        = irn;
	char       *offs       = NULL;
	const char *offs_cnst  = NULL;
	char       *offs_lea   = NULL;
	int         scale      = 0;
	int         isadd      = 0;
	int         dolea      = 0;
	int         have_am_sc = 0;
	int         am_sc_sign = 0;
	ident      *am_sc      = NULL;
	ir_node    *left, *right, *temp;
	ir_node    *base, *index;
	ia32_am_flavour_t am_flav;
	DEBUG_ONLY(firm_dbg_module_t *mod = cg->mod;)

	if (is_ia32_Add(irn))
		isadd = 1;

	left  = get_irn_n(irn, 2);
	right = get_irn_n(irn, 3);

	/* "normalize" arguments in case of add with two operands */
	if  (isadd && ! be_is_NoReg(cg, right)) {
		/* put LEA == ia32_am_O as right operand */
		if (is_ia32_Lea(left) && get_ia32_am_flavour(left) == ia32_am_O) {
			set_irn_n(irn, 2, right);
			set_irn_n(irn, 3, left);
			temp  = left;
			left  = right;
			right = temp;
		}

		/* put LEA != ia32_am_O as left operand */
		if (is_ia32_Lea(right) && get_ia32_am_flavour(right) != ia32_am_O) {
			set_irn_n(irn, 2, right);
			set_irn_n(irn, 3, left);
			temp  = left;
			left  = right;
			right = temp;
		}

		/* put SHL as left operand iff left is NOT a LEA */
		if (! is_ia32_Lea(left) && pred_is_specific_node(right, is_ia32_Shl)) {
			set_irn_n(irn, 2, right);
			set_irn_n(irn, 3, left);
			temp  = left;
			left  = right;
			right = temp;
		}
	}

	base    = left;
	index   = noreg;
	offs    = NULL;
	scale   = 0;
	am_flav = 0;

	/* check for operation with immediate */
	if (is_ia32_ImmConst(irn)) {
		DBG((mod, LEVEL_1, "\tfound op with imm const"));

		offs_cnst = get_ia32_cnst(irn);
		dolea     = 1;
	}
	else if (is_ia32_ImmSymConst(irn)) {
		DBG((mod, LEVEL_1, "\tfound op with imm symconst"));

		have_am_sc = 1;
		dolea      = 1;
		am_sc      = get_ia32_id_cnst(irn);
		am_sc_sign = is_ia32_am_sc_sign(irn);
	}

	/* determine the operand which needs to be checked */
	if (be_is_NoReg(cg, right)) {
		temp = left;
	}
	else {
		temp = right;
	}

	/* check if right operand is AMConst (LEA with ia32_am_O)  */
	/* but we can only eat it up if there is no other symconst */
	/* because the linker won't accept two symconsts           */
	if (! have_am_sc && is_ia32_Lea(temp) && get_ia32_am_flavour(temp) == ia32_am_O) {
		DBG((mod, LEVEL_1, "\tgot op with LEA am_O"));

		offs_lea   = get_ia32_am_offs(temp);
		am_sc      = get_ia32_am_sc(temp);
		am_sc_sign = is_ia32_am_sc_sign(temp);
		have_am_sc = 1;
		dolea      = 1;
	}

	if (isadd) {
		/* default for add -> make right operand to index */
		index = right;
		dolea = 1;

		DBG((mod, LEVEL_1, "\tgot LEA candidate with index %+F\n", index));

		/* determine the operand which needs to be checked */
		temp = left;
		if (is_ia32_Lea(left)) {
			temp = right;
		}

		/* check for SHL 1,2,3 */
		if (pred_is_specific_node(temp, is_ia32_Shl)) {
			temp = get_Proj_pred(temp);

			if (get_ia32_Immop_tarval(temp)) {
				scale = get_tarval_long(get_ia32_Immop_tarval(temp));

				if (scale <= 3) {
					index = get_irn_n(temp, 2);

					DBG((mod, LEVEL_1, "\tgot scaled index %+F\n", index));
				}
			}
		}

		/* fix base */
		if (! be_is_NoReg(cg, index)) {
			/* if we have index, but left == right -> no base */
			if (left == right) {
				base = noreg;
			}
			else if (! is_ia32_Lea(left) && (index != right)) {
				/* index != right -> we found a good Shl           */
				/* left  != LEA   -> this Shl was the left operand */
				/* -> base is right operand                        */
				base = right;
			}
		}
	}

	/* Try to assimilate a LEA as left operand */
	if (is_ia32_Lea(left) && (get_ia32_am_flavour(left) != ia32_am_O)) {
		am_flav = get_ia32_am_flavour(left);

		/* If we have an Add with a real right operand (not NoReg) and  */
		/* the LEA contains already an index calculation then we create */
		/* a new LEA.                                                   */
		/* If the LEA contains already a frame_entity then we also      */
		/* create a new one  otherwise we would loose it.               */
		if ((isadd && !be_is_NoReg(cg, index) && (am_flav & ia32_am_I)) || /* no new LEA if index already set */
			get_ia32_frame_ent(left)                                    || /* no new LEA if stack access */
			(have_am_sc && get_ia32_am_sc(left)))                          /* no new LEA if AM symconst already present */
		{
			DBG((mod, LEVEL_1, "\tleave old LEA, creating new one\n"));
		}
		else {
			DBG((mod, LEVEL_1, "\tgot LEA as left operand ... assimilating\n"));
			offs       = get_ia32_am_offs(left);
			am_sc      = have_am_sc ? am_sc : get_ia32_am_sc(left);
			have_am_sc = am_sc ? 1 : 0;
			am_sc_sign = is_ia32_am_sc_sign(left);
			base       = get_irn_n(left, 0);
			index      = get_irn_n(left, 1);
			scale      = get_ia32_am_scale(left);
		}
	}

	/* ok, we can create a new LEA */
	if (dolea) {
		res = new_rd_ia32_Lea(dbg, irg, block, base, index, mode_Is);

		/* add the old offset of a previous LEA */
		if (offs) {
			add_ia32_am_offs(res, offs);
		}

		/* add the new offset */
		if (isadd) {
			if (offs_cnst) {
				add_ia32_am_offs(res, offs_cnst);
			}
			if (offs_lea) {
				add_ia32_am_offs(res, offs_lea);
			}
		}
		else {
			/* either lea_O-cnst, -cnst or -lea_O  */
			if (offs_cnst) {
				if (offs_lea) {
					add_ia32_am_offs(res, offs_lea);
				}

				sub_ia32_am_offs(res, offs_cnst);
			}
			else {
				sub_ia32_am_offs(res, offs_lea);
			}
		}

		/* set the address mode symconst */
		if (have_am_sc) {
			set_ia32_am_sc(res, am_sc);
			if (am_sc_sign)
				set_ia32_am_sc_sign(res);
		}

		/* copy the frame entity (could be set in case of Add */
		/* which was a FrameAddr) */
		set_ia32_frame_ent(res, get_ia32_frame_ent(irn));

		if (is_ia32_use_frame(irn))
			set_ia32_use_frame(res);

		/* set scale */
		set_ia32_am_scale(res, scale);

		am_flav = ia32_am_N;
		/* determine new am flavour */
		if (offs || offs_cnst || offs_lea) {
			am_flav |= ia32_O;
		}
		if (! be_is_NoReg(cg, base)) {
			am_flav |= ia32_B;
		}
		if (! be_is_NoReg(cg, index)) {
			am_flav |= ia32_I;
		}
		if (scale > 0) {
			am_flav |= ia32_S;
		}
		set_ia32_am_flavour(res, am_flav);

		set_ia32_op_type(res, ia32_AddrModeS);

		DBG((mod, LEVEL_1, "\tLEA [%+F + %+F * %d + %s]\n", base, index, scale, get_ia32_am_offs(res)));

		/* get the result Proj of the Add/Sub */
		irn = get_res_proj(irn);

		assert(irn && "Couldn't find result proj");

		/* exchange the old op with the new LEA */
		exchange(irn, res);
	}

	return res;
}

/**
 * Optimizes a pattern around irn to address mode if possible.
 */
void ia32_optimize_am(ir_node *irn, void *env) {
	ia32_code_gen_t   *cg   = env;
	ir_node           *res  = irn;
	dbg_info          *dbg;
	ir_mode           *mode;
	ir_node           *block, *noreg_gp, *noreg_fp;
	ir_node           *left, *right, *temp;
	ir_node           *store, *load, *mem_proj;
	ir_node           *succ, *addr_b, *addr_i;
	int                check_am_src = 0;
	DEBUG_ONLY(firm_dbg_module_t *mod = cg->mod;)

	if (! is_ia32_irn(irn))
		return;

	dbg      = get_irn_dbg_info(irn);
	mode     = get_irn_mode(irn);
	block    = get_nodes_block(irn);
	noreg_gp = ia32_new_NoReg_gp(cg);
	noreg_fp = ia32_new_NoReg_fp(cg);

	DBG((mod, LEVEL_1, "checking for AM\n"));

	/* 1st part: check for address calculations and transform the into Lea */

	/* Following cases can occur:                                  */
	/* - Sub (l, imm) -> LEA [base - offset]                       */
	/* - Sub (l, r == LEA with ia32_am_O)   -> LEA [base - offset] */
	/* - Add (l, imm) -> LEA [base + offset]                       */
	/* - Add (l, r == LEA with ia32_am_O)  -> LEA [base + offset]  */
	/* - Add (l == LEA with ia32_am_O, r)  -> LEA [base + offset]  */
	/* - Add (l, r) -> LEA [base + index * scale]                  */
	/*              with scale > 1 iff l/r == shl (1,2,3)          */

	if (is_ia32_Sub(irn) || is_ia32_Add(irn)) {
		left  = get_irn_n(irn, 2);
		right = get_irn_n(irn, 3);

	    /* Do not try to create a LEA if one of the operands is a Load. */
		/* check is irn is a candidate for address calculation */
		if (is_candidate(block, irn, 1)) {
			DBG((mod, LEVEL_1, "\tfound address calculation candidate %+F ... ", irn));
			res = fold_addr(cg, irn, noreg_gp);

			if (res == irn)
				DB((mod, LEVEL_1, "transformed into %+F\n", res));
			else
				DB((mod, LEVEL_1, "not transformed\n"));
		}
	}

	/* 2nd part: fold following patterns:                                               */
	/* - Load  -> LEA into Load  } TODO: If the LEA is used by more than one Load/Store */
	/* - Store -> LEA into Store }       it might be better to keep the LEA             */
	/* - op -> Load into AMop with am_Source                                            */
	/*   conditions:                                                                    */
	/*     - op is am_Source capable AND                                                */
	/*     - the Load is only used by this op AND                                       */
	/*     - the Load is in the same block                                              */
	/* - Store -> op -> Load  into AMop with am_Dest                                    */
	/*   conditions:                                                                    */
	/*     - op is am_Dest capable AND                                                  */
	/*     - the Store uses the same address as the Load AND                            */
	/*     - the Load is only used by this op AND                                       */
	/*     - the Load and Store are in the same block AND                               */
	/*     - nobody else uses the result of the op                                      */

	if ((res == irn) && (get_ia32_am_support(irn) != ia32_am_None) && !is_ia32_Lea(irn)) {
		/* 1st: check for Load/Store -> LEA   */
		if (is_ia32_Ld(irn) || is_ia32_St(irn) || is_ia32_Store8Bit(irn)) {
			left = get_irn_n(irn, 0);

			if (is_ia32_Lea(left)) {
				DBG((mod, LEVEL_1, "\nmerging %+F into %+F\n", left, irn));

				/* get the AM attributes from the LEA */
				add_ia32_am_offs(irn, get_ia32_am_offs(left));
				set_ia32_am_scale(irn, get_ia32_am_scale(left));
				set_ia32_am_flavour(irn, get_ia32_am_flavour(left));

				set_ia32_am_sc(irn, get_ia32_am_sc(left));
				if (is_ia32_am_sc_sign(left))
					set_ia32_am_sc_sign(irn);

				set_ia32_op_type(irn, is_ia32_Ld(irn) ? ia32_AddrModeS : ia32_AddrModeD);

				/* set base and index */
				set_irn_n(irn, 0, get_irn_n(left, 0));
				set_irn_n(irn, 1, get_irn_n(left, 1));

				/* clear remat flag */
				set_ia32_flags(irn, get_ia32_flags(irn) & ~arch_irn_flags_rematerializable);
			}
		}
		/* check if the node is an address mode candidate */
		else if (is_candidate(block, irn, 0)) {
			DBG((mod, LEVEL_1, "\tfound address mode candidate %+F ... ", irn));

			left  = get_irn_n(irn, 2);
			if (get_irn_arity(irn) == 4) {
				/* it's an "unary" operation */
				right = left;
			}
			else {
				right = get_irn_n(irn, 3);
			}

			/* normalize commutative ops */
			if (node_is_ia32_comm(irn)) {
				/* Assure that right operand is always a Load if there is one    */
				/* because non-commutative ops can only use Dest AM if the right */
				/* operand is a load, so we only need to check right operand.    */
				if (pred_is_specific_nodeblock(block, left, is_ia32_Ld))
				{
					set_irn_n(irn, 2, right);
					set_irn_n(irn, 3, left);

					temp  = left;
					left  = right;
					right = temp;
				}
			}

			/* check for Store -> op -> Load */

			/* Store -> op -> Load optimization is only possible if supported by op */
			/* and if right operand is a Load                                       */
			if ((get_ia32_am_support(irn) & ia32_am_Dest) &&
				 pred_is_specific_nodeblock(block, right, is_ia32_Ld))
			{

				/* An address mode capable op always has a result Proj.                  */
				/* If this Proj is used by more than one other node, we don't need to    */
				/* check further, otherwise we check for Store and remember the address, */
				/* the Store points to. */

				succ = get_res_proj(irn);
				assert(succ && "Couldn't find result proj");

				addr_b = NULL;
				addr_i = NULL;
				store  = NULL;

				/* now check for users and Store */
				if (ia32_get_irn_n_edges(succ) == 1) {
					succ = get_edge_src_irn(get_irn_out_edge_first(succ));

					if (is_ia32_fStore(succ) || is_ia32_Store(succ)) {
						store  = succ;
						addr_b = get_irn_n(store, 0);

						/* Could be that the Store is connected to the address    */
						/* calculating LEA while the Load is already transformed. */
						if (is_ia32_Lea(addr_b)) {
							succ   = addr_b;
							addr_b = get_irn_n(succ, 0);
							addr_i = get_irn_n(succ, 1);
						}
						else {
							addr_i = noreg_gp;
						}
					}
				}

				if (store) {
					/* we found a Store as single user: Now check for Load */

					/* Extra check for commutative ops with two Loads */
					/* -> put the interesting Load right              */
					if (node_is_ia32_comm(irn) &&
						pred_is_specific_nodeblock(block, left, is_ia32_Ld))
					{
						if ((addr_b == get_irn_n(get_Proj_pred(left), 0)) &&
							(addr_i == get_irn_n(get_Proj_pred(left), 1)))
						{
							/* We exchange left and right, so it's easier to kill     */
							/* the correct Load later and to handle unary operations. */
							set_irn_n(irn, 2, right);
							set_irn_n(irn, 3, left);

							temp  = left;
							left  = right;
							right = temp;
						}
					}

					/* skip the Proj for easier access */
					load = get_Proj_pred(right);

					/* Compare Load and Store address */
					if (load_store_addr_is_equal(load, store, addr_b, addr_i)) {
						/* Right Load is from same address, so we can */
						/* disconnect the Load and Store here        */

						/* set new base, index and attributes */
						set_irn_n(irn, 0, addr_b);
						set_irn_n(irn, 1, addr_i);
						add_ia32_am_offs(irn, get_ia32_am_offs(load));
						set_ia32_am_scale(irn, get_ia32_am_scale(load));
						set_ia32_am_flavour(irn, get_ia32_am_flavour(load));
						set_ia32_op_type(irn, ia32_AddrModeD);
						set_ia32_frame_ent(irn, get_ia32_frame_ent(load));
						set_ia32_ls_mode(irn, get_ia32_ls_mode(load));

						set_ia32_am_sc(irn, get_ia32_am_sc(load));
						if (is_ia32_am_sc_sign(load))
							set_ia32_am_sc_sign(irn);

						if (is_ia32_use_frame(load))
							set_ia32_use_frame(irn);

						/* connect to Load memory and disconnect Load */
						if (get_irn_arity(irn) == 5) {
							/* binary AMop */
							set_irn_n(irn, 4, get_irn_n(load, 2));
							set_irn_n(irn, 3, noreg_gp);
						}
						else {
							/* unary AMop */
							set_irn_n(irn, 3, get_irn_n(load, 2));
							set_irn_n(irn, 2, noreg_gp);
						}

						/* connect the memory Proj of the Store to the op */
						mem_proj = get_mem_proj(store);
						set_Proj_pred(mem_proj, irn);
						set_Proj_proj(mem_proj, 1);

						/* clear remat flag */
						set_ia32_flags(irn, get_ia32_flags(irn) & ~arch_irn_flags_rematerializable);

						DB((mod, LEVEL_1, "merged with %+F and %+F into dest AM\n", load, store));
					}
				} /* if (store) */
				else if (get_ia32_am_support(irn) & ia32_am_Source) {
					/* There was no store, check if we still can optimize for source address mode */
					check_am_src = 1;
				}
			} /* if (support AM Dest) */
			else if (get_ia32_am_support(irn) & ia32_am_Source) {
				/* op doesn't support am AM Dest -> check for AM Source */
				check_am_src = 1;
			}

			/* normalize commutative ops */
			if (node_is_ia32_comm(irn)) {
				/* Assure that left operand is always a Load if there is one */
				/* because non-commutative ops can only use Source AM if the */
				/* left operand is a Load, so we only need to check the left */
				/* operand afterwards.                                       */
				if (pred_is_specific_nodeblock(block, right, is_ia32_Ld))	{
					set_irn_n(irn, 2, right);
					set_irn_n(irn, 3, left);

					temp  = left;
					left  = right;
					right = temp;
				}
			}

			/* optimize op -> Load iff Load is only used by this op   */
			/* and left operand is a Load which only used by this irn */
			if (check_am_src                                        &&
				pred_is_specific_nodeblock(block, left, is_ia32_Ld) &&
				(ia32_get_irn_n_edges(left) == 1))
			{
				left = get_Proj_pred(left);

				addr_b = get_irn_n(left, 0);
				addr_i = get_irn_n(left, 1);

				/* set new base, index and attributes */
				set_irn_n(irn, 0, addr_b);
				set_irn_n(irn, 1, addr_i);
				add_ia32_am_offs(irn, get_ia32_am_offs(left));
				set_ia32_am_scale(irn, get_ia32_am_scale(left));
				set_ia32_am_flavour(irn, get_ia32_am_flavour(left));
				set_ia32_op_type(irn, ia32_AddrModeS);
				set_ia32_frame_ent(irn, get_ia32_frame_ent(left));
				set_ia32_ls_mode(irn, get_ia32_ls_mode(left));

				set_ia32_am_sc(irn, get_ia32_am_sc(left));
				if (is_ia32_am_sc_sign(left))
					set_ia32_am_sc_sign(irn);

				/* clear remat flag */
				set_ia32_flags(irn, get_ia32_flags(irn) & ~arch_irn_flags_rematerializable);

				if (is_ia32_use_frame(left))
					set_ia32_use_frame(irn);

				/* connect to Load memory */
				if (get_irn_arity(irn) == 5) {
					/* binary AMop */
					set_irn_n(irn, 4, get_irn_n(left, 2));
				}
				else {
					/* unary AMop */
					set_irn_n(irn, 3, get_irn_n(left, 2));
				}

				/* disconnect from Load */
				set_irn_n(irn, 2, noreg_gp);

				/* If Load has a memory Proj, connect it to the op */
				mem_proj = get_mem_proj(left);
				if (mem_proj) {
					set_Proj_pred(mem_proj, irn);
					set_Proj_proj(mem_proj, 1);
				}

				DB((mod, LEVEL_1, "merged with %+F into source AM\n", left));
			}
		}
	}
}
