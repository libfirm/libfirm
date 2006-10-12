/**
 * Project:     libFIRM
 * File name:   ir/be/ia32/ia32_optimize.c
 * Purpose:     Implements several optimizations for IA32
 * Author:      Christian Wuerdig
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

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
#include "irgwalk.h"
#include "height.h"
#include "irbitset.h"

#include "../be_t.h"
#include "../beabi.h"
#include "../benode_t.h"
#include "../besched_t.h"

#include "ia32_new_nodes.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"     /* the generated interface (register type and class defenitions) */
#include "ia32_transform.h"
#include "ia32_dbg_stat.h"
#include "ia32_util.h"

#define AGGRESSIVE_AM

typedef enum {
	IA32_AM_CAND_NONE  = 0,
	IA32_AM_CAND_LEFT  = 1,
	IA32_AM_CAND_RIGHT = 2,
	IA32_AM_CAND_BOTH  = 3
} ia32_am_cand_t;

#undef is_NoMem
#define is_NoMem(irn) (get_irn_op(irn) == op_NoMem)

typedef int is_op_func_t(const ir_node *n);
typedef ir_node *load_func_t(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *base, ir_node *index, ir_node *mem);

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
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;
	ir_node  *cnst;

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (USE_SSE2(env->cg))
			cnst = new_rd_ia32_xConst(dbg, irg, block, mode);
		else
			cnst = new_rd_ia32_vfConst(dbg, irg, block, mode);
	}
	else
		cnst = new_rd_ia32_Const(dbg, irg, block, mode);

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
	pmap_entry *e = pmap_find(cg->isa->tv_ent, tv);
	entity *res;
	ir_graph *rem;

	if (! e) {
		ir_mode *mode = get_irn_mode(cnst);
		ir_type *tp = get_Const_type(cnst);
		if (tp == firm_unknown_type)
			tp = get_prim_type(cg->isa->types, mode);

		res = new_entity(get_glob_type(), unique_id(".LC%u"), tp);

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

		pmap_insert(cg->isa->tv_ent, tv, res);
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
	ir_node *cnst, *load;
	symconst_symbol sym;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;
	ir_node  *node  = env->irn;
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;

	if (mode_is_float(mode)) {
		FP_USED(env->cg);
		if (! USE_SSE2(env->cg)) {
			cnst_classify_t clss = classify_Const(node);

			if (clss == CNST_NULL)
				return new_rd_ia32_vfldz(dbg, irg, block, mode);
			else if (clss == CNST_ONE)
				return new_rd_ia32_vfld1(dbg, irg, block, mode);
		}
		sym.entity_p = get_entity_for_tv(env->cg, node);


		cnst      = new_rd_SymConst(dbg, irg, block, sym, symconst_addr_ent);
		load      = new_r_Load(irg, block, get_irg_no_mem(irg), cnst, mode);
		load      = new_r_Proj(irg, block, load, mode, pn_Load_res);
		env->irn  = cnst;
		env->mode = mode_P;
		cnst      = gen_SymConst(env);
		add_irn_dep(cnst, be_abi_get_start_barrier(env->cg->birg->abi));
		set_Load_ptr(get_Proj_pred(load), cnst);
		cnst      = load;
	}
	else {
		cnst = new_rd_ia32_Const(dbg, irg, block, get_irn_mode(node));
		add_irn_dep(cnst, be_abi_get_start_barrier(env->cg->birg->abi));
		set_ia32_Const_attr(cnst, node);
	}

	return cnst;
}

/**
 * Transforms (all) Const's into ia32_Const and places them in the
 * block where they are used (or in the cfg-pred Block in case of Phi's).
 * Additionally all reference nodes are changed into mode_Is nodes.
 * NOTE: irn must be a firm constant!
 */
static void ia32_transform_const(ir_node *irn, void *env) {
	ia32_code_gen_t      *cg   = env;
	ir_node              *cnst = NULL;
	ia32_transform_env_t tenv;

	tenv.cg   = cg;
	tenv.irg  = cg->irg;
	tenv.mode = get_irn_mode(irn);
	tenv.dbg  = get_irn_dbg_info(irn);
	tenv.irn  = irn;
	DEBUG_ONLY(tenv.mod = cg->mod;)

#if 1
	/* place const either in the smallest dominator of all its users or the original block */
	if (cg->opt & IA32_OPT_PLACECNST)
		tenv.block = node_users_smallest_common_dominator(irn, 1);
	else
		tenv.block = get_nodes_block(irn);
#else
	/* Actually, there is no real sense in placing     */
	/* the Consts in the successor of the start block. */
	{
		ir_node *afterstart = NULL;
		ir_node *startblock = get_irg_start_block(tenv.irg);
		const ir_edge_t *edge;

		foreach_block_succ(startblock, edge) {
			ir_node *block = get_edge_src_irn(edge);
			if (block != startblock) {
				afterstart = block;
				break;
			}
		}
		assert(afterstart != NULL);
		tenv.block = afterstart;
	}
#endif

	switch (get_irn_opcode(irn)) {
		case iro_Const:
			cnst = gen_Const(&tenv);
			break;
		case iro_SymConst:
			cnst = gen_SymConst(&tenv);
			break;
		default:
			assert(0 && "Wrong usage of ia32_transform_const!");
	}

	assert(cnst && "Could not create ia32 Const");

	/* set the new ia32 const */
	exchange(irn, cnst);
}

/**
 * Transform all firm consts and assure, we visit each const only once.
 */
static void ia32_place_consts_walker(ir_node *irn, void *env) {
	ia32_code_gen_t *cg = env;

	if (! is_Const(irn) && ! is_SymConst(irn))
		return;

	ia32_transform_const(irn, cg);
}

/**
 * Replace reference modes with mode_Iu and preserve store value modes.
 */
static void ia32_set_modes(ir_node *irn, void *env) {
	if (is_Block(irn))
		return;

	/* transform all reference nodes into mode_Iu nodes */
	if (mode_is_reference(get_irn_mode(irn))) {
		set_irn_mode(irn, mode_Iu);
	}
}

/**
 * Walks over the graph, transforms all firm consts into ia32 consts
 * and places them into the "best" block.
 * @param cg  The ia32 codegenerator object
 */
static void ia32_transform_all_firm_consts(ia32_code_gen_t *cg) {
	irg_walk_graph(cg->irg, NULL, ia32_place_consts_walker, cg);
}

/* Place all consts and change pointer arithmetics into unsigned integer arithmetics. */
void ia32_pre_transform_phase(ia32_code_gen_t *cg) {
	/*
		We need to transform the consts twice:
		- the psi condition tree transformer needs existing constants to be ia32 constants
		- the psi condition tree transformer inserts new firm constants which need to be transformed
	*/
	ia32_transform_all_firm_consts(cg);
	irg_walk_graph(cg->irg, ia32_set_modes, ia32_transform_psi_cond_tree, cg);
	ia32_transform_all_firm_consts(cg);
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
		if (get_irn_n(cand, i) != get_irn_n(irn, i)) {
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
		DBG_OPT_CJMP(irn);

		set_irn_op(irn, op_ia32_CJmpAM);

		DB((cg->mod, LEVEL_1, "%+F\n", irn));
	}
}

// only optimize up to 48 stores behind IncSPs
#define MAXPUSH_OPTIMIZE	48

/**
 * Tries to create pushs from IncSP,Store combinations
 */
static void ia32_create_Pushs(ir_node *irn, ia32_code_gen_t *cg) {
	int i;
	int offset;
	ir_node *node;
	ir_node *stores[MAXPUSH_OPTIMIZE];
	ir_node *block = get_nodes_block(irn);
	ir_graph *irg = cg->irg;
	ir_node *curr_sp;
	ir_mode *spmode = get_irn_mode(irn);

	memset(stores, 0, sizeof(stores));

	assert(be_is_IncSP(irn));

	offset = be_get_IncSP_offset(irn);
	if(offset < 4)
		return;

	/*
	 * We first walk the schedule after the IncSP node as long as we find
	 * suitable stores that could be transformed to a push.
	 * We save them into the stores array which is sorted by the frame offset/4
	 * attached to the node
	 */
	for(node = sched_next(irn); !sched_is_end(node); node = sched_next(node)) {
		const char *am_offs;
		ir_node *mem;
		int offset = -1;
		int n;
		int storeslot;

		// it has to be a store
		if(!is_ia32_Store(node))
			break;

		// it has to use our sp value
		if(get_irn_n(node, 0) != irn)
			continue;
		// store has to be attached to NoMem
		mem = get_irn_n(node, 3);
		if(!is_NoMem(mem)) {
			continue;
		}

		if( (get_ia32_am_flavour(node) & ia32_am_IS) != 0)
			break;

		am_offs = get_ia32_am_offs(node);
		if(am_offs == NULL) {
			offset = 0;
		} else {
			// the am_offs has to be of the form "+NUMBER"
			if(sscanf(am_offs, "+%d%n", &offset, &n) != 1 || am_offs[n] != '\0') {
				// we shouldn't have any cases in the compiler at the moment
				// that produce something different from esp+XX
				assert(0);
				break;
			}
		}

		storeslot = offset / 4;
		if(storeslot >= MAXPUSH_OPTIMIZE)
			continue;

		// storing into the same slot twice is bad (and shouldn't happen...)
		if(stores[storeslot] != NULL)
			break;

		// storing at half-slots is bad
		if(offset % 4 != 0)
			break;

		stores[storeslot] = node;
	}

	curr_sp = get_irn_n(irn, 0);

	// walk the stores in inverse order and create pushs for them
	i = (offset / 4) - 1;
	if(i >= MAXPUSH_OPTIMIZE) {
		i = MAXPUSH_OPTIMIZE - 1;
	}

	for( ; i >= 0; --i) {
		const ir_edge_t *edge, *next;
		const arch_register_t *spreg;
		ir_node *push;
		ir_node *val, *mem;
		ir_node *store = stores[i];
		ir_node *noreg = ia32_new_NoReg_gp(cg);

		if(store == NULL || is_Bad(store))
			break;

		val = get_irn_n(store, 2);
		mem = get_irn_n(store, 3);
		spreg = arch_get_irn_register(cg->arch_env, curr_sp);

		// create a push
		push = new_rd_ia32_Push(NULL, irg, block, noreg, noreg, val, curr_sp, mem);
		if(get_ia32_immop_type(store) != ia32_ImmNone) {
			copy_ia32_Immop_attr(push, store);
		}
		sched_add_before(irn, push);

		// create stackpointer proj
		curr_sp = new_r_Proj(irg, block, push, spmode, pn_ia32_Push_stack);
		arch_set_irn_register(cg->arch_env, curr_sp, spreg);
		sched_add_before(irn, curr_sp);

		// rewire memprojs of the store
		foreach_out_edge_safe(store, edge, next) {
			ir_node *succ = get_edge_src_irn(edge);

			assert(is_Proj(succ) && get_Proj_proj(succ) == pn_ia32_Store_M);
			set_irn_n(succ, 0, push);
		}

		// we can remove the store now
		set_irn_n(store, 0, new_Bad());
		set_irn_n(store, 1, new_Bad());
		set_irn_n(store, 2, new_Bad());
		set_irn_n(store, 3, new_Bad());
		sched_remove(store);

		offset -= 4;
	}

	be_set_IncSP_offset(irn, offset);

	// can we remove the IncSP now?
	if(offset == 0) {
		const ir_edge_t *edge, *next;

		foreach_out_edge_safe(irn, edge, next) {
			ir_node *arg = get_edge_src_irn(edge);
			int pos = get_edge_src_pos(edge);

			set_irn_n(arg, pos, curr_sp);
		}

		set_irn_n(irn, 0, new_Bad());
		sched_remove(irn);
	} else {
		set_irn_n(irn, 0, curr_sp);
	}
}

/**
 * Tries to optimize two following IncSP.
 */
static void ia32_optimize_IncSP(ir_node *irn, ia32_code_gen_t *cg) {
	ir_node *prev = be_get_IncSP_pred(irn);
	int real_uses = get_irn_n_edges(prev);

	if (be_is_IncSP(prev) && real_uses == 1) {
		/* first IncSP has only one IncSP user, kill the first one */
		int prev_offs = be_get_IncSP_offset(prev);
		int curr_offs = be_get_IncSP_offset(irn);

		be_set_IncSP_offset(prev, prev_offs + curr_offs);

		/* Omit the optimized IncSP */
		be_set_IncSP_pred(irn, be_get_IncSP_pred(prev));

		set_irn_n(prev, 0, new_Bad());
		sched_remove(prev);
	}
}

/**
 * Performs Peephole Optimizations.
 */
static void ia32_peephole_optimize_node(ir_node *irn, void *env) {
	ia32_code_gen_t *cg = env;

	/* AMD CPUs want explicit compare before conditional jump  */
	if (! ARCH_AMD(cg->opt_arch)) {
		if (is_ia32_TestJmp(irn))
			ia32_optimize_TestJmp(irn, cg);
		else if (is_ia32_CondJmp(irn))
			ia32_optimize_CondJmp(irn, cg);
	}

	if (be_is_IncSP(irn)) {
		// optimize_IncSP doesn't respect dependency edges yet...
		//ia32_optimize_IncSP(irn, cg);
		(void) ia32_optimize_IncSP;
		ia32_create_Pushs(irn, cg);
	}
}

void ia32_peephole_optimization(ir_graph *irg, ia32_code_gen_t *cg) {
	irg_walk_graph(irg, ia32_peephole_optimize_node, NULL, cg);
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

typedef struct {
	ia32_code_gen_t *cg;
	heights_t       *h;
} ia32_am_opt_env_t;

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
 * Checks if irn is a candidate for address calculation.
 *
 * - none of the operand must be a Load  within the same block OR
 * - all Loads must have more than one user                    OR
 * - the irn has a frame entity (it's a former FrameAddr)
 *
 * @param block   The block the Loads must/mustnot be in
 * @param irn     The irn to check
 * return 1 if irn is a candidate, 0 otherwise
 */
static int is_addr_candidate(const ir_node *block, const ir_node *irn) {
	ir_node *in, *left, *right;
	int      n, is_cand = 1;

	left  = get_irn_n(irn, 2);
	right = get_irn_n(irn, 3);

	in = left;

#ifndef AGGRESSIVE_AM
	if (pred_is_specific_nodeblock(block, in, is_ia32_Ld)) {
		n         = ia32_get_irn_n_edges(in);
		is_cand   = (n == 1) ? 0 : is_cand;  /* load with only one user: don't create LEA */
	}

	in = right;

	if (pred_is_specific_nodeblock(block, in, is_ia32_Ld)) {
		n         = ia32_get_irn_n_edges(in);
		is_cand   = (n == 1) ? 0 : is_cand;  /* load with only one user: don't create LEA */
	}
#else
	(void) n;
#endif

	is_cand = get_ia32_frame_ent(irn) ? 1 : is_cand;

	return is_cand;
}

/**
 * Checks if irn is a candidate for address mode.
 *
 * address mode (AM):
 * - at least one operand has to be a Load within the same block AND
 * - the load must not have other users than the irn             AND
 * - the irn must not have a frame entity set
 *
 * @param cg          The ia32 code generator
 * @param h           The height information of the irg
 * @param block       The block the Loads must/mustnot be in
 * @param irn         The irn to check
 * return 0 if irn is no candidate, 1 if left load can be used, 2 if right one, 3 for both
 */
static ia32_am_cand_t is_am_candidate(ia32_code_gen_t *cg, heights_t *h, const ir_node *block, ir_node *irn) {
	ir_node *in, *load, *other, *left, *right;
	int      is_cand = 0, cand;

	if (is_ia32_Ld(irn) || is_ia32_St(irn) || is_ia32_Store8Bit(irn) || is_ia32_vfild(irn) || is_ia32_vfist(irn) ||
		is_ia32_GetST0(irn) || is_ia32_SetST0(irn) || is_ia32_xStoreSimple(irn))
		return 0;

	left  = get_irn_n(irn, 2);
	right = get_irn_n(irn, 3);

	in = left;

	if (pred_is_specific_nodeblock(block, in, is_ia32_Ld)) {
#ifndef AGGRESSIVE_AM
		int n;
		n         = ia32_get_irn_n_edges(in);
		is_cand   = (n == 1) ? 1 : is_cand;  /* load with more than one user: no AM */
#else
		is_cand   = 1;
#endif

		load  = get_Proj_pred(in);
		other = right;

		/* 8bit Loads are not supported (for binary ops),
		 * they cannot be used with every register */
		if (get_irn_arity(irn) != 4 && get_mode_size_bits(get_ia32_ls_mode(load)) < 16) {
			assert(get_irn_arity(irn) == 5);
			is_cand = 0;
		}

		/* If there is a data dependency of other irn from load: cannot use AM */
		if (is_cand && get_nodes_block(other) == block) {
			other   = skip_Proj(other);
			is_cand = heights_reachable_in_block(h, other, load) ? 0 : is_cand;
			/* this could happen in loops */
			is_cand = heights_reachable_in_block(h, load, irn) ? 0 : is_cand;
		}
	}

	cand    = is_cand ? IA32_AM_CAND_LEFT : IA32_AM_CAND_NONE;
	in      = right;
	is_cand = 0;

	if (pred_is_specific_nodeblock(block, in, is_ia32_Ld)) {
#ifndef AGGRESSIVE_AM
		int n;
		n         = ia32_get_irn_n_edges(in);
		is_cand   = (n == 1) ? 1 : is_cand;  /* load with more than one user: no AM */
#endif

		load  = get_Proj_pred(in);
		other = left;

		/* 8bit Loads are not supported, they cannot be used with every register */
		if (get_mode_size_bits(get_ia32_ls_mode(load)) < 16)
			is_cand = 0;

		/* If there is a data dependency of other irn from load: cannot use load */
		if (is_cand && get_nodes_block(other) == block) {
			other   = skip_Proj(other);
			is_cand = heights_reachable_in_block(h, other, load) ? 0 : is_cand;
			/* this could happen in loops */
			is_cand = heights_reachable_in_block(h, load, irn) ? 0 : is_cand;
		}
	}

	cand = is_cand ? (cand | IA32_AM_CAND_RIGHT) : cand;

	/* check some special cases */
	if (USE_SSE2(cg) && is_ia32_Conv_I2FP(irn)) {
		/* SSE Conv I -> FP cvtsi2s(s|d) can only load 32 bit values */
		if (get_mode_size_bits(get_ia32_tgt_mode(irn)) != 32)
			cand = IA32_AM_CAND_NONE;
	}
	else if (is_ia32_Conv_I2I(irn)) {
		/* we cannot load an N bit value and implicitly convert it into an M bit value if N > M */
		if (get_mode_size_bits(get_ia32_src_mode(irn)) > get_mode_size_bits(get_ia32_tgt_mode(irn)))
			cand = IA32_AM_CAND_NONE;
	}

	/* if the irn has a frame entity: we do not use address mode */
	return get_ia32_frame_ent(irn) ? IA32_AM_CAND_NONE : cand;
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

typedef enum _ia32_take_lea_attr {
	IA32_LEA_ATTR_NONE  = 0,
	IA32_LEA_ATTR_BASE  = (1 << 0),
	IA32_LEA_ATTR_INDEX = (1 << 1),
	IA32_LEA_ATTR_OFFS  = (1 << 2),
	IA32_LEA_ATTR_SCALE = (1 << 3),
	IA32_LEA_ATTR_AMSC  = (1 << 4),
	IA32_LEA_ATTR_FENT  = (1 << 5)
} ia32_take_lea_attr;

/**
 * Decides if we have to keep the LEA operand or if we can assimilate it.
 */
static int do_new_lea(ir_node *irn, ir_node *base, ir_node *index, ir_node *lea,
		int have_am_sc, ia32_code_gen_t *cg)
{
	entity  *irn_ent  = get_ia32_frame_ent(irn);
	entity  *lea_ent  = get_ia32_frame_ent(lea);
	int      ret_val  = 0;
	int      is_noreg_base  = be_is_NoReg(cg, base);
	int      is_noreg_index = be_is_NoReg(cg, index);
	ia32_am_flavour_t am_flav = get_ia32_am_flavour(lea);

	/* If the Add and the LEA both have a different frame entity set: keep */
	if (irn_ent && lea_ent && (irn_ent != lea_ent))
		return IA32_LEA_ATTR_NONE;
	else if (! irn_ent && lea_ent)
		ret_val |= IA32_LEA_ATTR_FENT;

	/* If the Add and the LEA both have already an address mode symconst: keep */
	if (have_am_sc && get_ia32_am_sc(lea))
		return IA32_LEA_ATTR_NONE;
	else if (get_ia32_am_sc(lea))
		ret_val |= IA32_LEA_ATTR_AMSC;

	/* Check the different base-index combinations */

	if (! is_noreg_base && ! is_noreg_index) {
		/* Assimilate if base is the lea and the LEA is just a Base + Offset calculation */
		if ((base == lea) && ! (am_flav & ia32_I ? 1 : 0)) {
			if (am_flav & ia32_O)
				ret_val |= IA32_LEA_ATTR_OFFS;

			ret_val |= IA32_LEA_ATTR_BASE;
		}
		else
			return IA32_LEA_ATTR_NONE;
	}
	else if (! is_noreg_base && is_noreg_index) {
		/* Base is set but index not */
		if (base == lea) {
			/* Base points to LEA: assimilate everything */
			if (am_flav & ia32_O)
				ret_val |= IA32_LEA_ATTR_OFFS;
			if (am_flav & ia32_S)
				ret_val |= IA32_LEA_ATTR_SCALE;
			if (am_flav & ia32_I)
				ret_val |= IA32_LEA_ATTR_INDEX;

			ret_val |= IA32_LEA_ATTR_BASE;
		}
		else if (am_flav & ia32_B ? 0 : 1) {
			/* Base is not the LEA but the LEA is an index only calculation: assimilate */
			if (am_flav & ia32_O)
				ret_val |= IA32_LEA_ATTR_OFFS;
			if (am_flav & ia32_S)
				ret_val |= IA32_LEA_ATTR_SCALE;

			ret_val |= IA32_LEA_ATTR_INDEX;
		}
		else
			return IA32_LEA_ATTR_NONE;
	}
	else if (is_noreg_base && ! is_noreg_index) {
		/* Index is set but not base */
		if (index == lea) {
			/* Index points to LEA: assimilate everything */
			if (am_flav & ia32_O)
				ret_val |= IA32_LEA_ATTR_OFFS;
			if (am_flav & ia32_S)
				ret_val |= IA32_LEA_ATTR_SCALE;
			if (am_flav & ia32_B)
				ret_val |= IA32_LEA_ATTR_BASE;

			ret_val |= IA32_LEA_ATTR_INDEX;
		}
		else if (am_flav & ia32_I ? 0 : 1) {
			/* Index is not the LEA but the LEA is a base only calculation: assimilate */
			if (am_flav & ia32_O)
				ret_val |= IA32_LEA_ATTR_OFFS;
			if (am_flav & ia32_S)
				ret_val |= IA32_LEA_ATTR_SCALE;

			ret_val |= IA32_LEA_ATTR_BASE;
		}
		else
			return IA32_LEA_ATTR_NONE;
	}
	else {
		assert(0 && "There must have been set base or index");
	}

	return ret_val;
}

/**
 * Adds res before irn into schedule if irn was scheduled.
 * @param irn  The schedule point
 * @param res  The node to be scheduled
 */
static INLINE void try_add_to_sched(ir_node *irn, ir_node *res) {
	if (sched_is_scheduled(irn))
		sched_add_before(irn, res);
}

/**
 * Removes irn from schedule if it was scheduled. If irn is a mode_T node
 * all it's Projs are removed as well.
 * @param irn  The irn to be removed from schedule
 */
static INLINE void try_remove_from_sched(ir_node *irn) {
	int i, arity;

	if (sched_is_scheduled(irn)) {
		if (get_irn_mode(irn) == mode_T) {
			const ir_edge_t *edge;
			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (sched_is_scheduled(proj)) {
					set_irn_n(proj, 0, new_Bad());
					sched_remove(proj);
				}
			}
		}

		arity = get_irn_arity(irn);
		for(i = 0; i < arity; ++i) {
			set_irn_n(irn, i, new_Bad());
		}
		sched_remove(irn);
	}
}

/**
 * Folds Add or Sub to LEA if possible
 */
static ir_node *fold_addr(ia32_code_gen_t *cg, ir_node *irn, ir_node *noreg) {
	ir_graph   *irg        = get_irn_irg(irn);
	dbg_info   *dbg        = get_irn_dbg_info(irn);
	ir_node    *block      = get_nodes_block(irn);
	ir_node    *res        = irn;
	ir_node    *shift      = NULL;
	ir_node    *lea_o      = NULL;
	ir_node    *lea        = NULL;
	char       *offs       = NULL;
	const char *offs_cnst  = NULL;
	char       *offs_lea   = NULL;
	int         scale      = 0;
	int         isadd      = 0;
	int         dolea      = 0;
	int         have_am_sc = 0;
	int         am_sc_sign = 0;
	ident      *am_sc      = NULL;
	entity     *lea_ent    = NULL;
	ir_node    *left, *right, *temp;
	ir_node    *base, *index;
	int consumed_left_shift;
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
	temp = be_is_NoReg(cg, right) ? left : right;

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
		lea_o      = temp;

		if (temp == base)
			base = noreg;
		else if (temp == right)
			right = noreg;
	}

	if (isadd) {
		/* default for add -> make right operand to index */
		index               = right;
		dolea               = 1;
		consumed_left_shift = -1;

		DBG((mod, LEVEL_1, "\tgot LEA candidate with index %+F\n", index));

		/* determine the operand which needs to be checked */
		temp = left;
		if (is_ia32_Lea(left)) {
			temp = right;
			consumed_left_shift = 0;
		}

		/* check for SHL 1,2,3 */
		if (pred_is_specific_node(temp, is_ia32_Shl)) {
			temp  = get_Proj_pred(temp);
			shift = temp;

			if (get_ia32_Immop_tarval(temp)) {
				scale = get_tarval_long(get_ia32_Immop_tarval(temp));

				if (scale <= 3) {
					index               = get_irn_n(temp, 2);
					consumed_left_shift = consumed_left_shift < 0 ? 1 : 0;

					DBG((mod, LEVEL_1, "\tgot scaled index %+F\n", index));
				}
				else {
					scale = 0;
					shift = NULL;
				}
			}
		}

		/* fix base */
		if (! be_is_NoReg(cg, index)) {
			/* if we have index, but left == right -> no base */
			if (left == right) {
				base = noreg;
			}
			else if (consumed_left_shift == 1) {
				/* -> base is right operand  */
				base = (right == lea_o) ? noreg : right;
			}
		}
	}

	/* Try to assimilate a LEA as left operand */
	if (is_ia32_Lea(left) && (get_ia32_am_flavour(left) != ia32_am_O)) {
		/* check if we can assimilate the LEA */
		int take_attr = do_new_lea(irn, base, index, left, have_am_sc, cg);

		if (take_attr == IA32_LEA_ATTR_NONE) {
			DBG((mod, LEVEL_1, "\tleave old LEA, creating new one\n"));
		}
		else {
			DBG((mod, LEVEL_1, "\tgot LEA as left operand ... assimilating\n"));
			lea = left; /* for statistics */

			if (take_attr & IA32_LEA_ATTR_OFFS)
				offs = get_ia32_am_offs(left);

			if (take_attr & IA32_LEA_ATTR_AMSC) {
				am_sc      = get_ia32_am_sc(left);
				have_am_sc = 1;
				am_sc_sign = is_ia32_am_sc_sign(left);
			}

			if (take_attr & IA32_LEA_ATTR_SCALE)
				scale = get_ia32_am_scale(left);

			if (take_attr & IA32_LEA_ATTR_BASE)
				base = get_irn_n(left, 0);

			if (take_attr & IA32_LEA_ATTR_INDEX)
				index = get_irn_n(left, 1);

			if (take_attr & IA32_LEA_ATTR_FENT)
				lea_ent = get_ia32_frame_ent(left);
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
		if (lea_ent)
			set_ia32_frame_ent(res, lea_ent);
		else
			set_ia32_frame_ent(res, get_ia32_frame_ent(irn));

		if (get_ia32_frame_ent(res))
			set_ia32_use_frame(res);

		/* set scale */
		set_ia32_am_scale(res, scale);

		am_flav = ia32_am_N;
		/* determine new am flavour */
		if (offs || offs_cnst || offs_lea || have_am_sc) {
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

		SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(cg, irn));

		DBG((mod, LEVEL_1, "\tLEA [%+F + %+F * %d + %s]\n", base, index, scale, get_ia32_am_offs(res)));

		/* we will exchange it, report here before the Proj is created */
		if (shift && lea && lea_o) {
			try_remove_from_sched(shift);
			try_remove_from_sched(lea);
			try_remove_from_sched(lea_o);
			DBG_OPT_LEA4(irn, lea_o, lea, shift, res);
		}
		else if (shift && lea) {
			try_remove_from_sched(shift);
			try_remove_from_sched(lea);
			DBG_OPT_LEA3(irn, lea, shift, res);
		}
		else if (shift && lea_o) {
			try_remove_from_sched(shift);
			try_remove_from_sched(lea_o);
			DBG_OPT_LEA3(irn, lea_o, shift, res);
		}
		else if (lea && lea_o) {
			try_remove_from_sched(lea);
			try_remove_from_sched(lea_o);
			DBG_OPT_LEA3(irn, lea_o, lea, res);
		}
		else if (shift) {
			try_remove_from_sched(shift);
			DBG_OPT_LEA2(irn, shift, res);
		}
		else if (lea) {
			try_remove_from_sched(lea);
			DBG_OPT_LEA2(irn, lea, res);
		}
		else if (lea_o) {
			try_remove_from_sched(lea_o);
			DBG_OPT_LEA2(irn, lea_o, res);
		}
		else
			DBG_OPT_LEA1(irn, res);

		/* get the result Proj of the Add/Sub */
		try_add_to_sched(irn, res);
		try_remove_from_sched(irn);
		irn = ia32_get_res_proj(irn);

		assert(irn && "Couldn't find result proj");

		/* exchange the old op with the new LEA */
		exchange(irn, res);
	}

	return res;
}


/**
 * Merges a Load/Store node with a LEA.
 * @param irn The Load/Store node
 * @param lea The LEA
 */
static void merge_loadstore_lea(ir_node *irn, ir_node *lea) {
	entity *irn_ent = get_ia32_frame_ent(irn);
	entity *lea_ent = get_ia32_frame_ent(lea);

	/* If the irn and the LEA both have a different frame entity set: do not merge */
	if (irn_ent && lea_ent && (irn_ent != lea_ent))
		return;
	else if (! irn_ent && lea_ent) {
		set_ia32_frame_ent(irn, lea_ent);
		set_ia32_use_frame(irn);
	}

	/* get the AM attributes from the LEA */
	add_ia32_am_offs(irn, get_ia32_am_offs(lea));
	set_ia32_am_scale(irn, get_ia32_am_scale(lea));
	set_ia32_am_flavour(irn, get_ia32_am_flavour(lea));

	set_ia32_am_sc(irn, get_ia32_am_sc(lea));
	if (is_ia32_am_sc_sign(lea))
		set_ia32_am_sc_sign(irn);

	set_ia32_op_type(irn, is_ia32_Ld(irn) ? ia32_AddrModeS : ia32_AddrModeD);

	/* set base and index */
	set_irn_n(irn, 0, get_irn_n(lea, 0));
	set_irn_n(irn, 1, get_irn_n(lea, 1));

	try_remove_from_sched(lea);

	/* clear remat flag */
	set_ia32_flags(irn, get_ia32_flags(irn) & ~arch_irn_flags_rematerializable);

	if (is_ia32_Ld(irn))
		DBG_OPT_LOAD_LEA(lea, irn);
	else
		DBG_OPT_STORE_LEA(lea, irn);

}

/**
 * Sets new_right index of irn to right and new_left index to left.
 * Also exchange left and right
 */
static void exchange_left_right(ir_node *irn, ir_node **left, ir_node **right, int new_left, int new_right) {
	ir_node *temp;

	set_irn_n(irn, new_right, *right);
	set_irn_n(irn, new_left, *left);

	temp   = *left;
	*left  = *right;
	*right = temp;

	/* this is only needed for Compares, but currently ALL nodes
	 * have this attribute :-) */
	set_ia32_pncode(irn, get_inversed_pnc(get_ia32_pncode(irn)));
}

/**
 * Performs address calculation optimization (create LEAs if possible)
 */
static void optimize_lea(ir_node *irn, void *env) {
	ia32_code_gen_t *cg  = env;
	ir_node         *block, *noreg_gp, *left, *right;

	if (! is_ia32_irn(irn))
		return;

	/* Following cases can occur:                                  */
	/* - Sub (l, imm) -> LEA [base - offset]                       */
	/* - Sub (l, r == LEA with ia32_am_O)   -> LEA [base - offset] */
	/* - Add (l, imm) -> LEA [base + offset]                       */
	/* - Add (l, r == LEA with ia32_am_O)  -> LEA [base + offset]  */
	/* - Add (l == LEA with ia32_am_O, r)  -> LEA [base + offset]  */
	/* - Add (l, r) -> LEA [base + index * scale]                  */
	/*              with scale > 1 iff l/r == shl (1,2,3)          */

	if (is_ia32_Sub(irn) || is_ia32_Add(irn)) {
		left     = get_irn_n(irn, 2);
		right    = get_irn_n(irn, 3);
		block    = get_nodes_block(irn);
		noreg_gp = ia32_new_NoReg_gp(cg);

	    /* Do not try to create a LEA if one of the operands is a Load. */
		/* check is irn is a candidate for address calculation */
		if (is_addr_candidate(block, irn)) {
			ir_node *res;

			DBG((cg->mod, LEVEL_1, "\tfound address calculation candidate %+F ... ", irn));
			res = fold_addr(cg, irn, noreg_gp);

			if (res != irn)
				DB((cg->mod, LEVEL_1, "transformed into %+F\n", res));
			else
				DB((cg->mod, LEVEL_1, "not transformed\n"));
		}
	}
	else if (is_ia32_Ld(irn) || is_ia32_St(irn) || is_ia32_Store8Bit(irn)) {
		/* - Load  -> LEA into Load  } TODO: If the LEA is used by more than one Load/Store */
		/* - Store -> LEA into Store }       it might be better to keep the LEA             */
		left = get_irn_n(irn, 0);

		if (is_ia32_Lea(left)) {
			const ir_edge_t *edge, *ne;
			ir_node *src;

			/* merge all Loads/Stores connected to this LEA with the LEA */
			foreach_out_edge_safe(left, edge, ne) {
				src = get_edge_src_irn(edge);

				if (src && (get_edge_src_pos(edge) == 0) && (is_ia32_Ld(src) || is_ia32_St(src) || is_ia32_Store8Bit(src))) {
					DBG((cg->mod, LEVEL_1, "\nmerging %+F into %+F\n", left, irn));
					if (! is_ia32_got_lea(src))
						merge_loadstore_lea(src, left);
					set_ia32_got_lea(src);
				}
			}
		}
	}
}

/**
 * Checks for address mode patterns and performs the
 * necessary transformations.
 * This function is called by a walker.
 */
static void optimize_am(ir_node *irn, void *env) {
	ia32_am_opt_env_t *am_opt_env = env;
	ia32_code_gen_t   *cg         = am_opt_env->cg;
	heights_t         *h          = am_opt_env->h;
	ir_node           *block, *left, *right;
	ir_node           *store, *load, *mem_proj;
	ir_node           *succ, *addr_b, *addr_i;
	int               check_am_src          = 0;
	int               need_exchange_on_fail = 0;
	DEBUG_ONLY(firm_dbg_module_t *mod = cg->mod;)

	if (! is_ia32_irn(irn) || is_ia32_Ld(irn) || is_ia32_St(irn) || is_ia32_Store8Bit(irn))
		return;

	block = get_nodes_block(irn);

	DBG((mod, LEVEL_1, "checking for AM\n"));

	/* fold following patterns:                                                         */
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

	if ((get_ia32_am_support(irn) != ia32_am_None) && ! is_ia32_Lea(irn)) {
		ia32_am_cand_t cand      = is_am_candidate(cg, h, block, irn);
		ia32_am_cand_t orig_cand = cand;

		/* cand == 1: load is left;   cand == 2: load is right; */

		if (cand == IA32_AM_CAND_NONE)
			return;

		DBG((mod, LEVEL_1, "\tfound address mode candidate %+F ... ", irn));

		left  = get_irn_n(irn, 2);
		if (get_irn_arity(irn) == 4) {
			/* it's an "unary" operation */
			right = left;
			cand = IA32_AM_CAND_BOTH;
		}
		else {
			right = get_irn_n(irn, 3);
		}

		/* normalize commutative ops */
		if (node_is_ia32_comm(irn) && (cand == IA32_AM_CAND_RIGHT)) {

			/* Assure that left operand is always a Load if there is one    */
			/* because non-commutative ops can only use Dest AM if the left */
			/* operand is a load, so we only need to check left operand.    */

			exchange_left_right(irn, &left, &right, 3, 2);
			need_exchange_on_fail = 1;

			/* now: load is right */
			cand = IA32_AM_CAND_LEFT;
		}

		/* check for Store -> op -> Load */

		/* Store -> op -> Load optimization is only possible if supported by op */
		/* and if right operand is a Load                                       */
		if ((get_ia32_am_support(irn) & ia32_am_Dest) && (cand & IA32_AM_CAND_LEFT))
		{
			/* An address mode capable op always has a result Proj.                  */
			/* If this Proj is used by more than one other node, we don't need to    */
			/* check further, otherwise we check for Store and remember the address, */
			/* the Store points to. */

			succ = ia32_get_res_proj(irn);
			assert(succ && "Couldn't find result proj");

			addr_b = NULL;
			addr_i = NULL;
			store  = NULL;

			/* now check for users and Store */
			if (ia32_get_irn_n_edges(succ) == 1) {
				succ = get_edge_src_irn(get_irn_out_edge_first(succ));

				if (is_ia32_xStore(succ) || is_ia32_Store(succ)) {
					store  = succ;
					addr_b = get_irn_n(store, 0);
					addr_i = get_irn_n(store, 1);
				}
			}

			if (store) {
				/* we found a Store as single user: Now check for Load */

				/* skip the Proj for easier access */
				load = is_Proj(right) ? (is_ia32_Load(get_Proj_pred(right)) ? get_Proj_pred(right) : NULL) : NULL;

				/* Extra check for commutative ops with two Loads */
				/* -> put the interesting Load left               */
				if (load && node_is_ia32_comm(irn) && (cand == IA32_AM_CAND_BOTH)) {
					if (load_store_addr_is_equal(load, store, addr_b, addr_i)) {
						/* We exchange left and right, so it's easier to kill     */
						/* the correct Load later and to handle unary operations. */
						exchange_left_right(irn, &left, &right, 3, 2);
						need_exchange_on_fail ^= 1;
					}
				}

				/* skip the Proj for easier access */
				load = get_Proj_pred(left);

				/* Compare Load and Store address */
				if (load_store_addr_is_equal(load, store, addr_b, addr_i)) {
					/* Left Load is from same address, so we can */
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
						set_irn_n(irn, 2, ia32_get_admissible_noreg(cg, irn, 2));
					}
					else {
						/* unary AMop */
						set_irn_n(irn, 3, get_irn_n(load, 2));
						set_irn_n(irn, 2, ia32_get_admissible_noreg(cg, irn, 2));
					}

					/* connect the memory Proj of the Store to the op */
					mem_proj = ia32_get_proj_for_mode(store, mode_M);
					set_Proj_pred(mem_proj, irn);
					set_Proj_proj(mem_proj, 1);

					/* clear remat flag */
					set_ia32_flags(irn, get_ia32_flags(irn) & ~arch_irn_flags_rematerializable);

					try_remove_from_sched(load);
					try_remove_from_sched(store);
					DBG_OPT_AM_D(load, store, irn);

					DB((mod, LEVEL_1, "merged with %+F and %+F into dest AM\n", load, store));

					need_exchange_on_fail = 0;
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

		/* was exchanged but optimize failed: exchange back */
		if (need_exchange_on_fail) {
			exchange_left_right(irn, &left, &right, 3, 2);
			cand = orig_cand;
		}

		need_exchange_on_fail = 0;

		/* normalize commutative ops */
		if (check_am_src && node_is_ia32_comm(irn) && (cand == IA32_AM_CAND_LEFT)) {

			/* Assure that right operand is always a Load if there is one  */
			/* because non-commutative ops can only use Source AM if the   */
			/* right operand is a Load, so we only need to check the right */
			/* operand afterwards.                                         */

			exchange_left_right(irn, &left, &right, 3, 2);
			need_exchange_on_fail = 1;

			/* now: load is left */
			cand = IA32_AM_CAND_RIGHT;
		}

		/* optimize op -> Load iff Load is only used by this op    */
		/* and right operand is a Load which only used by this irn */
		if (check_am_src                &&
			(cand & IA32_AM_CAND_RIGHT) &&
			(ia32_get_irn_n_edges(right) == 1))
		{
			ir_node *load = get_Proj_pred(right);

			addr_b = get_irn_n(load, 0);
			addr_i = get_irn_n(load, 1);

			/* set new base, index and attributes */
			set_irn_n(irn, 0, addr_b);
			set_irn_n(irn, 1, addr_i);
			add_ia32_am_offs(irn, get_ia32_am_offs(load));
			set_ia32_am_scale(irn, get_ia32_am_scale(load));
			set_ia32_am_flavour(irn, get_ia32_am_flavour(load));
			set_ia32_op_type(irn, ia32_AddrModeS);
			set_ia32_frame_ent(irn, get_ia32_frame_ent(load));
			set_ia32_ls_mode(irn, get_ia32_ls_mode(load));

			set_ia32_am_sc(irn, get_ia32_am_sc(load));
			if (is_ia32_am_sc_sign(load))
				set_ia32_am_sc_sign(irn);

			/* clear remat flag */
			set_ia32_flags(irn, get_ia32_flags(irn) & ~arch_irn_flags_rematerializable);

			if (is_ia32_use_frame(load))
				set_ia32_use_frame(irn);

			/* connect to Load memory and disconnect Load */
			if (get_irn_arity(irn) == 5) {
				/* binary AMop */
				set_irn_n(irn, 4, get_irn_n(load, 2));
				set_irn_n(irn, 3, ia32_get_admissible_noreg(cg, irn, 3));
			} else {
				assert(get_irn_arity(irn) == 4);
				/* unary AMop */
				set_irn_n(irn, 3, get_irn_n(load, 2));
				set_irn_n(irn, 2, ia32_get_admissible_noreg(cg, irn, 2));
			}

			/* this is only needed for Compares, but currently ALL nodes
			 * have this attribute :-) */
			set_ia32_pncode(irn, get_inversed_pnc(get_ia32_pncode(irn)));

			DBG_OPT_AM_S(load, irn);

			/* If Load has a memory Proj, connect it to the op */
			mem_proj = ia32_get_proj_for_mode(load, mode_M);
			if (mem_proj) {
				set_Proj_pred(mem_proj, irn);
				set_Proj_proj(mem_proj, 1);
			}

			try_remove_from_sched(load);

			DB((mod, LEVEL_1, "merged with %+F into source AM\n", load));
		}
		else {
			/* was exchanged but optimize failed: exchange back */
			if (need_exchange_on_fail)
				exchange_left_right(irn, &left, &right, 3, 2);
		}
	}
}

/**
 * Performs address mode optimization.
 */
void ia32_optimize_addressmode(ia32_code_gen_t *cg) {
	/* if we are supposed to do AM or LEA optimization: recalculate edges */
	if (cg->opt & (IA32_OPT_DOAM | IA32_OPT_LEA)) {
		edges_deactivate(cg->irg);
		edges_activate(cg->irg);
	}
	else {
		/* no optimizations at all */
		return;
	}

	/* beware: we cannot optimize LEA and AM in one run because */
	/*         LEA optimization adds new nodes to the irg which */
	/*         invalidates the phase data                       */

	if (cg->opt & IA32_OPT_LEA) {
		irg_walk_blkwise_graph(cg->irg, NULL, optimize_lea, cg);
	}

	if (cg->dump)
		be_dump(cg->irg, "-lea", dump_ir_block_graph_sched);

	if (cg->opt & IA32_OPT_DOAM) {
		/* we need height information for am optimization */
		heights_t *h = heights_new(cg->irg);
		ia32_am_opt_env_t env;

		env.cg = cg;
		env.h  = h;

		irg_walk_blkwise_graph(cg->irg, NULL, optimize_am, &env);

		heights_free(h);
	}
}
