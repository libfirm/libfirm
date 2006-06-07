/** vim: set sw=4 ts=4:
 * @file   bespillremat.c
 * @date   2006-04-06
 * @author Adam M. Szalkowski & Sebastian Hack
 *
 * ILP based spilling & rematerialization
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WITH_ILP

#include <math.h>

#include "hashptr.h"
#include "debug.h"
#include "obst.h"
#include "set.h"
#include "list.h"
#include "pmap.h"

#include "irprintf.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irloop_t.h"
#include "execution_frequency.h"
#include "phiclass.h"

#include <lpp/lpp.h>
#include <lpp/lpp_net.h>
#include <lpp/lpp_cplex.h>
//#include <lc_pset.h>

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch.h"
#include "benode_t.h"
#include "beutil.h"
#include "bespillappel.h"
#include "bespill.h"
#include "bessadestrsimple.h"

#include "bechordal_t.h"

#define BIGM 100000.0

#define DUMP_SOLUTION
#define DUMP_ILP

#define  SOLVE
#undef  SOLVE_LOCAL
#define LPP_SERVER "i44pc52"
#define LPP_SOLVER "cplex"

#define COST_LOAD      10
#define COST_STORE     50
#define COST_REMAT     1

#define ILP_UNDEF		-1

typedef struct _spill_ilp_t {
	const arch_register_class_t  *cls;
	const be_chordal_env_t       *chordal_env;
	spill_env_t                  *senv;
	lpp_t                        *lpp;
	struct obstack               *obst;
	DEBUG_ONLY(firm_dbg_module_t * dbg);
} spill_ilp_t;

typedef int ilp_var_t;
typedef int ilp_cst_t;

typedef struct _keyval_t {
	const void          *key;
	const void          *val;
} keyval_t;

typedef struct _spill_t {
	ir_node      *irn;
	ilp_var_t     reg_in;
	ilp_var_t     mem_in;
	ilp_var_t     reg_out;
	ilp_var_t     mem_out;
	ilp_var_t     spill;
} spill_t;

static INLINE int
has_reg_class(const spill_ilp_t * si, const ir_node * irn)
{
	return chordal_has_class(si->chordal_env, irn);
}

static int
cmp_spill(const void *a, const void *b, size_t size)
{
	const spill_t *p = a;
	const spill_t *q = b;

//	return !(p->irn == q->irn && p->bb == q->bb);
	return !(p->irn == q->irn);
}

static keyval_t *
set_find_keyval(set * set, void * key)
{
	keyval_t     query;

	query.key = key;
	return set_find(set, &query, sizeof(query), HASH_PTR(key));
}

static keyval_t *
set_insert_keyval(set * set, void * key, void * val)
{
	keyval_t     query;

	query.key = key;
	query.val = val;
	return set_insert(set, &query, sizeof(query), HASH_PTR(key));
}

static void *
set_next_keyval(set * set)
{
	keyval_t     *result;

	result = set_next(set);

	if(!result)
		return NULL;

	return result->key;
}

static void *
set_first_keyval(set * set)
{
	keyval_t     *result;

	result = set_first(set);

	if(!result)
		return NULL;

	return result->key;
}

#define pset_foreach(s,i) for((i)=pset_first((s)); (i); (i)=pset_next((s)))
#define set_foreach(s,i) for((i)=set_first((s)); (i); (i)=set_next((s)))
#define foreach_post_remat(s,i) for((i)=next_post_remat((s)); (i); (i)=next_post_remat((i)))
#define foreach_pre_remat(s,i) for((i)=next_pre_remat((s)); (i); (i)=next_pre_remat((i)))

static int
cmp_keyval(const void *a, const void *b, size_t size)
{
	const keyval_t *p = a;
	const keyval_t *q = b;

	return !(p->key == q->key);
}

static float
execution_frequency(ir_node * irn)
{
	if(is_Block(irn))
		return expf((float)get_loop_depth(get_irn_loop(irn)) * logf(10));
	else
		return expf((float)get_loop_depth(get_irn_loop(get_nodes_block(irn))) * logf(10));
}

/**
 * Checks, whether node and its operands have suitable reg classes
 */
static INLINE int
is_rematerializable(const spill_ilp_t * si, const ir_node * irn)
{
	int             i,
	                n;
	const arch_env_t *arch_env = si->chordal_env->birg->main_env->arch_env;
	int               remat = (arch_irn_get_flags(arch_env, irn) & arch_irn_flags_rematerializable) != 0;

#if 0
	if(!remat)
		ir_fprintf(stderr, "  Node %+F is not rematerializable\n", irn);
#endif

	for (i = 0, n = get_irn_arity(irn); i < n && remat; ++i) {
		ir_node        *op = get_irn_n(irn, i);
		remat &= has_reg_class(si, op);

		if(!remat)
			ir_fprintf(stderr, "  Argument %d (%+F) of Node %+F has wrong regclass\n", i, op, irn);
	}

	return remat;
}

static INLINE int
value_is_defined_before(const spill_ilp_t * si, const ir_node * pos, const ir_node * val)
{
	ir_node *block;
	ir_node *def_block = get_nodes_block(val);
	int      ret;

	if(val != pos)
		return 0;

	/* if pos is at end of a basic block */
	if(is_Block(pos)) {
		ret = (pos == def_block || block_dominates(def_block, pos));
		ir_fprintf(stderr, "(def(bb)=%d) ", ret);
		return ret;
	}

	/* else if this is a normal operation */
	block = get_nodes_block(pos);
	if(block == def_block) {
		ret = sched_comes_after(val, pos);
		ir_fprintf(stderr, "(def(same block)=%d) ",ret);
		return ret;
	}

	ret = block_dominates(def_block, block);
	ir_fprintf(stderr, "(def(other block)=%d) ", ret);
	return ret;
}


/**
 * Returns first non-Phi node of block @p bb
 */
static INLINE ir_node *
sched_block_first_nonphi(const ir_node * bb)
{
	return sched_skip((ir_node*)bb, 1, sched_skip_phi_predicator, NULL);
}

static int
sched_skip_proj_predicator(const ir_node * irn, void * data)
{
	return (is_Proj(irn));
}

/**
 * Returns next operation node (non-Proj) after @p irn
 * or the basic block of this node
 */
static INLINE ir_node *
sched_next_op(const ir_node * irn)
{
	ir_node *next = sched_next(irn);

	if(is_Block(next))
		return next;

	return sched_skip(next, 1, sched_skip_proj_predicator, NULL);
}

/**
 * Returns previous operation node (non-Proj) before @p irn
 * or the basic block of this node
 */
static INLINE ir_node *
sched_prev_op(const ir_node * irn)
{
	ir_node *prev = sched_prev(irn);

	if(is_Block(prev))
		return prev;

	return sched_skip(prev, 0, sched_skip_proj_predicator, NULL);
}


/**
 * Preparation of blocks' ends for Luke Blockwalker(tm)(R)
 */
static void
luke_endwalker(ir_node * bb, void * data)
{
	spill_ilp_t    *si = (spill_ilp_t*)data;
	ir_node        *irn;
	irn_live_t     *li;
	pset           *live;
	char            buf[256];
	const int       n_regs = arch_register_class_n_regs(si->cls);
	ilp_cst_t       cst;


	live = pset_new_ptr_default();

	live_foreach(bb, li) {
		ir_node        *irn = (ir_node *) li->irn;
		if (live_is_end(li) && has_reg_class(si, irn)) {
			pset_insert_ptr(live, irn);
		}
	}

#if 0
	ir_snprintf(buf, sizeof(buf), "check_end_%N", bb);
	cst = lpp_add_cst(si->lpp, buf, lpp_less, n_regs);
	spill_bb->ilp = new_set(cmp_spill, 16);
	spill->reg_out = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);
	lpp_set_factor_fast(si->lpp, cst, spill->reg_out, 1.0);
#endif

	del_pset(live);
}


/**
 * Walk all irg blocks and emit this ILP
 */
static void
luke_blockwalker(ir_node * bb, void * data)
{
	spill_ilp_t    *si = (spill_ilp_t*)data;
	ir_node        *irn;
	irn_live_t     *li;
	pset           *live;
	char            buf[256];
	const int       n_regs = arch_register_class_n_regs(si->cls);
	ilp_cst_t       cst;
	int             i;
	ir_node        *tmp;


	live = pset_new_ptr_default();

	del_pset(live);
}

static INLINE int
is_zero(double x)
{
	return fabs(x) < 0.00001;
}

#if 0
static int
is_spilled(const spill_ilp_t * si, const live_range_t * lr)
{
	return !is_zero(lpp_get_var_sol(si->lpp, lr->in_mem_var));
}
#endif

void
be_spill_appel(const be_chordal_env_t * chordal_env)
{
	char            problem_name[256];
	struct obstack  obst;
	spill_ilp_t     si;

	ir_snprintf(problem_name, sizeof(problem_name), "%F_%s", chordal_env->irg, chordal_env->cls->name);

	obstack_init(&obst);
	si.chordal_env = chordal_env;
	si.obst = &obst;
	si.senv = be_new_spill_env(chordal_env);
	si.cls = chordal_env->cls;
	si.lpp = new_lpp(problem_name, lpp_minimize);
	FIRM_DBG_REGISTER(si.dbg, "firm.be.ra.spillappel");

	firm_dbg_set_mask(si.dbg,0xFF);

	set_irg_link(chordal_env->irg, &si);

	ir_fprintf(stderr, "\nProcessing %s\n\n", problem_name);

	ir_fprintf(stderr, "Initiating SSA destruction\n");
	be_ssa_destr_simple(chordal_env->irg, chordal_env->birg->main_env->arch_env);
	be_dump(chordal_env->irg, "-ssadestr-appel", dump_ir_block_graph_sched);

	/* build the ILP */

	DBG((si.dbg, LEVEL_1, "\tBuilding ILP\n"));
	DBG((si.dbg, LEVEL_2, "\t endwalker\n"));
	irg_block_walk_graph(chordal_env->irg, luke_endwalker, NULL, &si);

	DBG((si.dbg, LEVEL_2, "\t blockwalker\n"));
	irg_block_walk_graph(chordal_env->irg, luke_blockwalker, NULL, &si);

#ifdef DUMP_ILP
	{
		FILE           *f;
		char            buf[256];

		ir_snprintf(buf, sizeof(buf), "%s-spillappel.ilp", problem_name);
		if ((f = fopen(buf, "wt")) != NULL) {
			lpp_dump_plain(si.lpp, f);
			fclose(f);
		}
	}
#endif

	DBG((si.dbg, LEVEL_1, "\t%F\n", chordal_env->irg));

#ifdef SOLVE

#ifdef SOLVE_LOCAL
	lpp_solve_cplex(si.lpp);
#else
	lpp_solve_net(si.lpp, LPP_SERVER, LPP_SOLVER);
#endif
	assert(lpp_is_sol_valid(si.lpp)
	       && "solution of ILP must be valid");

	DBG((si.dbg, LEVEL_1, "\titerations: %d, solution time: %g\n", si.lpp->iterations, si.lpp->sol_time));

#ifdef DUMP_SOLUTION
	{
		FILE           *f;
		char            buf[256];

		ir_snprintf(buf, sizeof(buf), "%s-spillappel.sol", problem_name);
		if ((f = fopen(buf, "wt")) != NULL) {
			int             i;
			for (i = 0; i < si.lpp->var_next; ++i) {
				lpp_name_t     *name = si.lpp->vars[i];
				fprintf(f, "%20s %4d %10f\n", name->name, name->nr, name->value);
			}
			fclose(f);
		}
	}
#endif

//	writeback_results(&si);

#endif				/* SOLVE */
	firm_dbg_set_mask(si.dbg, 0);

	free_lpp(si.lpp);
	obstack_free(&obst, NULL);
	exit(0);
}

#else				/* WITH_ILP */

static void
only_that_you_can_compile_without_WITH_ILP_defined(void)
{
}

#endif				/* WITH_ILP */




#if 0
static void
process_block(ir_node * bl, void *data)
{
	char            buf[128];
	int             i,
	                n,
	                skipped = 0;
	spill_ilp_t    *si = data;
	int             step = 0;
	int             n_regs = arch_register_class_n_regs(si->cls);
	int             n_preds = get_irn_arity(bl);
	pset           *live = pset_new_ptr_default();
	irn_live_t     *li;
	ir_node        *irn;

	DBG((si->dbg, LEVEL_3, "\n"));
	DBG((si->dbg, LEVEL_3, "Processing %+F\n", bl));

	/*
	 * Get all live-end values of this block
	 */
	live_foreach(bl, li) {
		if (live_is_end(li) && has_reg_class(si, li->irn)) {
			ir_node        *irn = (ir_node *) li->irn;
			pset_insert_ptr(live, irn);

			/*
			 * The "user" of the live range to the end of a
			 * block is the block itself. This is quite
			 * arbitrary.
			 */
			set_irn_link(irn, get_live_range(si, irn, bl, -1));
		}
	}
	DBG((si->dbg, LEVEL_3, "Live-End:\n"));
	print_live_set(si, live);

	/*
	 * Walk through the schedule of this block from end to begin.
	 * Phis are handled togther with live ins after this loop.
	 */
	for (irn = sched_last(bl); !sched_is_begin(irn) && !is_Phi(irn); irn = sched_prev(irn)) {
		ir_node        *l;
		int             cst;
		int             relevant_args,
		                results;
		int             demand;
		int             n_cand;
		int             must_be_in_mem;
		pset           *cand;

		/*
		 * Determine the number of results
		 */
		/*
		 * Special handling of Projs
		 */
		if (is_Proj(irn)) {
			if (has_reg_class(si, irn)) {
				assert(pset_find_ptr(live, irn)
				       && "node must be live");
				pset_remove_ptr(live, irn);
				skipped++;
			}

			DBG((si->dbg, LEVEL_2, "Skipped %+F\n", irn));
			continue;
		}

		DBG((si->dbg, LEVEL_1, "Irn %+F\n", irn));
		if (skipped > 0) {
			/*
			 * ModeT node
			 */
			assert(get_irn_mode(irn) == mode_T && "node before projs must be tuple");
			results = skipped;
			skipped = 0;
		} else {
			/*
			 * Normal node
			 */
			if (has_reg_class(si, irn)) {
				assert(get_irn_mode(irn) != mode_T && "node must not be a tuple");
				assert(pset_find_ptr(live, irn)
				       && "node must be live");
				pset_remove_ptr(live, irn);
				results = 1;
			} else {
				results = 0;
			}
		}

		/*
		 * cand holds the irns which may be spilled
		 */
		cand = pset_new_ptr(8);
		for (l = pset_first(live); l; l = pset_next(live))
			pset_insert_ptr(cand, l);

		/*
		 * Determine number of arguments
		 */
		relevant_args = 0;
		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node        *op = get_irn_n(irn, i);
			if (has_reg_class(si, op)) {
				DBG((si->dbg, LEVEL_2, "  arg %+F\n", op));
				relevant_args++;

				/*
				 * arguments must not be spilled
				 */
				if (pset_find_ptr(cand, op))
					pset_remove_ptr(cand, op);
			}
		}

		/*
		 * Determine, how many values must be in memory.
		 * We have 'n_regs' registers.
		 * The instr. needs 'demand'.
		 * So (R:= n_regs - demand) registers can be used for candidates 'cand'.
		 * The rest (M:= n_cand - R) must reside in memory.
		 */
		demand = MAX(results, relevant_args);
		n_cand = pset_count(cand);
		must_be_in_mem = n_cand - (n_regs - demand);

		DBG((si->dbg, LEVEL_1, "  Demand: %d, Cands: %d, InMem: %d\n", demand, n_cand, must_be_in_mem));
		DBG((si->dbg, LEVEL_3, "  Cand-Set:\n"));
		print_live_set(si, cand);

		/*
		 * Generate the corresponding constraint spilling
		 * enough candidates at this label.
		 */
		if (must_be_in_mem > 0) {
			ir_snprintf(buf, sizeof(buf), "cp_%N_%N_%d", bl, irn, step);
			cst = lpp_add_cst(si->lpp, buf, lpp_greater, must_be_in_mem);

			for (l = pset_first(cand); l; l = pset_next(cand)) {
				live_range_t   *lr = get_irn_link(l);
				lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, 1.0);
			}
		}

		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node        *op = get_irn_n(irn, i);

			if (has_reg_class(si, op)) {
				live_range_t   *op_lr = get_live_range(si, op, irn, i);
				set_irn_link(op, op_lr);

#if 0
				/*
				 * The operand is reloaded at its usage, so it must not occur
				 * in the constraint which determines which values live at the
				 * instruction must reside in memory.
				 */
				if (must_be_in_mem > 0) {
					DBG((si->dbg, LEVEL_3, " Resetting %+F to 0:\n", op));
					lpp_set_factor_fast(si->lpp, cst, op_lr->in_mem_var, 0.0);
				}
#endif

				/*
				 * Check, if the node is a rematerializable node and
				 * if its operands are live here.
				 */
				if (si->enable_remat && can_remat(si, op, live)) {
					int             cst;
					int             j,
					                n;
					int             n_operands = 0;

					for (j = 0, n = get_irn_arity(op); j < n; ++j)
						n_operands += has_reg_class(si, get_irn_n(op, j));

					/*
					 * Make the remat constraint for
					 * this operand
					 */
					ir_snprintf(buf, sizeof(buf), "ce1_%N_%N_%d", op, irn, i);
					cst = lpp_add_cst(si->lpp, buf, lpp_less, n_operands);

					/*
					 * Make the rematerialize variable
					 * for the operand
					 */
					ir_snprintf(buf, sizeof(buf), "e_%N_%N_%d", op, irn, i);
					op_lr->is_remat_var = lpp_add_var(si->lpp, buf, lpp_binary, COST_REMAT);
					lpp_set_factor_fast(si->lpp, cst, op_lr->is_remat_var, n_operands);

					for (j = 0, n = get_irn_arity(op); j < n; ++j) {
						ir_node        *oop = get_irn_n(op, j);
						if (has_reg_class(si, oop)) {
							live_range_t * lr = get_irn_link(oop);
							lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, 1.0);
						}
					}

					ir_snprintf(buf, sizeof(buf), "ce2_%N_%N_%d", op, irn, i);
					cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
					lpp_set_factor_fast(si->lpp, cst, op_lr->is_remat_var, 1.0);
					lpp_set_factor_fast(si->lpp, cst, op_lr->in_mem_var, -1.0);
				}
			}
		}

		/*
		 * Insert arguments of current instr into the live set
		 */
		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node        *op = get_irn_n(irn, i);
			if (has_reg_class(si, op))
				pset_insert_ptr(live, op);
		}

		del_pset(cand);
		step++;
	}

	if (bl == get_irg_start_block(get_irn_irg(bl)))
		goto end;

	/*
	 * Here, the live set contains
	 * - phis of the block
	 * - live-in values of the block
	 *
	 * TODO: comment is wrong
	 * If a value is live in, it must be in a register in all predecessor
	 * blocks or in memory at the end of all predecessor blocks. Also, the
	 * closest use in the current block must then be from register or
	 * memory, respectively.
	 */
	for (irn = pset_first(live); irn; irn = pset_next(live)) {
		live_range_t   *lr = get_irn_link(irn);
		int             is_phi = is_Phi(irn)
		    && get_nodes_block(irn) == bl;
		int             cst;

		assert(has_reg_class(si, irn));
		assert(is_Phi(irn) || is_live_in(bl, irn));

		/*
		 * Deprecated: Can be done with the first uses map
		 */
		if (is_phi)
			lr->use_head->closest_use = lr;

		/*
		 * Remind the liverange of the first use of a live (or phi) in the
		 * current block.
		 */
		add_first_use(si, bl, irn, lr);

		for (i = 0; i < n_preds; ++i) {
			ir_node        *pred_bl = get_Block_cfgpred_block(bl, i);
			ir_node        *end_node = is_phi ? get_irn_n(irn, i) : irn;
			live_range_t   *op_lr = get_live_range(si, end_node, pred_bl,
							       -1);
			edge_reload_t  *edge = obstack_alloc(si->obst,
							     sizeof(edge[0]));

			ir_snprintf(buf, sizeof(buf), "edge_b%N_p%N_i%N", bl, pred_bl, end_node);
			edge->in_mem_var = lpp_add_var(si->lpp, buf, lpp_binary, get_weight(pred_bl) * COST_LOAD);
			edge->bl = bl;
			edge->irn = end_node;
			edge->pos = i;
			edge->next = si->edges;
			si->edges = edge;

			ir_snprintf(buf, sizeof(buf), "cedge_b%N_p%N_i%N", bl, pred_bl, end_node);
			cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
			lpp_set_factor_fast(si->lpp, cst, op_lr->in_mem_var, 1.0);
			lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, -1.0);
			lpp_set_factor_fast(si->lpp, cst, edge->in_mem_var, -1.0);
		}
	}

      end:
	del_pset(live);
}
#endif
