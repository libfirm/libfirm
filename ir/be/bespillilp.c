/**
 * @file   bespillilp.c
 * @date   15.07.2005
 * @author Sebastian Hack
 *
 * ILP based spilling
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>

#include "hashptr.h"
#include "debug.h"
#include "obst.h"
#include "set.h"
#include "list.h"
#include "pmap.h"

#include "irprintf.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irloop_t.h"

#include <lpp/lpp.h>
#include <lpp/lpp_net.h>
#include <lpp/lpp_cplex.h>

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch.h"
#include "benode_t.h"
#include "beutil.h"
#include "bespillilp.h"
#include "bespill.h"

#include "bechordal_t.h"

#define BIGM 100000.0

#define MAX(a,b) ((a) > (b) ? (a) : (b))

#define DBG_LEVEL SET_LEVEL_0 // 3

#define DUMP_SOLUTION
#define DUMP_ILP
#define DUMP_STATS

#undef  SOLVE_LOCAL
#define LPP_SERVER "i44pc52"
#define LPP_SOLVER "cplex"

#define COST_LOAD      10
#define COST_STORE     50
#define COST_REMAT     (-9)

#define is_end_of_block_use(lr) (is_Block((lr)->user))

/**
 * Reloads on edges.
 */
typedef struct _edge_reload_t {
	ir_node *irn;
	ir_node *bl;
	int pos;
	int in_mem_var;
	struct _edge_reload_t *next;
} edge_reload_t;

typedef struct _spill_stat_t {
	int n_spills;
	int n_reloads;
	int n_remat;
} spill_stat_t;

typedef struct _spill_ilp_t {
	spill_stat_t stats;
	const arch_register_class_t *cls;
	const be_chordal_env_t *chordal_env;
	firm_dbg_module_t *dbg;
	lpp_t *lpp;
	set *irn_use_heads;
	set *live_ranges;
	set *first_uses;
	spill_env_t *senv;
	edge_reload_t *edges;
	struct obstack *obst;
	int enable_store : 1;
	int enable_remat : 1;
} spill_ilp_t;

typedef struct _live_range_t live_range_t;

typedef struct _irn_use_head_t {
	struct list_head head;
	ir_node *irn;
	int spill_var;
	int n_uses;
	live_range_t *closest_use;
} irn_use_head_t;

struct _live_range_t {
	struct list_head list;
	irn_use_head_t *use_head;
	ir_node *user;
	ir_node *irn;
	int pos;
	int in_mem_var;
	int is_remat_var;
};

/*
 * Associates the first use of a live-in in a block
 * with its live range.
 */
typedef struct _first_use_t {
	ir_node *bl;
	ir_node *irn;       /**< A value live in at bl. */
	live_range_t *lr;   /**< The live range for the first use of irn in bl. */
} first_use_t;


/**
 * Get weight for spill/reload costs
 * Actually computed with loop depth.
 * @param irn The location where to check for the weights.
 * @return The weights at this points.
 */
static double get_weight(const ir_node *irn)
{
	ir_loop *loop = get_irn_loop((ir_node *) irn);
	int res = 1;

	if(loop) {
		int depth = get_loop_depth(loop);
		res += depth * depth;
	}

	return res;
}


static INLINE int has_reg_class(const spill_ilp_t *si, const ir_node *irn)
{
	return chordal_has_class(si->chordal_env, irn);
}

static int cmp_live_range(const void *a, const void *b, size_t n)
{
  const live_range_t *p = a;
  const live_range_t *q = b;

  return !(p->user == q->user && p->irn == q->irn && p->pos == q->pos);
}

static int cmp_irn_use_head(const void *a, const void *b, size_t n)
{
  const irn_use_head_t *p = a;
  const irn_use_head_t *q = b;

	return !(p->irn == q->irn);
}

static irn_use_head_t *get_use_head(spill_ilp_t *si, const ir_node *irn)
{
	irn_use_head_t templ;
	templ.irn = (ir_node *) irn;
	return set_find(si->irn_use_heads, &templ, sizeof(templ), HASH_PTR(irn));
}

static int cmp_first_use(const void *a, const void *b, size_t n)
{
  const first_use_t *p = a;
  const first_use_t *q = b;

  return !(p->irn == q->irn && p->bl == q->bl);
}

static void add_first_use(spill_ilp_t *si, ir_node *bl, ir_node *irn, live_range_t *lr)
{
	first_use_t templ;
	templ.bl    = bl;
	templ.irn   = irn;
	templ.lr    = lr;

	set_insert(si->first_uses, &templ, sizeof(templ),
			HASH_COMBINE(HASH_PTR(bl), HASH_PTR(irn)));
}

static live_range_t *get_first_use_lr(spill_ilp_t *si, ir_node *bl, ir_node *irn)
{
	first_use_t *res;
	first_use_t templ;
	templ.bl    = bl;
	templ.irn   = irn;

	res = set_find(si->first_uses, &templ, sizeof(templ),
			HASH_COMBINE(HASH_PTR(bl), HASH_PTR(irn)));

	return res ? res->lr : NULL;
}

/**
 * Checks, if a vertain node can be recomputed at a certain position.
 * @param si    The spill ILP environment.
 * @param irn   The node to recompute.
 * @param live  The nodes live at the place where @p irn shall be
 *              recomputed.
 * @return      1, if irn can be recomputed, 0 if not.
 */
static INLINE int can_remat(const spill_ilp_t *si, const ir_node *irn, pset *live)
{
	int i, n;
	const arch_env_t *arch_env    = si->chordal_env->main_env->arch_env;
	int remat = (arch_irn_get_flags(arch_env, irn) & arch_irn_flags_rematerializable) != 0;

	for(i = 0, n = get_irn_arity(irn); i < n && remat; ++i) {
		ir_node *op = get_irn_n(irn, i);
		remat &= !has_reg_class(si, op) || pset_find_ptr(live, op);
	}

	return remat;
}

static live_range_t *get_live_range(spill_ilp_t *si, ir_node *irn, ir_node *user, int pos)
{
	live_range_t lr, *res;
	irn_use_head_t iuh, *head;
	int is_new;
	unsigned hash = HASH_COMBINE(HASH_PTR(irn), HASH_PTR(user));

	lr.user         = user;
	lr.irn          = irn;
	lr.pos          = pos;
	lr.in_mem_var   = -1;
	lr.is_remat_var = -1;

	res = set_insert(si->live_ranges, &lr, sizeof(lr), hash);
	is_new = res->in_mem_var == -1;

	if(is_new) {
		char buf[128];
		double cost = 0.0;

		if(pos >= 0)
			cost = get_weight(user) * COST_LOAD;

		ir_snprintf(buf, sizeof(buf), "m_%s%N_%N_%d",
					is_Phi(irn) ? "phi_" : "", irn, user, MAX(pos, 0));
		res->in_mem_var = lpp_add_var(si->lpp, buf, lpp_binary, cost);
	}

	memset(&iuh, 0, sizeof(iuh));
	iuh.irn = irn;
	iuh.n_uses = -1;
	head = set_insert(si->irn_use_heads, &iuh, sizeof(iuh), HASH_PTR(irn));
	if(head->n_uses == -1) {
		head->n_uses = 0;
		INIT_LIST_HEAD(&head->head);
	}

	if(is_new) {
		list_add_tail(&res->list, &head->head);
		head->n_uses++;
	}

	res->use_head = head;

	return res;
}

static void print_live_set(spill_ilp_t *si, pset *s) {
	ir_node *n;
	for(n=pset_first(s); n; n=pset_next(s))
		DBG((si->dbg, LEVEL_3, "    %+F\n", n));
}

static void process_block(ir_node *bl, void *data)
{
	char buf[128];
	int i, n, skipped=0;
	spill_ilp_t *si  = data;
	int step         = 0;
	int n_regs       = arch_register_class_n_regs(si->cls);
	int n_preds      = get_irn_arity(bl);
	pset *live       = pset_new_ptr_default();
	irn_live_t *li;
	ir_node *irn;

	DBG((si->dbg, LEVEL_3, "\n"));
	DBG((si->dbg, LEVEL_3, "Processing %+F\n", bl));

	/*
	 * Get all live-end values of this block
	 */
	live_foreach(bl, li) {
		if(live_is_end(li) && has_reg_class(si, li->irn)) {
			ir_node *irn = (ir_node *) li->irn;
			pset_insert_ptr(live, irn);

			/*The "user" of the live range to the end of a block
			 * is the block itself. This is quite arbitrary. */
			set_irn_link(irn, get_live_range(si, irn, bl, -1));
		}
	}
	DBG((si->dbg, LEVEL_3, "Live-End:\n"));
	print_live_set(si, live);

	/*
	 * Walk through the schedule of this block from end to begin.
	 * Phis are handled togther with live ins after this loop.
	 */
	for(irn = sched_last(bl); !sched_is_begin(irn) && !is_Phi(irn); irn = sched_prev(irn)) {
		ir_node *l;
		int cst;
		int relevant_args, results;
		int demand;
		int n_cand;
		int must_be_in_mem;
		pset *cand;

		/*
		 * Determine the number of results
		 */
		/* Special handling of Projs */
		if(is_Proj(irn)) {
			if(has_reg_class(si, irn)) {
				assert(pset_find_ptr(live, irn) && "node must be live");
				pset_remove_ptr(live, irn);
				skipped++;
			}

			DBG((si->dbg, LEVEL_2, "Skipped %+F\n", irn));
			continue;
		}

		DBG((si->dbg, LEVEL_1, "Irn %+F\n", irn));
		if(skipped > 0) {
			/* ModeT node */
			assert(get_irn_mode(irn) == mode_T && "node before projs must be tuple");
			results = skipped;
			skipped = 0;
		} else {
			/* Normal node */
			if(has_reg_class(si, irn)) {
				assert(get_irn_mode(irn) != mode_T && "node must not be a tuple");
				assert(pset_find_ptr(live, irn) && "node must be live");
				pset_remove_ptr(live, irn);
				results = 1;
			} else {
				results = 0;
			}
		}

		/* cand holds the irns which may be spilled */
		cand = pset_new_ptr(8);
		for(l=pset_first(live); l; l=pset_next(live))
			pset_insert_ptr(cand, l);

		/*
		 * Determine number of arguments
		 */
		relevant_args = 0;
		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op = get_irn_n(irn, i);
			if(has_reg_class(si, op)) {
				DBG((si->dbg, LEVEL_2, "  arg %+F\n", op));
				relevant_args++;

				/* arguments must not be spilled */
				if(pset_find_ptr(cand, op))
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
		if(must_be_in_mem > 0) {
			ir_snprintf(buf, sizeof(buf), "cp_%N_%N_%d", bl, irn, step);
			cst = lpp_add_cst(si->lpp, buf, lpp_greater, must_be_in_mem);

			for(l = pset_first(cand); l; l = pset_next(cand)) {
				live_range_t *lr = get_irn_link(l);
				lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, 1.0);
			}
		}

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op = get_irn_n(irn, i);

			if(has_reg_class(si, op)) {
				live_range_t *op_lr = get_live_range(si, op, irn, i);
				set_irn_link(op, op_lr);

#if 0
				/*
				 * The operand is reloaded at its usage, so it must not occur
				 * in the constraint which determines which values live at the
				 * instruction must reside in memory.
				 */
				if(must_be_in_mem > 0) {
					DBG((si->dbg, LEVEL_3, " Resetting %+F to 0:\n", op));
					lpp_set_factor_fast(si->lpp, cst, op_lr->in_mem_var, 0.0);
				}
#endif

				/*
				 * Check, if the node is a rematerializable node and
				 * if its operands are live here.
				 */
				if(si->enable_remat && can_remat(si, op, live)) {
					int cst;
					int j, n;
					int n_operands = 0;

					for(j = 0, n = get_irn_arity(op); j < n; ++j)
						n_operands += has_reg_class(si, get_irn_n(op, j));

					/* Make the remat constraint for this operand */
					ir_snprintf(buf, sizeof(buf), "ce1_%N_%N_%d", op, irn, i);
					cst = lpp_add_cst(si->lpp, buf, lpp_less, n_operands);

					/* Make the rematerialize variable for the operand */
					ir_snprintf(buf, sizeof(buf), "e_%N_%N_%d", op, irn, i);
					op_lr->is_remat_var = lpp_add_var(si->lpp, buf, lpp_binary, COST_REMAT);
					lpp_set_factor_fast(si->lpp, cst, op_lr->is_remat_var, n_operands);

					for(j = 0, n = get_irn_arity(op); j < n; ++j) {
						ir_node *oop = get_irn_n(op, j);
						if(has_reg_class(si, oop)) {
							live_range_t *lr = get_irn_link(oop);
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
		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op = get_irn_n(irn, i);
			if(has_reg_class(si, op))
				pset_insert_ptr(live, op);
		}

		del_pset(cand);
		step++;
	}

	if(bl == get_irg_start_block(get_irn_irg(bl)))
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
	for(irn = pset_first(live); irn; irn = pset_next(live)) {
		live_range_t *lr = get_irn_link(irn);
		int is_phi = is_Phi(irn) && get_nodes_block(irn) == bl;
		int cst;

		assert(has_reg_class(si, irn));
		assert(is_Phi(irn) || is_live_in(bl, irn));

		/* Deprecated: Can be done with the first uses map */
		if(is_phi)
			lr->use_head->closest_use = lr;

		/*
		 * Remind the liverange of the first use of a live (or phi) in the
		 * current block.
		 */
		add_first_use(si, bl, irn, lr);

		for(i = 0; i < n_preds; ++i) {
			ir_node *pred_bl     = get_Block_cfgpred_block(bl, i);
			ir_node *end_node    = is_phi ? get_irn_n(irn, i) : irn;
			live_range_t *op_lr  = get_live_range(si, end_node, pred_bl, -1);
			edge_reload_t *edge  = obstack_alloc(si->obst, sizeof(edge[0]));

			ir_snprintf(buf, sizeof(buf), "edge_b%N_p%N_i%N", bl, pred_bl, end_node);
			edge->in_mem_var = lpp_add_var(si->lpp, buf, lpp_binary, get_weight(pred_bl) * COST_LOAD);
			edge->bl         = bl;
			edge->irn        = end_node;
			edge->pos        = i;
			edge->next       = si->edges;
			si->edges        = edge;

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

/**
 * Add the costs for a store.
 *
 * If one of the uses is from memory, add additional costs for the
 * spill.
 *
 * m_1 + ... + m_n - M * s <= 0
 *
 * @param si The ILP spilling environment.
 */
static void add_store_costs(spill_ilp_t *si)
{
	if(si->enable_store) {
		char buf[64];
		irn_use_head_t *uh;

		for(uh = set_first(si->irn_use_heads); uh; uh = set_next(si->irn_use_heads)) {
			int cst;
			live_range_t *lr;

			ir_snprintf(buf, sizeof(buf), "cs_%N", uh->irn);
			cst = lpp_add_cst(si->lpp, buf, lpp_less, 0);

			ir_snprintf(buf, sizeof(buf), "s_%N", uh->irn);
			uh->spill_var = lpp_add_var(si->lpp, buf, lpp_binary,
					get_weight(uh->irn) * COST_STORE);
			lpp_set_factor_fast(si->lpp, cst, uh->spill_var, -BIGM);

			list_for_each_entry(live_range_t, lr, &uh->head, list)
				lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, 1.0);
		}
	}
}

static INLINE int is_zero(double x)
{
  return fabs(x) < 0.00001;
}

static int is_spilled(const spill_ilp_t *si, const live_range_t *lr)
{
	return !is_zero(lpp_get_var_sol(si->lpp, lr->in_mem_var));
}

static int is_mem_phi(const ir_node *phi, void *data)
{
	spill_ilp_t *si = data;
	return is_spilled(si, get_use_head(si, phi)->closest_use);
}

static void writeback_results(spill_ilp_t *si)
{
	irn_use_head_t *uh;
	edge_reload_t *edge;

	/* Look at each node and examine the usages. */
	for(uh = set_first(si->irn_use_heads); uh; uh = set_next(si->irn_use_heads)) {
		live_range_t *lr;

		si->stats.n_spills += !is_zero(lpp_get_var_sol(si->lpp, uh->spill_var));

		/* Go through all live ranges of the node. */
		list_for_each_entry(live_range_t, lr, &uh->head, list) {
			if(is_spilled(si, lr) && !is_end_of_block_use(lr)) {
				DBG((si->dbg, LEVEL_2, "%+F: inserting reload at user %+F\n",
							lr->irn, lr->user));
				be_add_reload(si->senv, lr->irn, lr->user);
				si->stats.n_reloads += 1;
			}
		}
	}

	for(edge = si->edges; edge; edge = edge->next) {
		if(!is_zero(lpp_get_var_sol(si->lpp, edge->in_mem_var))) {
			DBG((si->dbg, LEVEL_2, "%+F: insert reload on edge %d from %+F\n",
						edge->irn, edge->pos, edge->bl));
			be_add_reload_on_edge(si->senv, edge->irn, edge->bl, edge->pos);
			si->stats.n_reloads += 1;
		}
	}

	be_insert_spills_reloads(si->senv, NULL);
}

void be_spill_ilp(const be_chordal_env_t *chordal_env)
{
	char problem_name[256];
	struct obstack obst;
	spill_ilp_t si;

	ir_snprintf(problem_name, sizeof(problem_name), "%F_%s",
		chordal_env->irg, chordal_env->cls->name);

	obstack_init(&obst);
	memset(&si.stats, 0, sizeof(si.stats));
	si.chordal_env     = chordal_env;
	si.obst            = &obst;
	si.dbg             = firm_dbg_register("be.ra.spillilp");
	si.senv            = be_new_spill_env(si.dbg, chordal_env, is_mem_phi, &si);
	si.cls             = chordal_env->cls;
	si.lpp             = new_lpp(problem_name, lpp_minimize);
	si.irn_use_heads   = new_set(cmp_irn_use_head, 4096);
	si.live_ranges     = new_set(cmp_live_range, 16384);
	si.first_uses      = new_set(cmp_first_use, 4096);
	si.edges           = NULL;
	si.enable_remat    = 0;
	si.enable_store    = 1;

	firm_dbg_set_mask(si.dbg, DBG_LEVEL);
	irg_block_walk_graph(chordal_env->irg, process_block, NULL, &si);
	if(si.enable_store)
		add_store_costs(&si);

#ifdef DUMP_ILP
	{
		FILE *f;
		char buf[256];

		ir_snprintf(buf, sizeof(buf), "%s-spill.ilp", problem_name);
		if((f = fopen(buf, "wt")) != NULL) {
			lpp_dump_plain(si.lpp, f);
			fclose(f);
		}
	}
#endif

	DBG((si.dbg, LEVEL_1, "%F\n", chordal_env->irg));
#ifdef SOLVE_LOCAL
	lpp_solve_cplex(si.lpp);
#else
	lpp_solve_net(si.lpp, LPP_SERVER, LPP_SOLVER);
#endif
	assert(lpp_is_sol_valid(si.lpp) && "solution of ILP must be valid");

	DBG((si.dbg, LEVEL_1, "\tnodes: %d, vars: %d, csts: %d\n",
				set_count(si.irn_use_heads), si.lpp->var_next, si.lpp->cst_next));
	DBG((si.dbg, LEVEL_1, "\titerations: %d, solution time: %g\n",
				si.lpp->iterations, si.lpp->sol_time));

#ifdef DUMP_SOLUTION
	{
		FILE *f;
		char buf[256];

		ir_snprintf(buf, sizeof(buf), "%s-spill.sol", problem_name);
		if((f = fopen(buf, "wt")) != NULL) {
			int i;
			for(i = 0; i < si.lpp->var_next; ++i) {
				lpp_name_t *name = si.lpp->vars[i];
				fprintf(f, "%20s %4d %10f\n", name->name, name->nr, name->value);
			}
			fclose(f);
		}
	}
#endif

	writeback_results(&si);

#ifdef DUMP_STATS
	{
		char buf[256];
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "%s-spill.stat", problem_name);
		if((f = fopen(buf, "wt")) != NULL) {
			fprintf(f, "%20s: %d\n", "nodes", set_count(si.irn_use_heads));
			fprintf(f, "%20s: %d\n", "vars", si.lpp->var_next);
			fprintf(f, "%20s: %d\n", "csts", si.lpp->cst_next);
			fprintf(f, "%20s: %f\n", "sol time", si.lpp->sol_time);
			fprintf(f, "%20s: %d\n", "spills", si.stats.n_spills);
			fprintf(f, "%20s: %d\n", "reloads", si.stats.n_reloads);
			fprintf(f, "%20s: %d\n", "remats", si.stats.n_remat);
			fclose(f);
		}
	}
#endif

	del_set(si.irn_use_heads);
	del_set(si.live_ranges);
	free_lpp(si.lpp);
	obstack_free(&obst, NULL);
}
