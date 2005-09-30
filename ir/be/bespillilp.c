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

#define BIGM 1000.0

#define MAX(a,b) ((a) > (b) ? (a) : (b))

#define DBG_LEVEL SET_LEVEL_0

#undef DUMP_SOLUTION
#undef DUMP_ILP
#undef DUMP_STATS

#define LPP_SERVER "i44pc52"
#define LPP_SOLVER "cplex"

#define COST_LOAD      10
#define COST_STORE     50
#define COST_REMAT     (-9)

#define is_end_of_block_use(lr) (is_Block((lr)->user))

typedef struct _spill_stat_t {
	int n_spills;
	int n_reloads;
	int n_remat;
} spill_stat_t;

typedef struct _spill_ilp_t {
	const arch_register_class_t *cls;
	firm_dbg_module_t *dbg;
	lpp_t *lpp;
	set *irn_use_heads;
	set *live_ranges;
	spill_env_t senv;
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

static int has_reg_class(const spill_ilp_t *si, const ir_node *irn)
{
  return arch_irn_has_reg_class(si->senv.session->main_env->arch_env,
      irn, arch_pos_make_out(0), si->cls);
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
  const arch_env_t *arch_env    = si->senv.session->main_env->arch_env;
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

  lr.user    = user;
  lr.irn     = irn;
  lr.pos     = pos;
  lr.in_mem_var = -1;
	lr.is_remat_var = -1;

  res = set_insert(si->live_ranges, &lr, sizeof(lr), hash);
	is_new = res->in_mem_var == -1;

  if(is_new) {
    char buf[128];
    ir_snprintf(buf, sizeof(buf), "m_%s%N_%N_%d",
				is_Phi(irn) ? "phi_" : "", irn, user, MAX(pos, 0));
    res->in_mem_var = lpp_add_var(si->lpp, buf, lpp_binary, pos >= 0 ? COST_LOAD : 0.0);
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

static ir_node *process_irn(spill_ilp_t *si, pset *live, ir_node *irn, int *demand)
{
	int i, n;
	int relevant_args = 0, num_projs = 0;

	while(is_Proj(irn)) {
		if(has_reg_class(si, irn)) {
			assert(pset_find_ptr(live, irn) && "node must be live");
			pset_remove_ptr(live, irn);
			num_projs++;
		}

		irn = sched_prev(irn);
	}

	if(num_projs > 0)
		assert(get_irn_mode(irn) == mode_T && "node before projs must be tuple");

	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);
		relevant_args += has_reg_class(si, op) && !pset_find_ptr(live, op);
	}

	assert(pset_find_ptr(live, irn) && "node must be live");
	pset_remove_ptr(live, irn);

	*demand = MAX(num_projs, relevant_args);
	return irn;
}

static void process_block(ir_node *bl, void *data)
{
	char buf[128];
	int i, n;
  spill_ilp_t *si  = data;
  int step         = 0;
  int n_regs       = arch_register_class_n_regs(si->cls);
	int n_preds      = get_irn_arity(bl);
  pset *live       = pset_new_ptr_default();
  irn_live_t *li;
  ir_node *irn, *next_irn;

  /* as always, bring the live end nodes to life here */
  live_foreach(bl, li) {
    if(live_is_end(li) && has_reg_class(si, li->irn)) {
      ir_node *irn = (ir_node *) li->irn;
      pset_insert_ptr(live, irn);

      /*
       * The "user" of the live range to the end of a block
       * is the block itself. This is quite arbitrary.
       */
      set_irn_link(irn, get_live_range(si, irn, bl, -1));
    }
  }

	for(irn = sched_last(bl); !sched_is_begin(irn) && !is_Phi(irn); irn = next_irn) {
		ir_node *l;
		int cst;
		int demand;
		int n_live;
		int must_be_in_mem;

		/* We handle phi togther with live ins after this loop (see below). */
		if(is_Phi(irn))
			break;

#if 0
		if(has_reg_class(si, irn))
			pset_remove_ptr(live, irn);

		demand = register_demand(si, live, irn);
		n_live = pset_count(live);
#endif

		next_irn = process_irn(si, live, irn, &demand);
		n_live = pset_count(live);

		/*
		 * Determine, how many values (which are not used at the label)
		 * must be in memory.
		 * demand means the number of registers, the operation will consume.
		 * So there are n_regs - demand registers available to store values
		 * which are not used at this label. The rest must reside in memory.
		 */
		must_be_in_mem = MAX(n_live + demand - n_regs, 0);

		if(must_be_in_mem > 0) {

			/*
			 * The constraint limiting the pressure at this label to
			 * the number of free registers.
			 */
			ir_snprintf(buf, sizeof(buf), "cp_%N_%d", bl, step);
			cst = lpp_add_cst(si->lpp, buf, lpp_greater, must_be_in_mem);

			for(l = pset_first(live); l; l = pset_next(live)) {
				live_range_t *lr = get_irn_link(l);
				lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, 1.0);
			}
		}

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op = get_irn_n(irn, i);

			if(has_reg_class(si, op)) {
				live_range_t *op_lr = get_live_range(si, op, irn, i);

				set_irn_link(op, op_lr);

				/*
				 * The operand is reloaded at its usage, so it must not occur
				 * in the constraint which determines which values live at the
				 * instruction must reside in memory.
				 */
				if(must_be_in_mem > 0) {
					lpp_set_factor_fast(si->lpp, cst, op_lr->in_mem_var, 0.0);
				}

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

			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);
				if(has_reg_class(si, op) && !is_Phi(irn))
					pset_insert_ptr(live, op);
			}

		step++;
	}

	if(bl == get_irg_start_block(get_irn_irg(bl)))
		goto end;

	/*
	 * Here, only the phis in the block and the values live in are in the
	 * live set.
	 *
	 * If a value is live in, it must be in a register in all predecessor
	 * blocks or in memory at the end of all predecessor blocks. Also, the
	 * closest use in the current block must then be from register or
	 * memory, respectively.
	 */
	for(irn = pset_first(live); irn; irn = pset_next(live)) {
		live_range_t *lr = get_irn_link(irn);
		int is_phi = is_Phi(irn) && get_nodes_block(irn) == bl;
		int cst;

		if(is_phi)
			lr->use_head->closest_use = lr;

		assert(has_reg_class(si, irn));
		assert(is_Phi(irn) || is_live_in(bl, irn));

#if 0
		ir_snprintf(buf, sizeof(buf), "c%s_%N_%N", (is_phi ? "phi" : "li"), irn, bl);
		cst = lpp_add_cst(si->lpp, buf, lpp_equal, 0.0);
		lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, -n_preds);

		for(i = 0; i < n_preds; ++i) {
			ir_node *pred_bl     = get_Block_cfgpred_block(bl, i);
			ir_node *end_node    = is_phi ? get_irn_n(irn, i) : irn;
			live_range_t *op_lr  = get_live_range(si, end_node, pred_bl, -1);

			lpp_set_factor_fast(si->lpp, cst, op_lr->in_mem_var, 1.0);
		}
#endif

		for(i = 0; i < n_preds; ++i) {
			ir_node *pred_bl     = get_Block_cfgpred_block(bl, i);
			ir_node *end_node    = is_phi ? get_irn_n(irn, i) : irn;
			live_range_t *op_lr  = get_live_range(si, end_node, pred_bl, -1);

			ir_snprintf(buf, sizeof(buf), "cpred_%N_%N_%d", lr->irn, bl, i);
			cst = lpp_add_cst(si->lpp, buf, lpp_equal, 0.0);
			lpp_set_factor_fast(si->lpp, cst, op_lr->in_mem_var, 1.0);
			lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, -1.0);
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
	char buf[64];
	irn_use_head_t *uh;
	double costs = si->enable_store ? COST_STORE : 0.0;

	for(uh = set_first(si->irn_use_heads); uh; uh = set_next(si->irn_use_heads)) {
		int cst;
		live_range_t *lr;

		ir_snprintf(buf, sizeof(buf), "cs_%N", uh->irn);
		cst = lpp_add_cst(si->lpp, buf, lpp_less, 0);

		ir_snprintf(buf, sizeof(buf), "s_%N", uh->irn);
		uh->spill_var = lpp_add_var(si->lpp, buf, lpp_binary, costs);
		lpp_set_factor_fast(si->lpp, cst, uh->spill_var, -BIGM);

    list_for_each_entry(live_range_t, lr, &uh->head, list)
			lpp_set_factor_fast(si->lpp, cst, lr->in_mem_var, 1.0);
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

static void writeback_results(spill_ilp_t *si)
{
	const be_node_factory_t *fact = si->senv.session->main_env->node_factory;
	irn_use_head_t *uh;
	si->senv.mem_phis = pset_new_ptr_default();

	for(uh = set_first(si->irn_use_heads); uh; uh = set_next(si->irn_use_heads)) {
		if(is_Phi(uh->irn) && is_spilled(si, uh->closest_use))
			pset_insert_ptr(si->senv.mem_phis, uh->irn);
	}

	/* Look at each node and examine the usages. */
	for(uh = set_first(si->irn_use_heads); uh; uh = set_next(si->irn_use_heads)) {
		live_range_t *lr;
		ir_node **reloads;

		int n_reloads           = 0;
		ir_node *irn            = uh->irn;
		ir_mode *mode           = get_irn_mode(irn);

		/* Go through all live ranges of the node. */
		list_for_each_entry(live_range_t, lr, &uh->head, list) {
			int spilled = is_spilled(si, lr);
			// int rematd  = !is_zero(lpp_get_var_sol(si->lpp, lr->is_remat_var));

			if(spilled && !is_end_of_block_use(lr)) {
				ir_node *bl      = get_nodes_block(lr->user);


				ir_node *spill   = be_spill_node(&si->senv, lr->irn);
				ir_node *reload  = new_Reload(fact, si->cls, si->senv.session->irg, bl, mode, spill);

				/* inc_stats_reload(si); */
				obstack_ptr_grow(si->obst, reload);
				n_reloads++;

				sched_add_before(lr->user, reload);
			}
		}

		if(n_reloads > 0) {
			reloads = obstack_finish(si->obst);
			be_introduce_copies_ignore(si->senv.session->dom_front, irn, n_reloads, reloads, si->senv.mem_phis);
			obstack_free(si->obst, reloads);
		}
	}
	be_remove_spilled_phis(&si->senv);
}

void be_spill_ilp(const be_main_session_env_t *session_env,
    const arch_register_class_t *cls)
{
	char problem_name[256];
	struct obstack obst;
	spill_ilp_t si;

	ir_snprintf(problem_name, sizeof(problem_name), "%F_%s", session_env->irg, cls->name);

	obstack_init(&obst);
	si.obst            = &obst;
	si.dbg             = firm_dbg_register("be.ra.spillilp");
	si.senv.session    = session_env;
	si.cls             = cls;
	si.lpp             = new_lpp(problem_name, lpp_minimize);
	si.irn_use_heads   = new_set(cmp_irn_use_head, 4096);
	si.live_ranges     = new_set(cmp_live_range, 16384);
	si.senv.spill_ctxs = new_set(be_set_cmp_spillctx, 4096);
	si.enable_remat    = 1;
	si.enable_store    = 0;

	firm_dbg_set_mask(si.dbg, DBG_LEVEL);
	irg_block_walk_graph(session_env->irg, process_block, NULL, &si);
	if(si.enable_store)
		add_store_costs(&si);

#ifdef DUMP_ILP
	{
		FILE *f;
		char buf[256];

		ir_snprintf(buf, sizeof(buf), "spill-%s.ilp", problem_name);
		if((f = fopen(buf, "wt")) != NULL) {
			lpp_dump_plain(si.lpp, f);
			fclose(f);
		}
	}
#endif

	DBG((si.dbg, LEVEL_1, "%F\n", session_env->irg));
	lpp_solve_net(si.lpp, LPP_SERVER, LPP_SOLVER);
	// lpp_solve_cplex(si.lpp);
	assert(lpp_is_sol_valid(si.lpp) && "solution of ILP must be valid");

	DBG((si.dbg, LEVEL_1, "\tnodes: %d, vars: %d, csts: %d\n",
				set_count(si.irn_use_heads), si.lpp->var_next, si.lpp->cst_next));
	DBG((si.dbg, LEVEL_1, "\titerations: %d, solution time: %g\n",
				si.lpp->iterations, si.lpp->sol_time));

#ifdef DUMP_SOLUTION
	{
		FILE *f;
		char buf[256];

		ir_snprintf(buf, sizeof(buf), "spill-%s.sol", problem_name);
		if((f = fopen(buf, "wt")) != NULL) {
			int i;
			for(i = 0; i < si.lpp->var_next; ++i) {
				lpp_name_t *name = si.lpp->vars[i];
				fprintf(f, "%10s %4d %10f\n", name->name, name->nr, name->value);
			}
			fclose(f);
		}
	}
#endif

  writeback_results(&si);

#ifdef DUMP_STATS
	{
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "%s-spill.stat", problem_name);
		if((f = fopen(buf, "wt")) != NULL) {
			fprintf(f, "%20s: %d\n", "nodes", set_count(si.irn_use_heads));
			fprintf(f, "%20s: %d\n", "vars", si.lpp->var_next);
			fprintf(f, "%20s: %d\n", "csts", si.lpp->cst_next);
			fprintf(f, "%20s: %f\n", "sol time", si.lpp->sol_time);
			fprintf(f, "%20s: %d\n", "spills", si->stats.n_spills);
			fprintf(f, "%20s: %d\n", "reloads", si->stats.n_reloads);
			fprintf(f, "%20s: %d\n", "remats", si->stats.n_remat);
			fclose(f);
		}
	}
#endif

  del_set(si.irn_use_heads);
  del_set(si.live_ranges);
  free_lpp(si.lpp);
  obstack_free(&obst, NULL);
}
