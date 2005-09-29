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

#define BIGM 1000.0

#define MAX(a,b) ((a) > (b) ? (a) : (b))

#define DBG_LEVEL SET_LEVEL_0

#undef DUMP_SOLUTION
#undef DUMP_ILP

#define LPP_SERVER "i44pc52"
#define LPP_SOLVER "cplex"

#define COST_LOAD      10
#define COST_STORE     50
#define COST_REMAT     (-9)

#define is_end_of_block_use(lr) (is_Block((lr)->user))

typedef struct _spill_ilp_t {
  const be_main_session_env_t *session_env;
  const arch_register_class_t *cls;
	firm_dbg_module_t *dbg;
  lpp_t *lpp;
	set *irn_use_heads;
  set *live_ranges;
	set *spill_ctx;
	pset *remove_phis;
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

typedef struct _spill_ctx_t {
	ir_node *spilled;  /**< The spilled node. */
	ir_node *user;     /**< The node this spill is for. */
	ir_node *spill;    /**< The spill itself. */
} spill_ctx_t;

static int has_reg_class(const spill_ilp_t *si, const ir_node *irn)
{
  return arch_irn_has_reg_class(si->session_env->main_env->arch_env,
      irn, arch_pos_make_out(0), si->cls);
}

static int register_demand(spill_ilp_t *si, const ir_node *irn)
{
	const arch_env_t *arch_env = si->session_env->main_env->arch_env;
	int n_in = arch_get_n_operands(arch_env, irn, 0);
	int n_out = arch_get_n_operands(arch_env, irn, -1);

	return MAX(n_in, n_out);
}

static int cmp_spill_ctx(const void *a, const void *b, size_t n)
{
	const spill_ctx_t *p = a;
	const spill_ctx_t *q = b;
  return !(p->user == q->user && p->spilled == q->spilled);
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
  const arch_env_t *arch_env    = si->session_env->main_env->arch_env;
	int remat = (arch_irn_get_flags(arch_env, irn) & arch_irn_flags_rematerializable) != 0;

	for(i = 0, n = get_irn_arity(irn); i < n && remat; ++i) {
		ir_node *op = get_irn_n(irn, i);
		remat &= !has_reg_class(si, op) || pset_find_ptr(live, op);
	}

	return remat;
}

static spill_ctx_t *get_spill_ctx(spill_ilp_t *si, ir_node *spilled, ir_node *ctx_irn)
{
	spill_ctx_t templ, *res;

	templ.spilled = spilled;
	templ.user    = ctx_irn;
	templ.spill   = NULL;

	res = set_insert(si->spill_ctx, &templ, sizeof(templ),
			HASH_COMBINE(HASH_PTR(spilled), HASH_PTR(ctx_irn)));

	return res;
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

static live_range_t *lookup_live_range(const spill_ilp_t *si, ir_node *irn,
		ir_node *user, int pos)
{
  live_range_t lr;
  unsigned hash = HASH_COMBINE(HASH_PTR(irn), HASH_PTR(user));

  lr.user    = user;
  lr.irn     = irn;
  lr.pos     = pos;
  lr.in_mem_var = -1;

  return set_find(si->live_ranges, &lr, sizeof(lr), hash);
}

static void create_block_live_range_sets(ir_node *bl, void *data)
{
	assert(is_Block(bl));
	set_irn_link(bl, pset_new_ptr_default());
}

static void delete_block_live_range_sets(ir_node *bl, void *data)
{
	assert(is_Block(bl));

	del_pset(get_irn_link(bl));
	set_irn_link(bl, NULL);
}

#if 0
static void annotate_live_ranges(ir_node *irn, void *data)
{
	const ir_edge_t *edge;

	foreach_out_edge(irn, edge) {
		pset *lr_set;

		ir_node *user = edge->use;
		int pos       = edge->pos;
		ir_node *bl   = get_nodes_block(user);

		if(is_Phi(user))
			bl = get_Block_cfgpred_block(bl, pos);

		lr_set = get_irn_link(bl);

	}
}
#endif


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
  ir_node *irn;

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

	sched_foreach_reverse(bl, irn) {
		ir_node *l;
		int cst;
		int demand;
		int n_live;
		int must_be_in_mem;

		/* We handle phi togther with live ins after this loop (see below). */
		if(is_Phi(irn))
			break;

		if(has_reg_class(si, irn))
			pset_remove_ptr(live, irn);

		demand = register_demand(si, irn);
		n_live = pset_count(live);

		/*
		 * Determine, how many values (which are not used at the label)
		 * must be in memory.
		 * demand means the number of registers, the operation will consume.
		 * So there are n_regs - demand registers available to store values
		 * which are not used at this label. The rest must reside in memory.
		 */
		must_be_in_mem = MAX(n_live - (n_regs - demand), 0);

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

static ir_node *spill_irn(spill_ilp_t *si, ir_node *irn, ir_node *ctx_irn)
{
  const be_node_factory_t *fact = si->session_env->main_env->node_factory;
  const arch_env_t *arch_env    = si->session_env->main_env->arch_env;
	spill_ctx_t *ctx;

	ctx = get_spill_ctx(si, irn, ctx_irn);
	if(ctx->spill)
		return ctx->spill;

	ctx->spill = be_spill(fact, arch_env, irn);
	return ctx->spill;
}

static ir_node *spill_phi(spill_ilp_t *si, ir_node *phi, ir_node *ctx_irn)
{
	int i, n;
	ir_mode *mode = get_irn_mode(phi);
	ir_node **ins;
	ir_node *bl = get_nodes_block(phi);
	ir_graph *irg = get_irn_irg(bl);
	spill_ctx_t *ctx;

	assert(is_Phi(phi));

	ctx = get_spill_ctx(si, phi, ctx_irn);
	if(ctx->spill)
		return ctx->spill;

	n    = get_irn_arity(phi);
	ins  = malloc(n * sizeof(ins[0]));

	for(i = 0; i < n; ++i)
		ins[i]  = new_r_Unknown(irg, mode_M);

	ctx->spill = new_r_Phi(irg, bl, n, ins, mode_M);
	free(ins);

	for(i = 0; i < n; ++i) {
		ir_node *arg = get_irn_n(phi, i);
		ir_node *res;

		if(is_Phi(arg))
			res = spill_phi(si, arg, ctx_irn);
		else
			res = spill_irn(si, arg, ctx_irn);

		set_irn_n(ctx->spill, i, res);
	}

	return ctx->spill;
}

static ir_node *spill_live_range(spill_ilp_t *si, live_range_t *lr)
{
	const live_range_t *closest = lr->use_head->closest_use;

	if(is_Phi(lr->irn) && closest && is_spilled(si, closest))
		return spill_phi(si, lr->irn, lr->irn);
	else
		return spill_irn(si, lr->irn, lr->irn);
}


static void writeback_results(spill_ilp_t *si)
{
	const be_node_factory_t *fact = si->session_env->main_env->node_factory;
	const arch_env_t *arch_env    = si->session_env->main_env->arch_env;
	ir_node *irn;
	irn_use_head_t *uh;
	pset *rem_phis = pset_new_ptr_default();

	for(uh = set_first(si->irn_use_heads); uh; uh = set_next(si->irn_use_heads)) {
		if(is_Phi(uh->irn) && is_spilled(si, uh->closest_use))
			pset_insert_ptr(rem_phis, uh->irn);
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
				ir_node *spill   = spill_live_range(si, lr);
				ir_node *reload  = new_Reload(fact, si->cls,
						si->session_env->irg, bl, mode, spill);

				obstack_ptr_grow(si->obst, reload);
				n_reloads++;

				sched_add_before(lr->user, reload);
			}

    	}

		if(n_reloads > 0) {
			reloads = obstack_finish(si->obst);
			be_introduce_copies_ignore(si->session_env->dom_front, irn,
					n_reloads, reloads, rem_phis);
			obstack_free(si->obst, reloads);
		}
    }

	for(irn = pset_first(rem_phis); irn; irn = pset_next(rem_phis)) {
		int i, n;

		for(i = 0, n = get_irn_arity(irn); i < n; ++i)
			set_irn_n(irn, i, new_r_Bad(si->session_env->irg));
		sched_remove(irn);
	}
}

void be_spill_ilp(const be_main_session_env_t *session_env,
    const arch_register_class_t *cls)
{
	char buf[256];
	char problem_name[256];
  struct obstack obst;
  spill_ilp_t si;

	ir_snprintf(problem_name, sizeof(problem_name), "%F_%s", session_env->irg, cls->name);

  obstack_init(&obst);
  si.obst           = &obst;
	si.dbg            = firm_dbg_register("be.ra.spillilp");
  si.session_env    = session_env;
  si.cls            = cls;
  si.lpp            = new_lpp(problem_name, lpp_minimize);
  si.irn_use_heads  = new_set(cmp_irn_use_head, 4096);
  si.live_ranges    = new_set(cmp_live_range, 16384);
  si.spill_ctx      = new_set(cmp_spill_ctx, 4096);
	si.enable_remat   = 1;
	si.enable_store   = 0;

	firm_dbg_set_mask(si.dbg, DBG_LEVEL);
	irg_block_walk_graph(session_env->irg, process_block, NULL, &si);
	if(si.enable_store)
		add_store_costs(&si);

#ifdef DUMP_ILP
	{
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "spill-%s.ilp", problem_name);
		if((f = fopen(buf, "wt")) != NULL) {
			lpp_dump_plain(si.lpp, f);
			fclose(f);
		}
	}
#endif

	DBG((si.dbg, LEVEL_1, "%F\n", session_env->irg));
//	lpp_solve_net(si.lpp, LPP_SERVER, LPP_SOLVER);
	lpp_solve_cplex(si.lpp);
	assert(lpp_is_sol_valid(si.lpp) && "ILP not feasible");

	assert(lpp_is_sol_valid(si.lpp) && "solution of ILP must be valid");

	DBG((si.dbg, LEVEL_1, "\tnodes: %d, vars: %d, csts: %d\n",
				set_count(si.irn_use_heads), si.lpp->var_next, si.lpp->cst_next));
	DBG((si.dbg, LEVEL_1, "\titerations: %d, solution time: %g\n",
				si.lpp->iterations, si.lpp->sol_time));

#ifdef DUMP_SOLUTION
	{
		FILE *f;

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

  del_set(si.irn_use_heads);
  del_set(si.live_ranges);
  free_lpp(si.lpp);
  obstack_free(&obst, NULL);
}
