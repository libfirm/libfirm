/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

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
#include "irdump_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irloop_t.h"
#include "irnodeset.h"
#include "phiclass.h"
#include "iredges.h"
#include "execfreq.h"
#include "irvrfy.h"
#include "irbackedge_t.h"

#include <lpp/lpp.h>
#include <lpp/mps.h>
#include <lpp/lpp_net.h>
#include <lpp/lpp_cplex.h>

#include "be_t.h"
#include "beirg_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "bessaconstr.h"
#include "bearch_t.h"
#include "beabi.h"
#include "benode_t.h"
#include "beutil.h"
#include "bespillremat.h"
#include "bespill.h"
#include "bepressurestat.h"
#include "beprofile.h"
#include "bespilloptions.h"

#include "bechordal_t.h"

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

#define DUMP_PROBLEM       1
#define DUMP_MPS           2
#define DUMP_SOLUTION      4
#define DUMP_STATS         8
#define DUMP_PRESSURE      16

#define KEEPALIVE_REMATS   1
#define KEEPALIVE_SPILLS   2
#define KEEPALIVE_RELOADS  4

#define VERIFY_MEMINTERF   1
#define VERIFY_DOMINANCE   2

#define REMATS_NONE        0
#define REMATS_BRIGGS      1
#define REMATS_NOINVERSE   2
#define REMATS_ALL         3

static unsigned opt_dump_flags = 0;
static int opt_log = 0;
static unsigned opt_keep_alive = 0;
static int opt_goodwin = 1;
static int opt_memcopies = 1;
static int opt_memoperands = 1;
static int opt_verify = VERIFY_MEMINTERF;
static unsigned opt_remats = REMATS_ALL;
static int opt_repair_schedule = 0;
static int opt_no_enlarge_liveness = 0;
static int opt_remat_while_live = 1;
static int opt_timeout = 300;
static double opt_cost_reload = 8.0;
static double opt_cost_memoperand =  7.0;
static double opt_cost_spill =  15.0;
static double opt_cost_remat =  1.0;


static const lc_opt_enum_mask_items_t dump_items[] = {
	{ "problem",  DUMP_PROBLEM  },
	{ "mps",      DUMP_MPS      },
	{ "solution", DUMP_SOLUTION },
	{ "stats",    DUMP_STATS },
	{ "pressure", DUMP_PRESSURE },
	{ NULL,       0 }
};

static lc_opt_enum_mask_var_t dump_var = {
	&opt_dump_flags, dump_items
};

static const lc_opt_enum_mask_items_t keepalive_items[] = {
	{ "remats",  KEEPALIVE_REMATS  },
	{ "spills",  KEEPALIVE_SPILLS  },
	{ "reloads", KEEPALIVE_RELOADS },
	{ NULL,      0 }
};

static lc_opt_enum_mask_var_t keep_alive_var = {
	&opt_keep_alive, keepalive_items
};

static const lc_opt_enum_mask_items_t remats_items[] = {
	{ "none",      REMATS_NONE      },
	{ "briggs",    REMATS_BRIGGS    },
	{ "noinverse", REMATS_NOINVERSE },
	{ "all",       REMATS_ALL       },
	{ NULL,        0 }
};

static lc_opt_enum_mask_var_t remats_var = {
	&opt_remats, remats_items
};

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_ENUM_MASK("keepalive", "keep alive inserted nodes",                              &keep_alive_var),

	LC_OPT_ENT_BOOL     ("goodwin",  "activate goodwin reduction",                              &opt_goodwin),
	LC_OPT_ENT_BOOL     ("memcopies",  "activate memcopy handling",                             &opt_memcopies),
	LC_OPT_ENT_BOOL     ("memoperands",  "activate memoperands",                                &opt_memoperands),
	LC_OPT_ENT_ENUM_INT ("remats",  "type of remats to insert",                                 &remats_var),
	LC_OPT_ENT_BOOL     ("repair_schedule",  "repair the schedule by rematting once used nodes",&opt_repair_schedule),
	LC_OPT_ENT_BOOL     ("no_enlage_liveness",  "do not enlarge liveness of operands of remats",&opt_no_enlarge_liveness),
	LC_OPT_ENT_BOOL     ("remat_while_live",  "only remat where rematted value was live",       &opt_remat_while_live),

	LC_OPT_ENT_ENUM_MASK("dump", "dump problem, solution or statistical data",                  &dump_var),
	LC_OPT_ENT_BOOL     ("log",  "activate the lpp log",                                        &opt_log),
	LC_OPT_ENT_INT      ("timeout",  "ILP solver timeout",                                      &opt_timeout),

	LC_OPT_ENT_DBL      ("cost_reload",  "cost of a reload",                                    &opt_cost_reload),
	LC_OPT_ENT_DBL      ("cost_memoperand",  "cost of a memory operand",                        &opt_cost_memoperand),
	LC_OPT_ENT_DBL      ("cost_spill",  "cost of a spill instruction",                          &opt_cost_spill),
	LC_OPT_ENT_DBL      ("cost_remat",  "cost of a rematerialization",                          &opt_cost_remat),
	{ NULL }
};

//#define EXECFREQ_LOOPDEPH   /* compute execution frequency from loop depth only */
//#define SCHEDULE_PHIM   /* insert phim nodes into schedule */

#define  SOLVE
//#define  SOLVE_LOCAL
#define LPP_SERVER "i44pc52"
#define LPP_SOLVER "cplex"


#define MAX_PATHS      INT_MAX
#define ILP_UNDEF		-1

typedef struct _spill_ilp_t {
	const arch_register_class_t  *cls;
	int                           n_regs;
	be_irg_t                     *birg;
	be_lv_t                      *lv;
	lpp_t                        *lpp;
	struct obstack               *obst;
	set                          *remat_info;
	pset                         *all_possible_remats;
	pset                         *inverse_ops;
	ir_node                      *keep;
	set                          *values; /**< for collecting all definitions of values before running ssa-construction */
	pset                         *spills;
	set                          *interferences;
	ir_node                      *m_unknown;
	set                          *memoperands;
	phi_classes_t                *pc;
#ifndef SCHEDULE_PHIM
	pset                         *phims;
#endif
	DEBUG_ONLY(firm_dbg_module_t * dbg);
} spill_ilp_t;

typedef int ilp_var_t;
typedef int ilp_cst_t;

typedef struct _spill_bb_t {
	set      *ilp;
	set      *reloads;
} spill_bb_t;

typedef struct _remat_t {
	const ir_node        *op;      /**< for copy_irn */
	const ir_node        *value;   /**< the value which is being recomputed by this remat */
	const ir_node        *proj;    /**< not NULL if the above op produces a tuple */
	int                   cost;    /**< cost of this remat */
	int                   inverse; /**< nonzero if this is an inverse remat */
} remat_t;

/**
 * Data to be attached to each IR node. For remats this contains the ilp_var
 * for this remat and for normal ops this contains the ilp_vars for
 * reloading each operand
 */
typedef struct _op_t {
	int             is_remat;
	union {
		struct {
			ilp_var_t       ilp;
			const remat_t  *remat; /** the remat this op belongs to */
			int             pre; /** 1, if this is a pressure-increasing remat */
		} remat;
		struct {
			ilp_var_t       ilp;
			ir_node        *op; /** the operation this live range belongs to */
			union {
				ilp_var_t      *reloads;
				ilp_var_t      *copies;
			} args;
		} live_range;
	} attr;
} op_t;

typedef struct _defs_t {
	const ir_node   *value;
	ir_node         *spills;  /**< points to the first spill for this value (linked by link field) */
	ir_node         *remats;  /**< points to the first definition for this value (linked by link field) */
} defs_t;

typedef struct _remat_info_t {
	const ir_node       *irn; /**< the irn to which these remats belong */
	pset                *remats; /**< possible remats for this value */
	pset                *remats_by_operand; /**< remats with this value as operand */
} remat_info_t;

typedef struct _keyval_t {
	const void          *key;
	const void          *val;
} keyval_t;

typedef struct _spill_t {
	ir_node            *irn;
	ilp_var_t           reg_in;
	ilp_var_t           mem_in;
	ilp_var_t           reg_out;
	ilp_var_t           mem_out;
	ilp_var_t           spill;
} spill_t;

typedef struct _memoperand_t {
	ir_node             *irn; /**< the irn */
	unsigned int         pos; /**< the position of the argument */
	ilp_var_t            ilp; /**< the ilp var for this memory operand */
} memoperand_t;

static INLINE int
has_reg_class(const spill_ilp_t * si, const ir_node * irn)
{
	return arch_irn_consider_in_reg_alloc(si->birg->main_env->arch_env,
	                                      si->cls, irn);
}

#if 0
static int
cmp_remat(const void *a, const void *b)
{
	const keyval_t *p = a;
	const keyval_t *q = b;
	const remat_t  *r = p->val;
	const remat_t  *s = q->val;

	assert(r && s);

	return !(r == s || r->op == s->op);
}
#endif
static int
cmp_remat(const void *a, const void *b)
{
	const remat_t  *r = a;
	const remat_t  *s = a;

	return !(r == s || r->op == s->op);
}

static int
cmp_spill(const void *a, const void *b, size_t size)
{
	const spill_t *p = a;
	const spill_t *q = b;

//	return !(p->irn == q->irn && p->bb == q->bb);
	return !(p->irn == q->irn);
}

static int
cmp_memoperands(const void *a, const void *b, size_t size)
{
	const memoperand_t *p = a;
	const memoperand_t *q = b;

	return !(p->irn == q->irn && p->pos == q->pos);
}

static keyval_t *
set_find_keyval(set * set, const void * key)
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

static defs_t *
set_find_def(set * set, const ir_node * value)
{
	defs_t     query;

	query.value = value;
	return set_find(set, &query, sizeof(query), HASH_PTR(value));
}

static defs_t *
set_insert_def(set * set, const ir_node * value)
{
	defs_t     query;

	query.value = value;
	query.spills = NULL;
	query.remats = NULL;
	return set_insert(set, &query, sizeof(query), HASH_PTR(value));
}

static memoperand_t *
set_insert_memoperand(set * set, ir_node * irn, unsigned int pos, ilp_var_t ilp)
{
	memoperand_t     query;

	query.irn = irn;
	query.pos = pos;
	query.ilp = ilp;
	return set_insert(set, &query, sizeof(query), HASH_PTR(irn)+pos);
}

static memoperand_t *
set_find_memoperand(set * set, const ir_node * irn, unsigned int pos)
{
	memoperand_t     query;

	query.irn = (ir_node*)irn;
	query.pos = pos;
	return set_find(set, &query, sizeof(query), HASH_PTR(irn)+pos);
}


static spill_t *
set_find_spill(set * set, const ir_node * value)
{
	spill_t     query;

	query.irn = (ir_node*)value;
	return set_find(set, &query, sizeof(query), HASH_PTR(value));
}

#define pset_foreach(s,i) for((i)=pset_first((s)); (i); (i)=pset_next((s)))
#define set_foreach(s,i) for((i)=set_first((s)); (i); (i)=set_next((s)))
#define foreach_post_remat(s,i) for((i)=next_post_remat((s)); (i); (i)=next_post_remat((i)))
#define foreach_pre_remat(si,s,i) for((i)=next_pre_remat((si),(s)); (i); (i)=next_pre_remat((si),(i)))
#define	sched_foreach_op(s,i) for((i)=sched_next_op((s));!sched_is_end((i));(i)=sched_next_op((i)))

static int
cmp_remat_info(const void *a, const void *b, size_t size)
{
	const remat_info_t *p = a;
	const remat_info_t *q = b;

	return !(p->irn == q->irn);
}

static int
cmp_defs(const void *a, const void *b, size_t size)
{
	const defs_t *p = a;
	const defs_t *q = b;

	return !(p->value == q->value);
}

static int
cmp_keyval(const void *a, const void *b, size_t size)
{
	const keyval_t *p = a;
	const keyval_t *q = b;

	return !(p->key == q->key);
}

static double
execution_frequency(const spill_ilp_t *si, const ir_node * irn)
{
#define FUDGE 0.001
	if(be_profile_has_data())
		return ((double)be_profile_get_block_execcount(get_block(irn))) + FUDGE;

#ifndef EXECFREQ_LOOPDEPH
	return get_block_execfreq(si->birg->exec_freq, get_block(irn)) + FUDGE;
#else
	if(is_Block(irn))
		return exp(get_loop_depth(get_irn_loop(irn)) * log(10)) + FUDGE;
	else
		return exp(get_loop_depth(get_irn_loop(get_nodes_block(irn))) * log(10)) + FUDGE;
#endif
}

static double
get_cost(const spill_ilp_t * si, const ir_node * irn)
{
	if(be_is_Spill(irn)) {
		return opt_cost_spill;
	} else if(be_is_Reload(irn)){
		return opt_cost_reload;
	} else {
		return arch_get_op_estimated_cost(si->birg->main_env->arch_env, irn);
	}
}

/**
 * Checks, whether node and its operands have suitable reg classes
 */
static INLINE int
is_rematerializable(const spill_ilp_t * si, const ir_node * irn)
{
	int               n;
	const arch_env_t *arch_env = si->birg->main_env->arch_env;
	int               remat = (arch_irn_get_flags(arch_env, irn) & arch_irn_flags_rematerializable) != 0;

#if 0
	if(!remat)
		ir_fprintf(stderr, "  Node %+F is not rematerializable\n", irn);
#endif

	for (n = get_irn_arity(irn)-1; n>=0 && remat; --n) {
		ir_node        *op = get_irn_n(irn, n);
		remat &= has_reg_class(si, op) || arch_irn_get_flags(arch_env, op) & arch_irn_flags_ignore || (get_irn_op(op) == op_NoMem);

//		if(!remat)
//			ir_fprintf(stderr, "  Argument %d (%+F) of Node %+F has wrong regclass\n", i, op, irn);
	}

	return remat;
}

/**
 * Try to create a remat from @p op with destination value @p dest_value
 */
static INLINE remat_t *
get_remat_from_op(spill_ilp_t * si, const ir_node * dest_value, const ir_node * op)
{
	remat_t  *remat = NULL;

//	if(!mode_is_datab(get_irn_mode(dest_value)))
//		return NULL;

	if(dest_value == op) {
		const ir_node *proj = NULL;

		if(is_Proj(dest_value)) {
			op = get_Proj_pred(op);
			proj = dest_value;
		}

		if(!is_rematerializable(si, op))
			return NULL;

		remat          = obstack_alloc(si->obst, sizeof(*remat));
		remat->op      = op;
		remat->cost    = (int)get_cost(si, op);
		remat->value   = dest_value;
		remat->proj    = proj;
		remat->inverse = 0;
	} else {
		arch_inverse_t     inverse;
		int                n;

		/* get the index of the operand we want to retrieve by the inverse op */
		for (n = get_irn_arity(op)-1; n>=0; --n) {
			ir_node        *arg = get_irn_n(op, n);

			if(arg == dest_value) break;
		}
		if(n<0) return NULL;

		DBG((si->dbg, LEVEL_5, "\t  requesting inverse op for argument %d of op %+F\n", n, op));

		/* else ask the backend to give an inverse op */
		if(arch_get_inverse(si->birg->main_env->arch_env, op, n, &inverse, si->obst)) {
			int   i;

			DBG((si->dbg, LEVEL_4, "\t  backend gave us an inverse op with %d nodes and cost %d\n", inverse.n, inverse.costs));

			assert(inverse.n > 0 && "inverse op should have at least one node");

			for(i=inverse.n-1; i>=0; --i) {
				pset_insert_ptr(si->inverse_ops, inverse.nodes[i]);
			}

			if(inverse.n <= 2) {
				remat = obstack_alloc(si->obst, sizeof(*remat));
				remat->op = inverse.nodes[0];
				remat->cost = inverse.costs;
				remat->value = dest_value;
				remat->proj = (inverse.n==2)?inverse.nodes[1]:NULL;
				remat->inverse = 1;

				// Matze: commented this out, this doesn't seem to be true if
				// the inverse is a simple operation with only 1 result...
				//assert(is_Proj(remat->proj));
			} else {
				assert(0 && "I can not handle remats with more than 2 nodes");
			}
		}
	}

	if(remat) {
		if(remat->proj) {
			DBG((si->dbg, LEVEL_3, "\t >Found remat %+F for %+F from %+F with %+F\n", remat->op, dest_value, op, remat->proj));
		} else {
			DBG((si->dbg, LEVEL_3, "\t >Found remat %+F for %+F from %+F\n", remat->op, dest_value, op));
		}
	}
	return remat;
}


static INLINE void
add_remat(const spill_ilp_t * si, const remat_t * remat)
{
	remat_info_t    *remat_info,
                     query;
	int              n;

	assert(remat->op);
	assert(remat->value);

	query.irn = remat->value;
	query.remats = NULL;
	query.remats_by_operand = NULL;
	remat_info = set_insert(si->remat_info, &query, sizeof(query), HASH_PTR(remat->value));

	if(remat_info->remats == NULL) {
		remat_info->remats = new_pset(cmp_remat, 4096);
	}
	pset_insert(remat_info->remats, remat, HASH_PTR(remat->op));

	/* insert the remat into the remats_be_operand set of each argument of the remat op */
	for (n = get_irn_arity(remat->op)-1; n>=0; --n) {
		ir_node        *arg = get_irn_n(remat->op, n);

		query.irn = arg;
		query.remats = NULL;
		query.remats_by_operand = NULL;
		remat_info = set_insert(si->remat_info, &query, sizeof(query), HASH_PTR(arg));

		if(remat_info->remats_by_operand == NULL) {
			remat_info->remats_by_operand = new_pset(cmp_remat, 4096);
		}
		pset_insert(remat_info->remats_by_operand, remat, HASH_PTR(remat->op));
	}
}

static int
get_irn_n_nonremat_edges(const spill_ilp_t * si, const ir_node * irn)
{
	const ir_edge_t   *edge = get_irn_out_edge_first(irn);
	int                i = 0;

	while(edge) {
		if(!pset_find_ptr(si->inverse_ops, edge->src)) {
			++i;
		}
		edge = get_irn_out_edge_next(irn, edge);
	}

	return i;
}

static int
get_irn_n_nonignore_args(const spill_ilp_t * si, const ir_node * irn)
{
	int n;
	int ret = 0;

	if(is_Proj(irn))
		irn = get_Proj_pred(irn);

	for(n=get_irn_arity(irn)-1; n>=0; --n) {
		const ir_node  *arg = get_irn_n(irn, n);

		if(has_reg_class(si, arg)) ++ret;
	}

	return ret;
}

static INLINE void
get_remats_from_op(spill_ilp_t * si, const ir_node * op)
{
	int      n;
	remat_t *remat;

	if( has_reg_class(si, op)
	&& (opt_repair_schedule || get_irn_n_nonremat_edges(si, op) > 1)
	&& (opt_remats !=  REMATS_BRIGGS || get_irn_n_nonignore_args(si, op) == 0)
	) {
		remat = get_remat_from_op(si, op, op);
		if(remat) {
			add_remat(si, remat);
		}
	}

	if(opt_remats == REMATS_ALL) {
		/* repeat the whole stuff for each remat retrieved by get_remat_from_op(op, arg)
		   for each arg */
		for (n = get_irn_arity(op)-1; n>=0; --n) {
			ir_node        *arg = get_irn_n(op, n);

			if(has_reg_class(si, arg)) {
				/* try to get an inverse remat */
				remat = get_remat_from_op(si, arg, op);
				if(remat) {
					add_remat(si, remat);
				}
			}
		}
	}
}

static INLINE int
value_is_defined_before(const spill_ilp_t * si, const ir_node * pos, const ir_node * val)
{
	ir_node *block;
	ir_node *def_block = get_nodes_block(val);
	int      ret;

	if(val == pos)
		return 0;

	/* if pos is at end of a basic block */
	if(is_Block(pos)) {
		ret = (pos == def_block || block_dominates(def_block, pos));
//		ir_fprintf(stderr, "(def(bb)=%d) ", ret);
		return ret;
	}

	/* else if this is a normal operation */
	block = get_nodes_block(pos);
	if(block == def_block) {
		if(!sched_is_scheduled(val)) return 1;

		ret = sched_comes_after(val, pos);
//		ir_fprintf(stderr, "(def(same block)=%d) ",ret);
		return ret;
	}

	ret = block_dominates(def_block, block);
//	ir_fprintf(stderr, "(def(other block)=%d) ", ret);
	return ret;
}

static INLINE ir_node *
sched_block_last_noncf(const spill_ilp_t * si, const ir_node * bb)
{
    return sched_skip((ir_node*)bb, 0, sched_skip_cf_predicator, (void *) si->birg->main_env->arch_env);
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

static INLINE ir_node *
sched_next_nonproj(const ir_node * irn, int forward)
{
	return sched_skip((ir_node*)irn, forward, sched_skip_proj_predicator, NULL);
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

	return sched_next_nonproj(next, 1);
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

	return sched_next_nonproj(prev, 0);
}

static void
sched_put_after(ir_node * insert, ir_node * irn)
{
	if(is_Block(insert)) {
		insert = sched_block_first_nonphi(insert);
	} else {
		insert = sched_next_op(insert);
	}
	sched_reset(irn);
	sched_add_before(insert, irn);
}

static void
sched_put_before(const spill_ilp_t * si, ir_node * insert, ir_node * irn)
{
  if(is_Block(insert)) {
	  insert = sched_block_last_noncf(si, insert);
  } else {
	  insert = sched_next_nonproj(insert, 0);
	  insert = sched_prev(insert);
  }
  sched_reset(irn);
  sched_add_after(insert, irn);
}

static ir_node *
next_post_remat(const ir_node * irn)
{
	op_t      *op;
    ir_node   *next;

	if(is_Block(irn)) {
		next = sched_block_first_nonphi(irn);
	} else {
		next = sched_next_op(irn);
	}

	if(sched_is_end(next))
		return NULL;

	op = get_irn_link(next);
	if(op->is_remat && !op->attr.remat.pre) {
		return next;
	}

	return NULL;
}


static ir_node *
next_pre_remat(const spill_ilp_t * si, const ir_node * irn)
{
	op_t      *op;
	ir_node   *ret;

	if(is_Block(irn)) {
		ret = sched_block_last_noncf(si, irn);
		ret = sched_next(ret);
		ret = sched_prev_op(ret);
	} else {
		ret = sched_prev_op(irn);
	}

	if(sched_is_end(ret) || is_Phi(ret))
		return NULL;

	op = (op_t*)get_irn_link(ret);
	if(op->is_remat && op->attr.remat.pre) {
		return ret;
	}

	return NULL;
}

/**
 * Tells you whether a @p remat can be placed before the irn @p pos
 */
static INLINE int
can_remat_before(const spill_ilp_t * si, const remat_t * remat, const ir_node * pos, const pset * live)
{
	const ir_node   *op = remat->op;
	const ir_node   *prev;
	int        n,
			   res = 1;

	if(is_Block(pos)) {
		prev = sched_block_last_noncf(si, pos);
		prev = sched_next_nonproj(prev, 0);
	} else {
		prev = sched_prev_op(pos);
	}
	/* do not remat if the rematted value is defined immediately before this op */
	if(prev == remat->op) {
		return 0;
	}

#if 0
	/* this should be just fine, the following OP will be using this value, right? */

	/* only remat AFTER the real definition of a value (?) */
	if(!value_is_defined_before(si, pos, remat->value)) {
//		ir_fprintf(stderr, "error(not defined)");
		return 0;
	}
#endif

	for(n=get_irn_arity(op)-1; n>=0 && res; --n) {
		const ir_node   *arg = get_irn_n(op, n);

		if(opt_no_enlarge_liveness) {
			if(has_reg_class(si, arg) && live) {
				res &= pset_find_ptr((pset*)live, arg)?1:0;
			} else {
				res &= value_is_defined_before(si, pos, arg);
			}
		} else {
			res &= value_is_defined_before(si, pos, arg);
		}
	}

	return res;
}

/**
 * Tells you whether a @p remat can be placed after the irn @p pos
 */
static INLINE int
can_remat_after(const spill_ilp_t * si, const remat_t * remat, const ir_node * pos, const pset * live)
{
	if(is_Block(pos)) {
		pos = sched_block_first_nonphi(pos);
	} else {
		pos = sched_next_op(pos);
	}

	/* only remat AFTER the real definition of a value (?) */
	if(!value_is_defined_before(si, pos, remat->value)) {
		return 0;
	}

	return can_remat_before(si, remat, pos, live);
}

/**
 * Collect potetially rematerializable OPs
 */
static void
walker_remat_collector(ir_node * irn, void * data)
{
	spill_ilp_t    *si = data;

	if(!is_Block(irn) && !is_Phi(irn)) {
		DBG((si->dbg, LEVEL_4, "\t  Processing %+F\n", irn));
		get_remats_from_op(si, irn);
	}
}

/**
 * Inserts a copy of @p irn before @p pos
 */
static ir_node *
insert_copy_before(const spill_ilp_t * si, const ir_node * irn, ir_node * pos)
{
	ir_node     *bb;
	ir_node     *copy;

	bb = is_Block(pos)?pos:get_nodes_block(pos);
	copy = exact_copy(irn);

	set_phi_class(si->pc, copy, NULL);
	set_nodes_block(copy, bb);
	sched_put_before(si, pos, copy);

	return copy;
}

/**
 * Inserts a copy of @p irn after @p pos
 */
static ir_node *
insert_copy_after(const spill_ilp_t * si, const ir_node * irn, ir_node * pos)
{
	ir_node     *bb;
	ir_node     *copy;

	bb = is_Block(pos)?pos:get_nodes_block(pos);
	copy = exact_copy(irn);

	set_phi_class(si->pc, copy, NULL);
	set_nodes_block(copy, bb);
	sched_put_after(pos, copy);

	return copy;
}

static ir_node *
insert_remat_after(spill_ilp_t * si, const remat_t * remat, ir_node * pos, const pset * live)
{
	char     buf[256];

	if(can_remat_after(si, remat, pos, live)) {
		ir_node         *copy,
						*proj_copy;
		op_t            *op;

		DBG((si->dbg, LEVEL_3, "\t  >inserting remat2 %+F\n", remat->op));

		copy = insert_copy_after(si, remat->op, pos);

		ir_snprintf(buf, sizeof(buf), "remat2_%N_%N", copy, pos);
		op = obstack_alloc(si->obst, sizeof(*op));
		op->is_remat = 1;
		op->attr.remat.remat = remat;
		op->attr.remat.pre = 0;
		op->attr.remat.ilp = lpp_add_var_default(si->lpp, buf, lpp_binary, remat->cost*execution_frequency(si, pos), 0.0);

		set_irn_link(copy, op);
		pset_insert_ptr(si->all_possible_remats, copy);
		if(remat->proj) {
			proj_copy = insert_copy_after(si, remat->proj, copy);
			set_irn_n(proj_copy, 0, copy);
			set_irn_link(proj_copy, op);
			pset_insert_ptr(si->all_possible_remats, proj_copy);
		} else {
			proj_copy = NULL;
		}

		return copy;
	}

	return NULL;
}

static ir_node *
insert_remat_before(spill_ilp_t * si, const remat_t * remat, ir_node * pos, const pset * live)
{
	char     buf[256];

	if(can_remat_before(si, remat, pos, live)) {
		ir_node         *copy,
						*proj_copy;
		op_t            *op;

		DBG((si->dbg, LEVEL_3, "\t  >inserting remat %+F\n", remat->op));

		copy = insert_copy_before(si, remat->op, pos);

		ir_snprintf(buf, sizeof(buf), "remat_%N_%N", copy, pos);
		op = obstack_alloc(si->obst, sizeof(*op));
		op->is_remat = 1;
		op->attr.remat.remat = remat;
		op->attr.remat.pre = 1;
		op->attr.remat.ilp = lpp_add_var_default(si->lpp, buf, lpp_binary, remat->cost*execution_frequency(si, pos), 0.0);

		set_irn_link(copy, op);
		pset_insert_ptr(si->all_possible_remats, copy);
		if(remat->proj) {
			proj_copy = insert_copy_after(si, remat->proj, copy);
			set_irn_n(proj_copy, 0, copy);
			set_irn_link(proj_copy, op);
			pset_insert_ptr(si->all_possible_remats, proj_copy);
		} else {
			proj_copy = NULL;
		}

		return copy;
	}

	return NULL;
}

static int
get_block_n_succs(const ir_node *block) {
	const ir_edge_t *edge;

	assert(edges_activated(current_ir_graph));

	edge = get_block_succ_first(block);
	if (! edge)
		return 0;

	edge = get_block_succ_next(block, edge);
	return edge ? 2 : 1;
}

static int
is_start_block(const ir_node * bb)
{
	return get_irg_start_block(get_irn_irg(bb)) == bb;
}

static int
is_merge_edge(const ir_node * bb)
{
	if(is_start_block(bb))
		return 0;

	if(opt_goodwin)
		return get_block_n_succs(bb) == 1;
	else
		return 1;
}

static int
is_diverge_edge(const ir_node * bb)
{
	if(is_start_block(bb))
		return 0;

	if(opt_goodwin)
		return get_Block_n_cfgpreds(bb) == 1;
	else
		return 1;
}

static void
get_live_end(spill_ilp_t * si, ir_node * bb, pset * live)
{
	ir_node        *irn;
	int i;

	be_lv_foreach(si->lv, bb, be_lv_state_end, i) {
		irn = be_lv_get_irn(si->lv, bb, i);

		if (has_reg_class(si, irn) && !pset_find_ptr(si->all_possible_remats, irn)) {
			pset_insert_ptr(live, irn);
		}
	}

	irn = sched_last(bb);

	/* all values eaten by control flow operations are also live until the end of the block */
	sched_foreach_reverse(bb, irn) {
		int  i;

		if(!sched_skip_cf_predicator(irn, si->birg->main_env->arch_env)) break;

		for(i=get_irn_arity(irn)-1; i>=0; --i) {
			ir_node *arg = get_irn_n(irn,i);

			if(has_reg_class(si, arg)) {
				pset_insert_ptr(live, arg);
			}
		}
	}
	/*
	 * find values that are used by remats at end of block
	 * and insert them into live set
	 */
	foreach_pre_remat(si, bb, irn) {
		int       n;

		for (n=get_irn_arity(irn)-1; n>=0; --n) {
			ir_node        *remat_arg = get_irn_n(irn, n);

			if(!has_reg_class(si, remat_arg)) continue;

			/* if value is becoming live through use by remat */
			if(!pset_find_ptr(live, remat_arg)) {
				DBG((si->dbg, LEVEL_4, "  value %+F becoming live through use by remat at end of block %+F\n", remat_arg, irn));

				pset_insert_ptr(live, remat_arg);
			}
		}
	}
}

static void
walker_regclass_copy_insertor(ir_node * irn, void * data)
{
	spill_ilp_t    *si = data;

	if(is_Phi(irn) && has_reg_class(si, irn)) {
		int n;

		for(n=get_irn_arity(irn)-1; n>=0; --n) {
			ir_node  *phi_arg = get_irn_n(irn, n);
			ir_node  *bb = get_Block_cfgpred_block(get_nodes_block(irn), n);

			if(!has_reg_class(si, phi_arg)) {
				ir_node   *copy = be_new_Copy(si->cls, si->birg->irg, bb, phi_arg);
				ir_node   *pos = sched_block_last_noncf(si, bb);
				op_t      *op = obstack_alloc(si->obst, sizeof(*op));

				DBG((si->dbg, LEVEL_2, "\t copy to my regclass for arg %+F of %+F\n", phi_arg, irn));
				sched_add_after(pos, copy);
				set_irn_n(irn, n, copy);

				op->is_remat = 0;
				op->attr.live_range.args.reloads = NULL;
				op->attr.live_range.ilp = ILP_UNDEF;
				set_irn_link(copy, op);
			}
		}
	}
}

/**
 * Insert (so far unused) remats into the irg to
 * recompute the potential liveness of all values
 */
static void
walker_remat_insertor(ir_node * bb, void * data)
{
	spill_ilp_t    *si = data;
	ir_node        *irn;
	int             n, i;
	pset           *live;
	pset           *post_remats;
	remat_t        *remat;

	/* skip start block, no remats to do there */
	if(is_start_block(bb)) return;

	DBG((si->dbg, LEVEL_3, "\t Entering %+F\n\n", bb));

	live = pset_new_ptr_default();
	be_lv_foreach(si->lv, bb, be_lv_state_end, i) {
		ir_node        *value = be_lv_get_irn(si->lv, bb, i);

		/* add remats at end of block */
		if (has_reg_class(si, value)) {
			pset_insert_ptr(live, value);
		}
	}

	irn = sched_last(bb);
	while(!sched_is_end(irn)) {
		ir_node   *next;
		pset      *args;
		ir_node   *arg;
		pset      *used;

		next = sched_prev(irn);

		/* delete defined value from live set */
		if(has_reg_class(si, irn)) {
			pset_remove_ptr(live, irn);
		}

		if(is_Phi(irn) || is_Proj(irn)) {
			irn = next;
			continue;
		}

		args = pset_new_ptr_default();
		used = pset_new_ptr_default();

		/* collect arguments of op and set args of op already live in epilog */
		for (n = get_irn_arity(irn)-1; n>=0; --n) {
			ir_node        *arg = get_irn_n(irn, n);

			pset_insert_ptr(args, arg);
			if(has_reg_class(si, arg)) {
				pset_insert_ptr(live, arg);
				pset_insert_ptr(used, arg);
			}
		}

		/* insert all possible remats before irn */
		pset_foreach(args, arg) {
			remat_info_t   *remat_info,
						    query;

			/* continue if the operand has the wrong reg class */
			if(!has_reg_class(si, arg))
				continue;

			query.irn = arg;
			query.remats = NULL;
			query.remats_by_operand = NULL;
			remat_info = set_find(si->remat_info, &query, sizeof(query), HASH_PTR(arg));

			if(!remat_info) {
				continue;
			}

			if(remat_info->remats) {
				pset_foreach(remat_info->remats, remat) {
					ir_node  *remat_irn = NULL;

					DBG((si->dbg, LEVEL_4, "\t  considering remat %+F for arg %+F\n", remat->op, arg));
					remat_irn = insert_remat_before(si, remat, irn, live);

					if(remat_irn) {
						for(n=get_irn_arity(remat_irn)-1; n>=0; --n) {
							ir_node  *remat_arg = get_irn_n(remat_irn, n);

							/* collect args of remats which are not args of op */
							if(has_reg_class(si, remat_arg) && !pset_find_ptr(args, remat_arg)) {
								pset_insert_ptr(used, remat_arg);
							}
						}
					}
				}
			}
		}

		/* do not place post remats after jumps */
		if(sched_skip_cf_predicator(irn, si->birg->main_env->arch_env)) {
			del_pset(used);
			del_pset(args);
			break;
		}

		/* insert all possible remats after irn */
		post_remats = pset_new_ptr_default();
		pset_foreach(used, arg) {
			remat_info_t   *remat_info,
						    query;

			/* continue if the operand has the wrong reg class */
			if(!has_reg_class(si, arg))
				continue;

			query.irn = arg;
			query.remats = NULL;
			query.remats_by_operand = NULL;
			remat_info = set_find(si->remat_info, &query, sizeof(query), HASH_PTR(arg));

			if(!remat_info) {
				continue;
			}

			if(remat_info->remats_by_operand) {
				pset_foreach(remat_info->remats_by_operand, remat) {
					/* do not insert remats producing the same value as one of the operands */
					if(!pset_find_ptr(args, remat->value)) {
						DBG((si->dbg, LEVEL_4, "\t  considering remat %+F with arg %+F\n", remat->op, arg));

						/* only remat values that can be used by real ops */
						if(!opt_remat_while_live || pset_find_ptr(live, remat->value)) {
							pset_insert_ptr(post_remats, remat);
						}
					}
				}
			}
		}
		pset_foreach(post_remats, remat) {
			insert_remat_after(si, remat, irn, live);
		}
		del_pset(post_remats);

		del_pset(used);
		del_pset(args);
		irn = next;
	}

	/* add remats at end if successor has multiple predecessors */
	if(is_merge_edge(bb)) {
		pset     *live_out = pset_new_ptr_default();
		ir_node  *value;

		get_live_end(si, bb, live_out);

		/* add remats at end of block */
		pset_foreach(live_out, value) {
			remat_info_t   *remat_info,
						   query;

			query.irn = value;
			query.remats = NULL;
			query.remats_by_operand = NULL;
			remat_info = set_find(si->remat_info, &query, sizeof(query), HASH_PTR(value));

			if(remat_info && remat_info->remats) {
				pset_foreach(remat_info->remats, remat) {
					DBG((si->dbg, LEVEL_4, "\t  considering remat %+F at end of block %+F\n", remat->op, bb));

					insert_remat_before(si, remat, bb, live_out);
				}
			}
		}
		del_pset(live_out);
	}

	if(is_diverge_edge(bb)) {
		pset     *live_in = pset_new_ptr_default();
		ir_node  *value;

		be_lv_foreach(si->lv, bb, be_lv_state_in, i) {
			value = be_lv_get_irn(si->lv, bb, i);

			if(has_reg_class(si, value)) {
				pset_insert_ptr(live_in, value);
			}
		}
		/* add phis to live_in */
		sched_foreach(bb, value) {
			if(!is_Phi(value)) break;

			if(has_reg_class(si, value)) {
				pset_insert_ptr(live_in, value);
			}
		}

		/* add remat2s at beginning of block */
		post_remats = pset_new_ptr_default();
		pset_foreach(live_in, value) {
			remat_info_t   *remat_info,
						   query;

			query.irn = value;
			query.remats = NULL;
			query.remats_by_operand = NULL;
			remat_info = set_find(si->remat_info, &query, sizeof(query), HASH_PTR(value));

			if(remat_info && remat_info->remats_by_operand) {
				pset_foreach(remat_info->remats_by_operand, remat) {
					DBG((si->dbg, LEVEL_4, "\t  considering remat2 %+F at beginning of block %+F\n", remat->op, bb));

					/* put the remat here if all its args are available and result is still live */
					if(!opt_remat_while_live || pset_find_ptr(live_in, remat->value)) {
						pset_insert_ptr(post_remats, remat);
					}
				}
			}
		}
		pset_foreach(post_remats, remat) {
			insert_remat_after(si, remat, bb, live_in);
		}
		del_pset(post_remats);
		del_pset(live_in);
	}
}

static int
can_be_copied(const ir_node * bb, const ir_node * irn)
{
	const ir_edge_t *edge    = get_block_succ_first(bb);
	const ir_node   *next_bb = edge->src;
	int             pos      = edge->pos;
	const ir_node   *phi;

	assert(is_merge_edge(bb));

	sched_foreach(next_bb, phi) {
		const ir_node  *phi_arg;

		if(!is_Phi(phi)) break;

		phi_arg = get_irn_n(phi, pos);

		if(phi_arg == irn) {
			return 1;
		}
	}
	return 0;
}

/**
 * Initialize additional node info
 */
static void
luke_initializer(ir_node * bb, void * data)
{
	spill_ilp_t    *si = (spill_ilp_t*)data;
	spill_bb_t     *spill_bb;
	ir_node        *irn;

	spill_bb = obstack_alloc(si->obst, sizeof(*spill_bb));
	set_irn_link(bb, spill_bb);

	sched_foreach(bb, irn) {
		op_t      *op;

		op = obstack_alloc(si->obst, sizeof(*op));
		op->is_remat = 0;
		op->attr.live_range.ilp = ILP_UNDEF;
		if(is_Phi(irn)) {
			if(opt_memcopies) {
				op->attr.live_range.args.copies = obstack_alloc(si->obst, sizeof(*op->attr.live_range.args.copies) * get_irn_arity(irn));
				memset(op->attr.live_range.args.copies, 0xFF, sizeof(*op->attr.live_range.args.copies) * get_irn_arity(irn));
			}
		} else if(!is_Proj(irn)) {
			op->attr.live_range.args.reloads = obstack_alloc(si->obst, sizeof(*op->attr.live_range.args.reloads) * get_irn_arity(irn));
			memset(op->attr.live_range.args.reloads, 0xFF, sizeof(*op->attr.live_range.args.reloads) * get_irn_arity(irn));
		} else {
			op->attr.live_range.args.reloads = NULL;
		}
		set_irn_link(irn, op);
	}
}


/**
 * Preparation of blocks' ends for Luke Blockwalker(tm)(R)
 */
static void
luke_endwalker(ir_node * bb, void * data)
{
	spill_ilp_t    *si = (spill_ilp_t*)data;
	pset           *live;
	pset           *use_end;
	char            buf[256];
	ilp_cst_t       cst;
	ir_node        *irn;
	spill_bb_t     *spill_bb = get_irn_link(bb);
	int             i;

	live = pset_new_ptr_default();
	use_end = pset_new_ptr_default();

	be_lv_foreach(si->lv, bb, be_lv_state_end, i) {
		irn = be_lv_get_irn(si->lv, bb, i);
		if (has_reg_class(si, irn) && !pset_find_ptr(si->all_possible_remats, irn)) {
			pset_insert_ptr(live, irn);
		}
	}
	/*
	 * find values that are used by remats at end of block
	 * and insert them into live set
	 */
	foreach_pre_remat(si, bb, irn) {
		int       n;

		for (n=get_irn_arity(irn)-1; n>=0; --n) {
			ir_node        *remat_arg = get_irn_n(irn, n);

			if(has_reg_class(si, remat_arg)) {
				pset_insert_ptr(live, remat_arg);
			}
		}
	}

	/* collect values used by cond jumps etc. at bb end (use_end) -> always live */
	/* their reg_out must always be set */
	sched_foreach_reverse(bb, irn) {
		int   n;

		if(!sched_skip_cf_predicator(irn, si->birg->main_env->arch_env)) break;

		for (n=get_irn_arity(irn)-1; n>=0; --n) {
			ir_node        *irn_arg = get_irn_n(irn, n);

			if(has_reg_class(si, irn_arg)) {
				pset_insert_ptr(use_end, irn_arg);
			}
		}
	}

	ir_snprintf(buf, sizeof(buf), "check_end_%N", bb);
	//cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, si->n_regs);
	cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, si->n_regs - pset_count(use_end));

	spill_bb->ilp = new_set(cmp_spill, pset_count(live)+pset_count(use_end));

	/* if this is a merge edge we can reload at the end of this block */
	if(is_merge_edge(bb)) {
		spill_bb->reloads = new_set(cmp_keyval, pset_count(live)+pset_count(use_end));
	} else if(pset_count(use_end)){
		spill_bb->reloads = new_set(cmp_keyval, pset_count(use_end));
	} else {
		spill_bb->reloads = NULL;
	}

	pset_foreach(live,irn) {
		spill_t     query,
					*spill;
		double      spill_cost;
		int         default_spilled;


		/* handle values used by control flow nodes later separately */
		if(pset_find_ptr(use_end, irn)) continue;

		query.irn = irn;
		spill = set_insert(spill_bb->ilp, &query, sizeof(query), HASH_PTR(irn));

		spill_cost = is_Unknown(irn)?0.0001:opt_cost_spill*execution_frequency(si, bb);

		ir_snprintf(buf, sizeof(buf), "reg_out_%N_%N", irn, bb);
		spill->reg_out = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 0.0);
		lpp_set_factor_fast(si->lpp, cst, spill->reg_out, 1.0);

		ir_snprintf(buf, sizeof(buf), "mem_out_%N_%N", irn, bb);
		spill->mem_out = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 1.0);

		ir_snprintf(buf, sizeof(buf), "spill_%N_%N", irn, bb);
		/* by default spill value right after definition */
		default_spilled = be_is_live_in(si->lv, bb, irn) || is_Phi(irn);
		spill->spill    = lpp_add_var_default(si->lpp, buf, lpp_binary, spill_cost, !default_spilled);

		if(is_merge_edge(bb)) {
			ilp_var_t   reload;
			ilp_cst_t   rel_cst;

			ir_snprintf(buf, sizeof(buf), "reload_%N_%N", bb, irn);
			reload = lpp_add_var_default(si->lpp, buf, lpp_binary, opt_cost_reload*execution_frequency(si, bb), can_be_copied(bb, irn));
			set_insert_keyval(spill_bb->reloads, irn, INT_TO_PTR(reload));

			/* reload <= mem_out */
			rel_cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
			lpp_set_factor_fast(si->lpp, rel_cst, reload, 1.0);
			lpp_set_factor_fast(si->lpp, rel_cst, spill->mem_out, -1.0);
		}

		spill->reg_in = ILP_UNDEF;
		spill->mem_in = ILP_UNDEF;
	}

	pset_foreach(use_end,irn) {
		spill_t     query,
					*spill;
		double      spill_cost;
		ilp_cst_t   end_use_req,
					rel_cst;
		ilp_var_t   reload;
		int         default_spilled;

		query.irn = irn;
		spill = set_insert(spill_bb->ilp, &query, sizeof(query), HASH_PTR(irn));

		spill_cost = is_Unknown(irn)?0.0001:opt_cost_spill*execution_frequency(si, bb);

		ir_snprintf(buf, sizeof(buf), "reg_out_%N_%N", irn, bb);
		spill->reg_out = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 1.0);

		ir_snprintf(buf, sizeof(buf), "mem_out_%N_%N", irn, bb);
		spill->mem_out = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 1.0);

		ir_snprintf(buf, sizeof(buf), "spill_%N_%N", irn, bb);
		default_spilled = be_is_live_in(si->lv, bb, irn) || is_Phi(irn);
		spill->spill    = lpp_add_var_default(si->lpp, buf, lpp_binary, spill_cost, !default_spilled);

		/* reload for use be control flow op */
		ir_snprintf(buf, sizeof(buf), "reload_%N_%N", bb, irn);
		reload = lpp_add_var_default(si->lpp, buf, lpp_binary, opt_cost_reload*execution_frequency(si, bb), 1.0);
		set_insert_keyval(spill_bb->reloads, irn, INT_TO_PTR(reload));

		/* reload <= mem_out */
		rel_cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
		lpp_set_factor_fast(si->lpp, rel_cst, reload, 1.0);
		lpp_set_factor_fast(si->lpp, rel_cst, spill->mem_out, -1.0);

		spill->reg_in = ILP_UNDEF;
		spill->mem_in = ILP_UNDEF;

		ir_snprintf(buf, sizeof(buf), "req_cf_end_%N_%N", irn, bb);
		end_use_req = lpp_add_cst_uniq(si->lpp, buf, lpp_equal, 1);
		lpp_set_factor_fast(si->lpp, end_use_req, spill->reg_out, 1.0);
	}

	del_pset(live);
	del_pset(use_end);
}

#ifndef NDEBUG
/**
 * Find a remat of value @p value in the epilog of @p pos
 */
static ir_node *
find_post_remat(const ir_node * value, const ir_node * pos)
{
	while((pos = next_post_remat(pos)) != NULL) {
		op_t   *op;

		op = get_irn_link(pos);
		assert(op->is_remat && !op->attr.remat.pre);

		if(op->attr.remat.remat->value == value)
			return (ir_node*)pos;

#if 0
	const ir_edge_t *edge;
		foreach_out_edge(pos, edge) {
			ir_node   *proj = get_edge_src_irn(edge);
			assert(is_Proj(proj));
		}
#endif

	}

	return NULL;
}
#endif

static spill_t *
add_to_spill_bb(spill_ilp_t * si, ir_node * bb, ir_node * irn)
{
	spill_bb_t  *spill_bb = get_irn_link(bb);
	spill_t     *spill,
				 query;
	char         buf[256];
	int          default_spilled;

	query.irn = irn;
	spill = set_find(spill_bb->ilp, &query, sizeof(query), HASH_PTR(irn));
	if(!spill) {
		double   spill_cost = is_Unknown(irn)?0.0001:opt_cost_spill*execution_frequency(si, bb);

		spill = set_insert(spill_bb->ilp, &query, sizeof(query), HASH_PTR(irn));

		spill->reg_out = ILP_UNDEF;
		spill->reg_in  = ILP_UNDEF;
		spill->mem_in  = ILP_UNDEF;

		ir_snprintf(buf, sizeof(buf), "mem_out_%N_%N", irn, bb);
		spill->mem_out = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 1.0);

		ir_snprintf(buf, sizeof(buf), "spill_%N_%N", irn, bb);
		default_spilled = be_is_live_in(si->lv, bb, irn) || is_Phi(irn);
		spill->spill    = lpp_add_var_default(si->lpp, buf, lpp_binary, spill_cost, !default_spilled);
	}

	return spill;
}

/**
 *  Inserts ILP-constraints and variables for memory copying before the given position
 */
static void
insert_mem_copy_position(spill_ilp_t * si, pset * live, const ir_node * block)
{
	const ir_node    *succ;
	const ir_edge_t  *edge;
	spill_bb_t       *spill_bb = get_irn_link(block);
	ir_node          *phi;
	int               pos;
	ilp_cst_t         cst;
	ilp_var_t         copyreg;
	char              buf[256];
	ir_node          *tmp;


	assert(edges_activated(current_ir_graph));

	edge = get_block_succ_first(block);
	if(!edge) return;

	succ = edge->src;
	pos = edge->pos;

	edge = get_block_succ_next(block, edge);
	/* next block can only contain phis, if this is a merge edge */
	if(edge) return;

	ir_snprintf(buf, sizeof(buf), "copyreg_%N", block);
	copyreg = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 1.0);

	ir_snprintf(buf, sizeof(buf), "check_copyreg_%N", block);
	cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, si->n_regs);

	pset_foreach(live, tmp) {
		spill_t  *spill;
#if 0
		op_t  *op = get_irn_link(irn);
		lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.ilp, 1.0);
#endif
		spill = set_find_spill(spill_bb->ilp, tmp);
		assert(spill);

		lpp_set_factor_fast(si->lpp, cst, spill->reg_out, 1.0);
	}
	lpp_set_factor_fast(si->lpp, cst, copyreg, 1.0);

	sched_foreach(succ, phi) {
		const ir_node  *to_copy;
		op_t           *to_copy_op;
		spill_t        *to_copy_spill;
		op_t           *phi_op = get_irn_link(phi);
		ilp_var_t       reload = ILP_UNDEF;


		if(!is_Phi(phi)) break;
		if(!has_reg_class(si, phi)) continue;

		to_copy = get_irn_n(phi, pos);
		to_copy_op = get_irn_link(to_copy);

		to_copy_spill = set_find_spill(spill_bb->ilp, to_copy);
		assert(to_copy_spill);

		if(spill_bb->reloads) {
			keyval_t *keyval = set_find_keyval(spill_bb->reloads, to_copy);

			if(keyval) {
				reload = PTR_TO_INT(keyval->val);
			}
		}

		ir_snprintf(buf, sizeof(buf), "req_copy_%N_%N_%N", block, phi, to_copy);
		cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

		/* copy - reg_out - reload - remat - live_range <= 0 */
		lpp_set_factor_fast(si->lpp, cst, phi_op->attr.live_range.args.copies[pos], 1.0);
		lpp_set_factor_fast(si->lpp, cst, to_copy_spill->reg_out, -1.0);
		if(reload != ILP_UNDEF) lpp_set_factor_fast(si->lpp, cst, reload, -1.0);
		lpp_set_factor_fast(si->lpp, cst, to_copy_op->attr.live_range.ilp, -1.0);
		foreach_pre_remat(si, block, tmp) {
			op_t     *remat_op = get_irn_link(tmp);
			if(remat_op->attr.remat.remat->value == to_copy) {
				lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
			}
		}

		ir_snprintf(buf, sizeof(buf), "copyreg_%N_%N_%N", block, phi, to_copy);
		cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

		/* copy - reg_out - copyreg <= 0 */
		lpp_set_factor_fast(si->lpp, cst, phi_op->attr.live_range.args.copies[pos], 1.0);
		lpp_set_factor_fast(si->lpp, cst, to_copy_spill->reg_out, -1.0);
		lpp_set_factor_fast(si->lpp, cst, copyreg, -1.0);
	}
}


/**
 * Walk all irg blocks and emit this ILP
 */
static void
luke_blockwalker(ir_node * bb, void * data)
{
	spill_ilp_t    *si = (spill_ilp_t*)data;
	ir_node        *irn;
	pset           *live;
	char            buf[256];
	ilp_cst_t       cst;
	spill_bb_t     *spill_bb = get_irn_link(bb);
	ir_node        *tmp;
	spill_t        *spill;
	pset           *defs = pset_new_ptr_default();
	const arch_env_t *arch_env = si->birg->main_env->arch_env;

	live = pset_new_ptr_default();

	/****************************************
	 *      B A S I C  B L O C K  E N D
	 ***************************************/


	/* init live values at end of block */
	get_live_end(si, bb, live);

	pset_foreach(live, irn) {
		op_t           *op;
		ilp_var_t       reload = ILP_UNDEF;

		spill = set_find_spill(spill_bb->ilp, irn);
		assert(spill);

		if(spill_bb->reloads) {
			keyval_t *keyval = set_find_keyval(spill_bb->reloads, irn);

			if(keyval) {
				reload = PTR_TO_INT(keyval->val);
			}
		}

		op = get_irn_link(irn);
		assert(!op->is_remat);

		ir_snprintf(buf, sizeof(buf), "lr_%N_%N", irn, bb);
		op->attr.live_range.ilp = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 0.0);
		op->attr.live_range.op = bb;

		ir_snprintf(buf, sizeof(buf), "reg_out_%N_%N", bb, irn);
		cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

		/* reg_out - reload - remat - live_range <= 0 */
		lpp_set_factor_fast(si->lpp, cst, spill->reg_out, 1.0);
		if(reload != ILP_UNDEF) lpp_set_factor_fast(si->lpp, cst, reload, -1.0);
		lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.ilp, -1.0);
		foreach_pre_remat(si, bb, tmp) {
			op_t     *remat_op = get_irn_link(tmp);
			if(remat_op->attr.remat.remat->value == irn) {
				lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
			}
		}
		ir_snprintf(buf, sizeof(buf), "reg_out2_%N_%N", bb, irn);
		cst = lpp_add_cst_uniq(si->lpp, buf, lpp_greater, 0.0);

		/* value may only die at bb end if it is used for a mem copy */
		/* reg_out + \sum copy - reload - remat - live_range >= 0 */
		lpp_set_factor_fast(si->lpp, cst, spill->reg_out, 1.0);
		if(reload != ILP_UNDEF) lpp_set_factor_fast(si->lpp, cst, reload, -1.0);
		lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.ilp, -1.0);
		foreach_pre_remat(si, bb, tmp) {
			op_t     *remat_op = get_irn_link(tmp);
			if(remat_op->attr.remat.remat->value == irn) {
				lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
			}
		}
		if(is_merge_edge(bb)) {
			const ir_edge_t *edge = get_block_succ_first(bb);
			const ir_node   *next_bb = edge->src;
			int              pos = edge->pos;
			const ir_node   *phi;

			sched_foreach(next_bb, phi) {
				const ir_node  *phi_arg;

				if(!is_Phi(phi)) break;

				phi_arg = get_irn_n(phi, pos);

				if(phi_arg == irn) {
					op_t      *phi_op = get_irn_link(phi);
					ilp_var_t  copy = phi_op->attr.live_range.args.copies[pos];

					lpp_set_factor_fast(si->lpp, cst, copy, 1.0);
				}
			}
		}
	}

	if(opt_memcopies)
		insert_mem_copy_position(si, live, bb);

	/*
	 * assure the remat args are available
	 */
	foreach_pre_remat(si, bb, tmp) {
		op_t     *remat_op = get_irn_link(tmp);
		int       n;

		for (n=get_irn_arity(tmp)-1; n>=0; --n) {
			ir_node        *remat_arg = get_irn_n(tmp, n);
			op_t           *arg_op = get_irn_link(remat_arg);

			if(!has_reg_class(si, remat_arg)) continue;

			spill = set_find_spill(spill_bb->ilp, remat_arg);
			assert(spill);

			/* arguments of remats have to be live until the very end of the block
			 * remat = reg_out(remat_arg) and (reload(remat_arg) or live_range(remat_arg)),
			 * no remats, they could be in wrong order
			 */

			ir_snprintf(buf, sizeof(buf), "req_remat_%N_arg_%N", tmp, remat_arg);
			cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

			lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 3.0);
			lpp_set_factor_fast(si->lpp, cst, spill->reg_out, -2.0);
			lpp_set_factor_fast(si->lpp, cst, arg_op->attr.live_range.ilp, -1.0);

			/* use reload placed for this argument */
			if(spill_bb->reloads) {
				keyval_t *keyval = set_find_keyval(spill_bb->reloads, remat_arg);

				if(keyval) {
					ilp_var_t       reload = PTR_TO_INT(keyval->val);

					lpp_set_factor_fast(si->lpp, cst, reload, -1.0);
				}
			}
		}
	}
	DBG((si->dbg, LEVEL_4, "\t   %d values live at end of block %+F\n", pset_count(live), bb));




	/**************************************
	 *    B A S I C  B L O C K  B O D Y
	 **************************************/

	sched_foreach_reverse_from(sched_block_last_noncf(si, bb), irn) {
		op_t       *op;
		op_t       *tmp_op;
		int         n,
					u = 0,
					d = 0;
		ilp_cst_t	check_pre,
					check_post;
		set        *args;
		pset       *used;
		pset       *remat_defs;
		keyval_t   *keyval;
		ilp_cst_t   one_memoperand = -1;

		/* iterate only until first phi */
		if(is_Phi(irn))
			break;

		op = get_irn_link(irn);
		/* skip remats */
		if(op->is_remat) continue;

		DBG((si->dbg, LEVEL_4, "\t  at node %+F\n", irn));

		/* collect defined values */
		if(has_reg_class(si, irn)) {
			pset_insert_ptr(defs, irn);
		}

		/* skip projs */
		if(is_Proj(irn)) continue;

		/*
		 * init set of irn's arguments
		 * and all possibly used values around this op
		 * and values defined by post remats
		 */
		args =       new_set(cmp_keyval, get_irn_arity(irn));
		used =       pset_new_ptr(pset_count(live) + get_irn_arity(irn));
		remat_defs = pset_new_ptr(pset_count(live));

		if(!is_start_block(bb) || !be_is_Barrier(irn)) {
			for (n=get_irn_arity(irn)-1; n>=0; --n) {
				ir_node        *irn_arg = get_irn_n(irn, n);
				if(has_reg_class(si, irn_arg)) {
					set_insert_keyval(args, irn_arg, (void*)n);
					pset_insert_ptr(used, irn_arg);
				}
			}
			foreach_post_remat(irn, tmp) {
				op_t    *remat_op = get_irn_link(tmp);

				pset_insert_ptr(remat_defs, remat_op->attr.remat.remat->value);

				for (n=get_irn_arity(tmp)-1; n>=0; --n) {
					ir_node        *remat_arg = get_irn_n(tmp, n);
					if(has_reg_class(si, remat_arg)) {
						pset_insert_ptr(used, remat_arg);
					}
				}
			}
			foreach_pre_remat(si, irn, tmp) {
				for (n=get_irn_arity(tmp)-1; n>=0; --n) {
					ir_node        *remat_arg = get_irn_n(tmp, n);
					if(has_reg_class(si, remat_arg)) {
						pset_insert_ptr(used, remat_arg);
					}
				}
			}
		}

		/**********************************
		 *   I N  E P I L O G  O F  irn
		 **********************************/

		/* ensure each dying value is used by only one post remat */
		pset_foreach(used, tmp) {
			ir_node     *value = tmp;
			op_t        *value_op = get_irn_link(value);
			ir_node     *remat;
			int          n_remats = 0;

			cst = ILP_UNDEF;
			foreach_post_remat(irn, remat) {
				op_t  *remat_op = get_irn_link(remat);

				for(n=get_irn_arity(remat)-1; n>=0; --n) {
					ir_node   *remat_arg = get_irn_n(remat, n);

					/* if value is used by this remat add it to constraint */
					if(remat_arg == value) {
						if(n_remats == 0) {
							/* sum remat2s <= 1 + n_remats*live_range */
							ir_snprintf(buf, sizeof(buf), "dying_lr_%N_%N", value, irn);
							cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 1.0);
						}

						n_remats++;
						lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
						break;
					}
				}
			}

			if(pset_find_ptr(live, value) && cst != ILP_UNDEF) {
				lpp_set_factor_fast(si->lpp, cst, value_op->attr.live_range.ilp, -n_remats);
			}
		}

        /* ensure at least one value dies at post remat */
        foreach_post_remat(irn, tmp) {
            op_t     *remat_op = get_irn_link(tmp);
            pset     *remat_args = pset_new_ptr(get_irn_arity(tmp));
            ir_node  *remat_arg;

            for(n=get_irn_arity(tmp)-1; n>=0; --n) {
                remat_arg = get_irn_n(tmp, n);

                if(has_reg_class(si, remat_arg)) {

                    /* does arg always die at this op? */
                    if(!pset_find_ptr(live, remat_arg))
                        goto skip_one_must_die;

                    pset_insert_ptr(remat_args, remat_arg);
                }
            }

            /* remat + \sum live_range(remat_arg) <= |args| */
            ir_snprintf(buf, sizeof(buf), "one_must_die_%+F", tmp);
            cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, pset_count(remat_args));
            lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);

            pset_foreach(remat_args, remat_arg) {
                op_t  *arg_op = get_irn_link(remat_arg);

                lpp_set_factor_fast(si->lpp, cst, arg_op->attr.live_range.ilp, 1.0);
            }

skip_one_must_die:
            del_pset(remat_args);
        }

		/* new live ranges for values from L\U defined by post remats */
		pset_foreach(live, tmp) {
			ir_node     *value = tmp;
			op_t        *value_op = get_irn_link(value);

			if(!set_find_keyval(args, value) && !pset_find_ptr(defs, value)) {
				ilp_var_t    prev_lr = ILP_UNDEF;
				ir_node     *remat;

				if(pset_find_ptr(remat_defs, value)) {

					/* next_live_range <= prev_live_range + sum remat2s */
					ir_snprintf(buf, sizeof(buf), "next_lr_%N_%N", value, irn);
					cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

					ir_snprintf(buf, sizeof(buf), "lr_%N_%N", value, irn);
					prev_lr = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 0.0);

					lpp_set_factor_fast(si->lpp, cst, value_op->attr.live_range.ilp, 1.0);
					lpp_set_factor_fast(si->lpp, cst, prev_lr, -1.0);

					foreach_post_remat(irn, remat) {
						op_t        *remat_op = get_irn_link(remat);

						/* if value is being rematerialized by this remat */
						if(value == remat_op->attr.remat.remat->value) {
							lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
						}
					}

					value_op->attr.live_range.ilp = prev_lr;
					value_op->attr.live_range.op = irn;
				}
			}
		}

		/* requirements for post remats and start live ranges from L/U' for values dying here */
		foreach_post_remat(irn, tmp) {
			op_t        *remat_op = get_irn_link(tmp);
			int          n;

			for (n=get_irn_arity(tmp)-1; n>=0; --n) {
				ir_node        *remat_arg = get_irn_n(tmp, n);
				op_t           *arg_op = get_irn_link(remat_arg);

				if(!has_reg_class(si, remat_arg)) continue;

				/* only for values in L\U (TODO and D?), the others are handled with post_use */
				if(!pset_find_ptr(used, remat_arg)) {
					/* remat <= live_range(remat_arg) */
					ir_snprintf(buf, sizeof(buf), "req_remat2_%N_arg_%N", tmp, remat_arg);
					cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

					/* if value is becoming live through use by remat2 */
					if(!pset_find_ptr(live, remat_arg)) {
						ilp_var_t     lr;

						ir_snprintf(buf, sizeof(buf), "lr_%N_%N", remat_arg, irn);
						lr = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 0.0);

						arg_op->attr.live_range.ilp = lr;
						arg_op->attr.live_range.op = irn;

						DBG((si->dbg, LEVEL_3, "  value %+F becoming live through use by remat2 %+F\n", remat_arg, tmp));

						pset_insert_ptr(live, remat_arg);
						add_to_spill_bb(si, bb, remat_arg);
					}

					lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
					lpp_set_factor_fast(si->lpp, cst, arg_op->attr.live_range.ilp, -1.0);
				}
			}
		}

		d = pset_count(defs);
		DBG((si->dbg, LEVEL_4, "\t   %+F produces %d values in my register class\n", irn, d));

		/* count how many regs irn needs for arguments */
		u = set_count(args);


		/* check the register pressure in the epilog */
		/* sum_{L\U'} lr + sum_{U'} post_use <= k - |D| */
		ir_snprintf(buf, sizeof(buf), "check_post_%N", irn);
		check_post = lpp_add_cst_uniq(si->lpp, buf, lpp_less, si->n_regs - d);

		/* add L\U' to check_post */
		pset_foreach(live, tmp) {
			if(!pset_find_ptr(used, tmp) && !pset_find_ptr(defs, tmp)) {
				/* if a live value is not used by irn */
				tmp_op = get_irn_link(tmp);
				lpp_set_factor_fast(si->lpp, check_post, tmp_op->attr.live_range.ilp, 1.0);
			}
		}

		/***********************************************************
		 *  I T E R A T I O N  O V E R  U S E S  F O R  E P I L O G
		 **********************************************************/


		pset_foreach(used, tmp) {
			ilp_var_t       prev_lr;
			ilp_var_t       post_use;
			int             p = 0;
			spill_t        *spill;
			ir_node        *arg = tmp;
			op_t           *arg_op = get_irn_link(arg);
			ir_node        *remat;

			spill = add_to_spill_bb(si, bb, arg);

			/* new live range for each used value */
			ir_snprintf(buf, sizeof(buf), "lr_%N_%N", arg, irn);
			prev_lr = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 0.0);

			/* the epilog stuff - including post_use, check_post, check_post_remat */
			ir_snprintf(buf, sizeof(buf), "post_use_%N_%N", arg, irn);
			post_use = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 0.0);

			lpp_set_factor_fast(si->lpp, check_post, post_use, 1.0);

			/* arg is live throughout epilog if the next live_range is in a register */
			if(pset_find_ptr(live, arg)) {
				DBG((si->dbg, LEVEL_3, "\t  arg %+F is possibly live in epilog of %+F\n", arg, irn));

				/* post_use >= next_lr + remat */
				ir_snprintf(buf, sizeof(buf), "post_use_%N_%N-%d", arg, irn, p++);
				cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
				lpp_set_factor_fast(si->lpp, cst, post_use, -1.0);
				lpp_set_factor_fast(si->lpp, cst, arg_op->attr.live_range.ilp, 1.0);
			}

			/* forall post remat which use arg add a similar cst */
			foreach_post_remat(irn, remat) {
				int      n;

				for (n=get_irn_arity(remat)-1; n>=0; --n) {
					ir_node    *remat_arg = get_irn_n(remat, n);
					op_t       *remat_op = get_irn_link(remat);

					if(remat_arg == arg) {
						DBG((si->dbg, LEVEL_3, "\t  found remat with arg %+F in epilog of %+F\n", arg, irn));

						/* post_use >= remat */
						ir_snprintf(buf, sizeof(buf), "post_use_%N_%N-%d", arg, irn, p++);
						cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
						lpp_set_factor_fast(si->lpp, cst, post_use, -1.0);
						lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
					}
				}
			}

			/* if value is not an arg of op and not possibly defined by post remat
			 * then it may only die and not become live
			 */
			if(!set_find_keyval(args, arg)) {
				/* post_use <= prev_lr */
				ir_snprintf(buf, sizeof(buf), "req_post_use_%N_%N", arg, irn);
				cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
				lpp_set_factor_fast(si->lpp, cst, post_use, 1.0);
				lpp_set_factor_fast(si->lpp, cst, prev_lr, -1.0);

				if(!pset_find_ptr(remat_defs, arg) && pset_find_ptr(live, arg)) {
					/* next_lr <= prev_lr */
					ir_snprintf(buf, sizeof(buf), "next_lr_%N_%N", arg, irn);
					cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
					lpp_set_factor_fast(si->lpp, cst, arg_op->attr.live_range.ilp, 1.0);
					lpp_set_factor_fast(si->lpp, cst, prev_lr, -1.0);
				}
			}

			if(opt_memoperands && (!is_start_block(bb) || be_is_Barrier(irn))) {
				for(n = get_irn_arity(irn)-1; n>=0; --n) {
					if(get_irn_n(irn, n) == arg && arch_possible_memory_operand(arch_env, irn, n)) {
						ilp_var_t       memoperand;

						ir_snprintf(buf, sizeof(buf), "memoperand_%N_%d", irn, n);
						memoperand = lpp_add_var_default(si->lpp, buf, lpp_binary, opt_cost_memoperand*execution_frequency(si, bb), 0.0);
						set_insert_memoperand(si->memoperands, irn, n, memoperand);

						ir_snprintf(buf, sizeof(buf), "nolivepost_%N_%d", irn, n);
						cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 1.0);

						lpp_set_factor_fast(si->lpp, cst, memoperand, 1.0);
						lpp_set_factor_fast(si->lpp, cst, post_use, 1.0);
					}
				}
			}

			/* new live range begins for each used value */
			arg_op->attr.live_range.ilp = prev_lr;
			arg_op->attr.live_range.op = irn;

			pset_insert_ptr(live, arg);
		}

		/* just to be sure */
		check_post = ILP_UNDEF;

		/* allow original defintions to be removed */
		if(opt_repair_schedule) {
			pset_foreach(defs, tmp) {
				op_t      *tmp_op = get_irn_link(tmp);
				spill_t   *spill = set_find_spill(spill_bb->ilp, tmp);
#if 1
				ilp_var_t  delete;
				assert(spill);

				ir_snprintf(buf, sizeof(buf), "delete_%N", tmp);
				delete = lpp_add_var_default(si->lpp, buf, lpp_binary, -1.0*get_cost(si, irn)*execution_frequency(si, bb), 0.0);

				/* op may not be killed if its first live_range is 1 */
				ir_snprintf(buf, sizeof(buf), "killorig-lr_%N", tmp);
				cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 1.0);
				lpp_set_factor_fast(si->lpp, cst, delete, 1.0);
				lpp_set_factor_fast(si->lpp, cst, tmp_op->attr.live_range.ilp, 1.0);

				/* op may not be killed if it is spilled after the definition */
				ir_snprintf(buf, sizeof(buf), "killorig-spill_%N", tmp);
				cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 1.0);
				lpp_set_factor_fast(si->lpp, cst, delete, 1.0);
				lpp_set_factor_fast(si->lpp, cst, spill->spill, 1.0);
#else
				ilp_var_t  keep;
				assert(spill);

				ir_snprintf(buf, sizeof(buf), "keep_%N", tmp);
				keep = lpp_add_var_default(si->lpp, buf, lpp_binary, get_cost(si, irn)*execution_frequency(si, bb), 1.0);

				/* op may not be killed if its first live_range is 1 */
				ir_snprintf(buf, sizeof(buf), "killorig-lr_%N", tmp);
				cst = lpp_add_cst_uniq(si->lpp, buf, lpp_greater, 0.0);
				lpp_set_factor_fast(si->lpp, cst, keep, 1.0);
				lpp_set_factor_fast(si->lpp, cst, tmp_op->attr.live_range.ilp, -1.0);

				/* op may not be killed if it is spilled after the definition */
				ir_snprintf(buf, sizeof(buf), "killorig-spill_%N", tmp);
				cst = lpp_add_cst_uniq(si->lpp, buf, lpp_greater, 0.0);
				lpp_set_factor_fast(si->lpp, cst, keep, 1.0);
				lpp_set_factor_fast(si->lpp, cst, spill->spill, -1.0);
#endif
			}
		} else {
#if 0
			pset_foreach(defs, tmp) {
				op_t      *tmp_op = get_irn_link(tmp);
				spill_t   *spill = set_find_spill(spill_bb->ilp, tmp);
				assert(spill);

				/* live_range or spill should be 1
				   TODO: lr should be live until first use */
				ir_snprintf(buf, sizeof(buf), "nokillorig_%N", tmp);
				cst = lpp_add_cst_uniq(si->lpp, buf, lpp_greater, 1.0);
				lpp_set_factor_fast(si->lpp, cst, tmp_op->attr.live_range.ilp, 1.0);
				lpp_set_factor_fast(si->lpp, cst, spill->spill, 1.0);
			}
#endif
		}


		/******************
		 *   P R O L O G
		 ******************/

		/* check the register pressure in the prolog */
		/* sum_{L\U} lr <= k - |U| */
		ir_snprintf(buf, sizeof(buf), "check_pre_%N", irn);
		check_pre = lpp_add_cst_uniq(si->lpp, buf, lpp_less, si->n_regs - u);

		/* for the prolog remove defined values from the live set */
		pset_foreach(defs, tmp) {
			pset_remove_ptr(live, tmp);
		}

		if(opt_memoperands && (!is_start_block(bb) || be_is_Barrier(irn))) {
			ir_snprintf(buf, sizeof(buf), "one_memoperand_%N", irn);
			one_memoperand = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 1.0);
		}

		/***********************************************************
		 *  I T E R A T I O N  O V E R  A R G S  F O R  P R O L O G
		 **********************************************************/


		set_foreach(args, keyval) {
			spill_t          *spill;
			const ir_node    *arg = keyval->key;
			int               i = PTR_TO_INT(keyval->val);
			op_t             *arg_op = get_irn_link(arg);
			ilp_cst_t         requirements;
			int               n_memoperands;

			spill = set_find_spill(spill_bb->ilp, arg);
			assert(spill);

			ir_snprintf(buf, sizeof(buf), "reload_%N_%N", arg, irn);
			op->attr.live_range.args.reloads[i] = lpp_add_var_default(si->lpp, buf, lpp_binary, opt_cost_reload*execution_frequency(si, bb), 1.0);

			/* reload <= mem_out */
			ir_snprintf(buf, sizeof(buf), "req_reload_%N_%N", arg, irn);
			cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
			lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.args.reloads[i], 1.0);
			lpp_set_factor_fast(si->lpp, cst, spill->mem_out, -1.0);

			/* requirement: arg must be in register for use */
			/* reload + remat + live_range == 1 */
			ir_snprintf(buf, sizeof(buf), "req_%N_%N", irn, arg);
			requirements = lpp_add_cst_uniq(si->lpp, buf, lpp_equal, 1.0);

			lpp_set_factor_fast(si->lpp, requirements, arg_op->attr.live_range.ilp, 1.0);
			lpp_set_factor_fast(si->lpp, requirements, op->attr.live_range.args.reloads[i], 1.0);
			foreach_pre_remat(si, irn, tmp) {
				op_t     *remat_op = get_irn_link(tmp);
				if(remat_op->attr.remat.remat->value == arg) {
					lpp_set_factor_fast(si->lpp, requirements, remat_op->attr.remat.ilp, 1.0);
				}
			}

			if(opt_memoperands && (!is_start_block(bb) || be_is_Barrier(irn))) {
				n_memoperands = 0;
				for(n = get_irn_arity(irn)-1; n>=0; --n) {
					if(get_irn_n(irn, n) == arg) {
						n_memoperands++;
					}
				}
				for(n = get_irn_arity(irn)-1; n>=0; --n) {
					if(get_irn_n(irn, n) == arg && arch_possible_memory_operand(arch_env, irn, n)) {
						memoperand_t  *memoperand;
						memoperand = set_find_memoperand(si->memoperands, irn, n);

						/* memoperand <= mem_out */
						ir_snprintf(buf, sizeof(buf), "req_memoperand_%N_%d", irn, n);
						cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
						lpp_set_factor_fast(si->lpp, cst, memoperand->ilp, 1.0);
						lpp_set_factor_fast(si->lpp, cst, spill->mem_out, -1.0);

						/* the memoperand is only sufficient if it is used once by the op */
						if(n_memoperands == 1)
							lpp_set_factor_fast(si->lpp, requirements, memoperand->ilp, 1.0);

						lpp_set_factor_fast(si->lpp, one_memoperand, memoperand->ilp, 1.0);

						/* we have one more free register if we use a memory operand */
						lpp_set_factor_fast(si->lpp, check_pre, memoperand->ilp, -1.0);
					}
				}
			}
		}

		/* iterate over L\U */
		pset_foreach(live, tmp) {
			if(!set_find_keyval(args, tmp)) {
				/* if a live value is not used by irn */
				tmp_op = get_irn_link(tmp);
				lpp_set_factor_fast(si->lpp, check_pre, tmp_op->attr.live_range.ilp, 1.0);
			}
		}

		/* requirements for remats */
		foreach_pre_remat(si, irn, tmp) {
			op_t        *remat_op = get_irn_link(tmp);
			int          n;

			for (n=get_irn_arity(tmp)-1; n>=0; --n) {
				ir_node        *remat_arg = get_irn_n(tmp, n);
				op_t           *arg_op = get_irn_link(remat_arg);

				if(!has_reg_class(si, remat_arg)) continue;

				/* remat <= live_rang(remat_arg) [ + reload(remat_arg) ] */
				ir_snprintf(buf, sizeof(buf), "req_remat_%N_arg_%N", tmp, remat_arg);
				cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

				lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
				lpp_set_factor_fast(si->lpp, cst, arg_op->attr.live_range.ilp, -1.0);

				/* if remat arg is also used by current op then we can use reload placed for this argument */
				if((keyval = set_find_keyval(args, remat_arg)) != NULL) {
					int    index = (int)keyval->val;

					lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.args.reloads[index], -1.0);
				}
			}
		}




		/*************************
		 *  D O N E  W I T H  O P
		 *************************/

		DBG((si->dbg, LEVEL_4, "\t   %d values live at %+F\n", pset_count(live), irn));

		pset_foreach(live, tmp) {
			assert(has_reg_class(si, tmp));
		}

#ifndef NDEBUG
		for (n=get_irn_arity(irn)-1; n>=0; --n) {
			ir_node        *arg = get_irn_n(irn, n);

			assert(!find_post_remat(arg, irn) && "there should be no post remat for an argument of an op");
		}
#endif

		del_pset(remat_defs);
		del_pset(used);
		del_set(args);
		del_pset(defs);
		defs = pset_new_ptr_default();

		/* skip everything above barrier in start block */
		if(is_start_block(bb) && be_is_Barrier(irn)) {
			assert(pset_count(live) == 0);
			break;
		}

	}
	del_pset(defs);



	/***************************************
	 *   B E G I N N I N G  O F  B L O C K
	 ***************************************/


	/* we are now at the beginning of the basic block, there are only \Phis in front of us */
	DBG((si->dbg, LEVEL_3, "\t   %d values live at beginning of block %+F\n", pset_count(live), bb));

	pset_foreach(live, irn) {
		assert(is_Phi(irn) || get_nodes_block(irn) != bb);
	}

	/* construct mem_outs for all values */
	set_foreach(spill_bb->ilp, spill) {
		ir_snprintf(buf, sizeof(buf), "mem_out_%N_%N", spill->irn, bb);
		cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

		lpp_set_factor_fast(si->lpp, cst, spill->mem_out, 1.0);
		lpp_set_factor_fast(si->lpp, cst, spill->spill, -1.0);

		if(pset_find_ptr(live, spill->irn)) {
			int default_spilled;
			DBG((si->dbg, LEVEL_5, "\t     %+F live at beginning of block %+F\n", spill->irn, bb));

			ir_snprintf(buf, sizeof(buf), "mem_in_%N_%N", spill->irn, bb);
			default_spilled = be_is_live_in(si->lv, bb, spill->irn) || is_Phi(spill->irn);
			spill->mem_in   = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, default_spilled);
			lpp_set_factor_fast(si->lpp, cst, spill->mem_in, -1.0);

			if(opt_memcopies && is_Phi(spill->irn) && get_nodes_block(spill->irn) == bb) {
				int   n;
				op_t *op = get_irn_link(spill->irn);

				for(n=get_irn_arity(spill->irn)-1; n>=0; --n) {
					const ir_node  *arg = get_irn_n(spill->irn, n);
					double          freq=0.0;
					int             m;
					ilp_var_t       var;


					/* argument already done? */
					if(op->attr.live_range.args.copies[n] != ILP_UNDEF) continue;

					/* get sum of execution frequencies of blocks with the same phi argument */
					for(m=n; m>=0; --m) {
						const ir_node  *arg2 = get_irn_n(spill->irn, m);

						if(arg==arg2) {
							freq += execution_frequency(si, get_Block_cfgpred_block(bb, m));
						}
					}

					/* copies are not for free */
					ir_snprintf(buf, sizeof(buf), "copy_%N_%N", arg, spill->irn);
					var = lpp_add_var_default(si->lpp, buf, lpp_binary, opt_cost_spill * freq, 1.0);

					for(m=n; m>=0; --m) {
						const ir_node  *arg2 = get_irn_n(spill->irn, m);

						if(arg==arg2) {
							op->attr.live_range.args.copies[m] = var;
						}
					}

#if 0
					/* copy <= mem_in */
					ir_snprintf(buf, sizeof(buf), "nocopy_%N_%N", arg, spill->irn);
					cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
					lpp_set_factor_fast(si->lpp, cst, var, 1.0);
					lpp_set_factor_fast(si->lpp, cst, spill->mem_in, -1.0);
#endif
				}
			}
		}
	}

	foreach_post_remat(bb, tmp) {
		int         n;
		op_t       *remat_op = get_irn_link(tmp);
		pset       *remat_args = pset_new_ptr(get_irn_arity(tmp));
		ir_node    *remat_arg;

		for (n=get_irn_arity(tmp)-1; n>=0; --n) {
			remat_arg = get_irn_n(tmp, n);

			if(has_reg_class(si, remat_arg)) {
				pset_insert_ptr(remat_args, remat_arg);
			}
		}

		/* remat + \sum live_range(remat_arg) <= |args| */
		ir_snprintf(buf, sizeof(buf), "one_must_die_%N", tmp);
		cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, pset_count(remat_args));
		lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);

		pset_foreach(remat_args, remat_arg) {
			if(pset_find_ptr(live, remat_arg)) {
				op_t       *remat_arg_op = get_irn_link(remat_arg);
				lpp_set_factor_fast(si->lpp, cst, remat_arg_op->attr.live_range.ilp, 1.0);
			}
		}
		del_pset(remat_args);
	}

	foreach_post_remat(bb, tmp) {
		int  n;

		for(n=get_irn_arity(tmp)-1; n>=0; --n) {
			ir_node  *remat_arg = get_irn_n(tmp, n);

			/* if value is becoming live through use by remat2 */
			if(has_reg_class(si, remat_arg) && !pset_find_ptr(live, remat_arg)) {
				op_t       *remat_arg_op = get_irn_link(remat_arg);
				ilp_cst_t   nomem;

				DBG((si->dbg, LEVEL_3, "  value %+F becoming live through use by remat2 at bb start %+F\n", remat_arg, tmp));

				pset_insert_ptr(live, remat_arg);
				spill = add_to_spill_bb(si, bb, remat_arg);
				remat_arg_op->attr.live_range.ilp = ILP_UNDEF;

				/* we need reg_in and mem_in for this value; they will be referenced later */
				ir_snprintf(buf, sizeof(buf), "reg_in_%N_%N", remat_arg, bb);
				spill->reg_in = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 0.0);
				ir_snprintf(buf, sizeof(buf), "mem_in_%N_%N", remat_arg, bb);
				spill->mem_in = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 1.0);


				/* optimization: all memory stuff should be 0, for we do not want to insert reloads for remats */
				ir_snprintf(buf, sizeof(buf), "nomem_%N_%N", remat_arg, bb);
				nomem = lpp_add_cst_uniq(si->lpp, buf, lpp_equal, 0.0);
				lpp_set_factor_fast(si->lpp, nomem, spill->spill, 1.0);
			}
		}
	}

	/* L\U is empty at bb start */
	/* arg is live throughout epilog if it is reg_in into this block */

	/* check the register pressure at the beginning of the block
	 * including remats
	 */
	/* reg_in entspricht post_use */

	ir_snprintf(buf, sizeof(buf), "check_start_%N", bb);
	cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, si->n_regs);

	pset_foreach(live, irn) {
        ilp_cst_t  nospill;

		spill = set_find_spill(spill_bb->ilp, irn);
		assert(spill);

		ir_snprintf(buf, sizeof(buf), "reg_in_%N_%N", irn, bb);
		spill->reg_in = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 0.0);

		lpp_set_factor_fast(si->lpp, cst, spill->reg_in, 1.0);

		/* spill + mem_in <= 1 */
		ir_snprintf(buf, sizeof(buf), "nospill_%N_%N", irn, bb);
		nospill = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 1);

		lpp_set_factor_fast(si->lpp, nospill, spill->mem_in, 1.0);
		lpp_set_factor_fast(si->lpp, nospill, spill->spill, 1.0);

	} /* post_remats are NOT included in register pressure check because
	   they do not increase regpressure */

	/* mem_in/reg_in for live_in values, especially phis and their arguments */
	pset_foreach(live, irn) {
		int          p = 0,
					 n;

		spill = set_find_spill(spill_bb->ilp, irn);
		assert(spill && spill->irn == irn);

		if(is_Phi(irn) && get_nodes_block(irn) == bb) {
			for (n=get_Phi_n_preds(irn)-1; n>=0; --n) {
				ilp_cst_t       mem_in,
								reg_in;
				ir_node        *phi_arg = get_Phi_pred(irn, n);
				ir_node        *bb_p = get_Block_cfgpred_block(bb, n);
				spill_bb_t     *spill_bb_p = get_irn_link(bb_p);
				spill_t        *spill_p;
				op_t           *op = get_irn_link(irn);

				/* although the phi is in the right regclass one or more of
				 * its arguments can be in a different one or at least to
				 * ignore
				 */
				if(has_reg_class(si, phi_arg)) {
					/* mem_in < mem_out_arg + copy */
					ir_snprintf(buf, sizeof(buf), "mem_in_%N_%N-%d", irn, bb, p);
					mem_in = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

					/* reg_in < reg_out_arg */
					ir_snprintf(buf, sizeof(buf), "reg_in_%N_%N-%d", irn, bb, p++);
					reg_in = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

					lpp_set_factor_fast(si->lpp, mem_in, spill->mem_in, 1.0);
					lpp_set_factor_fast(si->lpp, reg_in, spill->reg_in, 1.0);

					spill_p = set_find_spill(spill_bb_p->ilp, phi_arg);
					assert(spill_p);

					lpp_set_factor_fast(si->lpp, mem_in, spill_p->mem_out, -1.0);
					if(opt_memcopies)
						lpp_set_factor_fast(si->lpp, mem_in, op->attr.live_range.args.copies[n], -1.0);

					lpp_set_factor_fast(si->lpp, reg_in, spill_p->reg_out, -1.0);
				}
			}
		} else {
			/* else assure the value arrives on all paths in the same resource */

			for (n=get_Block_n_cfgpreds(bb)-1; n>=0; --n) {
				ilp_cst_t       mem_in,
								reg_in;
				ir_node        *bb_p = get_Block_cfgpred_block(bb, n);
				spill_bb_t     *spill_bb_p = get_irn_link(bb_p);
				spill_t        *spill_p;

				ir_snprintf(buf, sizeof(buf), "mem_in_%N_%N-%d", irn, bb, p);
				mem_in = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
				ir_snprintf(buf, sizeof(buf), "reg_in_%N_%N-%d", irn, bb, p++);
				reg_in = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

				lpp_set_factor_fast(si->lpp, mem_in, spill->mem_in, 1.0);
				lpp_set_factor_fast(si->lpp, reg_in, spill->reg_in, 1.0);

				spill_p = set_find_spill(spill_bb_p->ilp, irn);
				assert(spill_p);

				lpp_set_factor_fast(si->lpp, mem_in, spill_p->mem_out, -1.0);
				lpp_set_factor_fast(si->lpp, reg_in, spill_p->reg_out, -1.0);
			}
		}
	}

	foreach_post_remat(bb, tmp) {
		int         n;

		for (n=get_irn_arity(tmp)-1; n>=0; --n) {
			ir_node    *remat_arg = get_irn_n(tmp, n);
			op_t       *remat_op = get_irn_link(tmp);

			if(!has_reg_class(si, remat_arg)) continue;

			spill = set_find_spill(spill_bb->ilp, remat_arg);
			assert(spill);

			ir_snprintf(buf, sizeof(buf), "req_remat2_%N_%N_arg_%N", tmp, bb, remat_arg);
			cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
			lpp_set_factor_fast(si->lpp, cst, spill->reg_in, -1.0);
			lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
		}
	}

	pset_foreach(live, irn) {
		const op_t      *op = get_irn_link(irn);
		const ir_node   *remat;
		int              n_remats = 0;

		cst = ILP_UNDEF;

		foreach_post_remat(bb, remat) {
			int   n;

			for (n=get_irn_arity(remat)-1; n>=0; --n) {
				const ir_node  *arg = get_irn_n(remat, n);

				if(arg == irn) {
					const op_t   *remat_op = get_irn_link(remat);

					if(cst == ILP_UNDEF) {
						/* sum remat2s <= 1 + n_remats*live_range */
						ir_snprintf(buf, sizeof(buf), "dying_lr_%N_%N", irn, bb);
						cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 1.0);
					}
					lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
					++n_remats;
					break;
				}
			}
		}
		if(cst != ILP_UNDEF && op->attr.live_range.ilp != ILP_UNDEF) {
			lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.ilp, -n_remats);
		}
	}

	/* first live ranges from reg_ins */
	pset_foreach(live, irn) {
		op_t      *op = get_irn_link(irn);

		if(op->attr.live_range.ilp != ILP_UNDEF) {

			spill = set_find_spill(spill_bb->ilp, irn);
			assert(spill && spill->irn == irn);

			ir_snprintf(buf, sizeof(buf), "first_lr_%N_%N", irn, bb);
			cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);
			lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.ilp, 1.0);
			lpp_set_factor_fast(si->lpp, cst, spill->reg_in, -1.0);

			foreach_post_remat(bb, tmp) {
				op_t     *remat_op = get_irn_link(tmp);

				if(remat_op->attr.remat.remat->value == irn) {
					lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
				}
			}
		}
	}

	/* walk forward now and compute constraints for placing spills */
	/* this must only be done for values that are not defined in this block */
	pset_foreach(live, irn) {
		/*
		 * if value is defined in this block we can anways place the spill directly after the def
		 *    -> no constraint necessary
		 */
		if(!is_Phi(irn) && get_nodes_block(irn) == bb) {
			assert(0);
		}


		spill = set_find_spill(spill_bb->ilp, irn);
		assert(spill);

		ir_snprintf(buf, sizeof(buf), "req_spill_%N_%N", irn, bb);
		cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0.0);

		lpp_set_factor_fast(si->lpp, cst, spill->spill, 1.0);
		if(is_diverge_edge(bb)) lpp_set_factor_fast(si->lpp, cst, spill->reg_in, -1.0);

		if(!is_Phi(irn)) {
			sched_foreach_op(bb, tmp) {
				op_t   *op = get_irn_link(tmp);

				if(is_Phi(tmp)) continue;
				assert(!is_Proj(tmp));

				if(op->is_remat) {
					const ir_node   *value = op->attr.remat.remat->value;

					if(value == irn) {
						/* only collect remats up to the first real use of a value */
						lpp_set_factor_fast(si->lpp, cst, op->attr.remat.ilp, -1.0);
					}
				} else {
					int   n;

					for (n=get_irn_arity(tmp)-1; n>=0; --n) {
						ir_node    *arg = get_irn_n(tmp, n);

						if(arg == irn) {
							/* if a value is used stop collecting remats */
                            goto next_live;
						}
					}
				}
			}
		}
next_live: ;
	}

	del_pset(live);
}

typedef struct _irnlist_t {
	struct list_head   list;
	ir_node           *irn;
} irnlist_t;

typedef struct _interference_t {
	struct list_head    blocklist;
	ir_node            *a;
	ir_node            *b;
} interference_t;

static int
cmp_interference(const void *a, const void *b, size_t size)
{
	const interference_t *p = a;
	const interference_t *q = b;

	return !(p->a == q->a && p->b == q->b);
}

static interference_t *
set_find_interference(set * set, ir_node * a, ir_node * b)
{
	interference_t     query;

	query.a = (a>b)?a:b;
	query.b = (a>b)?b:a;

	return set_find(set, &query, sizeof(query), HASH_PTR(PTR_TO_INT(a) ^ PTR_TO_INT(b)));
}

static interference_t *
set_insert_interference(spill_ilp_t * si, set * set, ir_node * a, ir_node * b, ir_node * bb)
{
	interference_t     query,
					  *result;
	irnlist_t         *list = obstack_alloc(si->obst, sizeof(*list));

	list->irn = bb;

	result = set_find_interference(set, a, b);
	if(result) {

		list_add(&list->list, &result->blocklist);
		return result;
	}

	query.a = (a>b)?a:b;
	query.b = (a>b)?b:a;

	result = set_insert(set, &query, sizeof(query), HASH_PTR(PTR_TO_INT(a) ^ PTR_TO_INT(b)));

	INIT_LIST_HEAD(&result->blocklist);
	list_add(&list->list, &result->blocklist);

	return result;
}

static int
values_interfere_in_block(const spill_ilp_t * si, const ir_node * bb, const ir_node * a, const ir_node * b)
{
	const ir_edge_t *edge;

	if(get_nodes_block(a) != bb && get_nodes_block(b) != bb) {
		/* both values are live in, so they interfere */
		return 1;
	}

	/* ensure a dominates b */
	if(value_dominates(b,a)) {
		const ir_node * t;
		t = b;
		b = a;
		a = t;
	}
	assert(get_nodes_block(b) == bb && "at least b should be defined here in this block");


	/* the following code is stolen from bera.c */
	if(be_is_live_end(si->lv, bb, a))
		return 1;

	foreach_out_edge(a, edge) {
		const ir_node *user = edge->src;
		if(get_nodes_block(user) == bb
				&& !is_Phi(user)
				&& b != user
				&& !pset_find_ptr(si->inverse_ops, user)
				&& value_dominates(b, user))
			return 1;
	}

	return 0;
}

/**
 * Walk all irg blocks and collect interfering values inside of phi classes
 */
static void
luke_interferencewalker(ir_node * bb, void * data)
{
	spill_ilp_t    *si = (spill_ilp_t*)data;
	int             l1, l2;

	be_lv_foreach(si->lv, bb, be_lv_state_end | be_lv_state_out | be_lv_state_in, l1) {
		ir_node        *a = be_lv_get_irn(si->lv, bb, l1);
		op_t           *a_op = get_irn_link(a);


		/* a is only interesting if it is in my register class and if it is inside a phi class */
		if (has_reg_class(si, a) && get_phi_class(si->pc, a)) {
			if (a_op->is_remat || pset_find_ptr(si->inverse_ops, a))
				continue;

			for (l2 = _be_lv_next_irn(si->lv, bb, 0xff, l1 + 1); l2 >= 0; l2 = _be_lv_next_irn(si->lv, bb, 0xff, l2 + 1)) {
				ir_node *b    = be_lv_get_irn(si->lv, bb, l2);
				op_t    *b_op = get_irn_link(b);

				/* a and b are only interesting if they are in the same phi class */
				if (has_reg_class(si, b) && get_phi_class(si->pc, a) == get_phi_class(si->pc, b)) {
					if (b_op->is_remat || pset_find_ptr(si->inverse_ops, b))
						continue;

					if (values_interfere_in_block(si, bb, a, b)) {
						DBG((si->dbg, LEVEL_4, "\tvalues interfere in %+F: %+F, %+F\n", bb, a, b));
						set_insert_interference(si, si->interferences, a, b, bb);
					}
				}
			}
		}
	}
}

static unsigned int copy_path_id = 0;

static void
write_copy_path_cst(spill_ilp_t *si, pset * copies, ilp_var_t any_interfere)
{
	ilp_cst_t  cst;
	ilp_var_t  copy;
	char       buf[256];
	void      *ptr;

	ir_snprintf(buf, sizeof(buf), "copy_path-%d", copy_path_id++);
	cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0);

	lpp_set_factor_fast(si->lpp, cst, any_interfere, 1.0);

	pset_foreach(copies, ptr) {
		copy = PTR_TO_INT(ptr);
		lpp_set_factor_fast(si->lpp, cst, copy, -1.0);
	}
}

/**
 * @parameter copies   contains a path of copies which lead us to irn
 * @parameter visited  contains a set of nodes already visited on this path
 */
static int
find_copy_path(spill_ilp_t * si, const ir_node * irn, const ir_node * target, ilp_var_t any_interfere, pset * copies, pset * visited)
{
	const ir_edge_t *edge;
	op_t            *op = get_irn_link(irn);
    pset            *visited_users = pset_new_ptr_default();
	int              paths = 0;

	if(op->is_remat) return 0;

	pset_insert_ptr(visited, irn);

	if(is_Phi(irn)) {
		int    n;
        pset  *visited_operands = pset_new_ptr(get_irn_arity(irn));

		/* visit all operands */
		for(n=get_irn_arity(irn)-1; n>=0; --n) {
			ir_node  *arg = get_irn_n(irn, n);
			ilp_var_t  copy = op->attr.live_range.args.copies[n];

			if(!has_reg_class(si, arg)) continue;
            if(pset_find_ptr(visited_operands, arg)) continue;
            pset_insert_ptr(visited_operands, arg);

			if(arg == target) {
				if(++paths > MAX_PATHS && pset_count(copies) != 0) {
					del_pset(visited_operands);
					del_pset(visited_users);
					pset_remove_ptr(visited, irn);
					return paths;
				}
				pset_insert(copies, INT_TO_PTR(copy), copy);
				write_copy_path_cst(si, copies, any_interfere);
				pset_remove(copies, INT_TO_PTR(copy), copy);
			} else if(!pset_find_ptr(visited, arg)) {
				pset_insert(copies, INT_TO_PTR(copy), copy);
				paths += find_copy_path(si, arg, target, any_interfere, copies, visited);
				pset_remove(copies, INT_TO_PTR(copy), copy);

                if(paths > MAX_PATHS) {
                    if(pset_count(copies) == 0) {
                        ilp_cst_t  cst;
                        char       buf[256];

                        ir_snprintf(buf, sizeof(buf), "always_copy-%d-%d", any_interfere, copy);
                        cst = lpp_add_cst_uniq(si->lpp, buf, lpp_equal, 0);
                        lpp_set_factor_fast(si->lpp, cst, any_interfere, -1.0);
                        lpp_set_factor_fast(si->lpp, cst, copy, 1.0);
                        DBG((si->dbg, LEVEL_1, "ALWAYS COPYING %d FOR INTERFERENCE %d\n", copy, any_interfere));

                        paths = 0;
                    } else {
                        del_pset(visited_operands);
                        del_pset(visited_users);
                        pset_remove_ptr(visited, irn);
                        return paths;
                    }
                } else if(pset_count(copies) == 0) {
					paths = 0;
				}
			}
		}

        del_pset(visited_operands);
	}

	/* visit all uses which are phis */
	foreach_out_edge(irn, edge) {
		ir_node  *user = edge->src;
		int       pos  = edge->pos;
		op_t     *op = get_irn_link(user);
		ilp_var_t copy;

		if(!is_Phi(user)) continue;
		if(!has_reg_class(si, user)) continue;
        if(pset_find_ptr(visited_users, user)) continue;
        pset_insert_ptr(visited_users, user);

		copy = op->attr.live_range.args.copies[pos];

		if(user == target) {
			if(++paths > MAX_PATHS && pset_count(copies) != 0) {
				del_pset(visited_users);
				pset_remove_ptr(visited, irn);
				return paths;
			}
			pset_insert(copies, INT_TO_PTR(copy), copy);
			write_copy_path_cst(si, copies, any_interfere);
			pset_remove(copies, INT_TO_PTR(copy), copy);
		} else if(!pset_find_ptr(visited, user)) {
			pset_insert(copies, INT_TO_PTR(copy), copy);
			paths += find_copy_path(si, user, target, any_interfere, copies, visited);
			pset_remove(copies, INT_TO_PTR(copy), copy);

            if(paths > MAX_PATHS) {
                if(pset_count(copies) == 0) {
                    ilp_cst_t  cst;
                    char       buf[256];

                    ir_snprintf(buf, sizeof(buf), "always_copy-%d-%d", any_interfere, copy);
                    cst = lpp_add_cst_uniq(si->lpp, buf, lpp_equal, 0);
                    lpp_set_factor_fast(si->lpp, cst, any_interfere, -1.0);
                    lpp_set_factor_fast(si->lpp, cst, copy, 1.0);
                    DBG((si->dbg, LEVEL_1, "ALWAYS COPYING %d FOR INTERFERENCE %d\n", copy, any_interfere));

                    paths = 0;
                } else {
                    del_pset(visited_users);
                    pset_remove_ptr(visited, irn);
                    return paths;
                }
            } else if(pset_count(copies) == 0) {
				paths = 0;
			}
		}
	}

    del_pset(visited_users);
	pset_remove_ptr(visited, irn);
	return paths;
}

static void
gen_copy_constraints(spill_ilp_t * si, const ir_node * a, const ir_node * b, ilp_var_t any_interfere)
{
	pset * copies = pset_new_ptr_default();
	pset * visited = pset_new_ptr_default();

	find_copy_path(si, a, b, any_interfere, copies, visited);

	del_pset(visited);
	del_pset(copies);
}


static void
memcopyhandler(spill_ilp_t * si)
{
	interference_t   *interference;
	char              buf[256];
	/* teste Speicherwerte auf Interferenz */

	DBG((si->dbg, LEVEL_2, "\t calling interferencewalker\n"));
	irg_block_walk_graph(si->birg->irg, luke_interferencewalker, NULL, si);

	/* now lets emit the ILP unequations for the crap */
	set_foreach(si->interferences, interference) {
		irnlist_t      *irnlist;
		ilp_var_t      interfere, any_interfere;
		ilp_cst_t      any_interfere_cst, cst;
		const ir_node  *a  = interference->a;
		const ir_node  *b  = interference->b;

		/* any_interf <= \sum interf */
		ir_snprintf(buf, sizeof(buf), "interfere_%N_%N", a, b);
		any_interfere_cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0);
		any_interfere     = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 1.0);

		lpp_set_factor_fast(si->lpp, any_interfere_cst, any_interfere, 1.0);

		list_for_each_entry(irnlist_t, irnlist, &interference->blocklist, list) {
			const ir_node  *bb = irnlist->irn;
			spill_bb_t     *spill_bb = get_irn_link(bb);
			spill_t        *spilla,
						   *spillb;
			char           buf[256];

			spilla = set_find_spill(spill_bb->ilp, a);
			assert(spilla);

			spillb = set_find_spill(spill_bb->ilp, b);
			assert(spillb);

			/* interfere <-> (mem_in_a or spill_a) and (mem_in_b or spill_b): */
			/* 1:   mem_in_a + mem_in_b + spill_a + spill_b - interfere <= 1 */
			/* 2: - mem_in_a - spill_a + interfere <= 0 */
			/* 3: - mem_in_b - spill_b + interfere <= 0 */
			ir_snprintf(buf, sizeof(buf), "interfere_%N_%N_%N", bb, a, b);
			interfere = lpp_add_var_default(si->lpp, buf, lpp_binary, 0.0, 1.0);

			ir_snprintf(buf, sizeof(buf), "interfere_%N_%N_%N-1", bb, a, b);
			cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 1);

			lpp_set_factor_fast(si->lpp, cst, interfere, -1.0);
			if(spilla->mem_in != ILP_UNDEF) lpp_set_factor_fast(si->lpp, cst, spilla->mem_in, 1.0);
			lpp_set_factor_fast(si->lpp, cst, spilla->spill, 1.0);
			if(spillb->mem_in != ILP_UNDEF) lpp_set_factor_fast(si->lpp, cst, spillb->mem_in, 1.0);
			lpp_set_factor_fast(si->lpp, cst, spillb->spill, 1.0);

			ir_snprintf(buf, sizeof(buf), "interfere_%N_%N_%N-2", bb, a, b);
			cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0);

			lpp_set_factor_fast(si->lpp, cst, interfere, 1.0);
			if(spilla->mem_in != ILP_UNDEF) lpp_set_factor_fast(si->lpp, cst, spilla->mem_in, -1.0);
			lpp_set_factor_fast(si->lpp, cst, spilla->spill, -1.0);

			ir_snprintf(buf, sizeof(buf), "interfere_%N_%N_%N-3", bb, a, b);
			cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0);

			lpp_set_factor_fast(si->lpp, cst, interfere, 1.0);
			if(spillb->mem_in != ILP_UNDEF) lpp_set_factor_fast(si->lpp, cst, spillb->mem_in, -1.0);
			lpp_set_factor_fast(si->lpp, cst, spillb->spill, -1.0);


			lpp_set_factor_fast(si->lpp, any_interfere_cst, interfere, -1.0);

			/* any_interfere >= interf */
			ir_snprintf(buf, sizeof(buf), "interfere_%N_%N-%N", a, b, bb);
			cst = lpp_add_cst_uniq(si->lpp, buf, lpp_less, 0);

			lpp_set_factor_fast(si->lpp, cst, interfere, 1.0);
			lpp_set_factor_fast(si->lpp, cst, any_interfere, -1.0);
		}

		/* now that we know whether the two values interfere in memory we can drop constraints to enforce copies */
		gen_copy_constraints(si,a,b,any_interfere);
	}
}


static INLINE int
is_zero(double x)
{
	return fabs(x) < 0.00001;
}

static int mark_remat_nodes_hook(FILE *F, ir_node *n, ir_node *l)
{
	spill_ilp_t *si = get_irg_link(current_ir_graph);

	if(pset_find_ptr(si->all_possible_remats, n)) {
		op_t   *op = (op_t*)get_irn_link(n);
		assert(op && op->is_remat);

		if(!op->attr.remat.remat->inverse) {
			if(op->attr.remat.pre) {
				ir_fprintf(F, "color:red info3:\"remat value: %+F\"", op->attr.remat.remat->value);
			} else {
				ir_fprintf(F, "color:orange info3:\"remat2 value: %+F\"", op->attr.remat.remat->value);
			}

			return 1;
		} else {
			op_t   *op = (op_t*)get_irn_link(n);
			assert(op && op->is_remat);

			if(op->attr.remat.pre) {
				ir_fprintf(F, "color:cyan info3:\"remat inverse value: %+F\"", op->attr.remat.remat->value);
			} else {
				ir_fprintf(F, "color:lightcyan info3:\"remat2 inverse value: %+F\"", op->attr.remat.remat->value);
			}

			return 1;
		}
	}

	return 0;
}

static void
dump_graph_with_remats(ir_graph * irg, const char * suffix)
{
	set_dump_node_vcgattr_hook(mark_remat_nodes_hook);
	be_dump(irg, suffix, dump_ir_block_graph_sched);
	set_dump_node_vcgattr_hook(NULL);
}

/**
 * Edge hook to dump the schedule edges with annotated register pressure.
 */
static int
sched_pressure_edge_hook(FILE *F, ir_node *irn)
{
	if(sched_is_scheduled(irn) && sched_has_prev(irn)) {
		ir_node *prev = sched_prev(irn);
		fprintf(F, "edge:{sourcename:\"");
		PRINT_NODEID(irn);
		fprintf(F, "\" targetname:\"");
		PRINT_NODEID(prev);
		fprintf(F, "\" label:\"%d", (int)get_irn_link(irn));
		fprintf(F, "\" color:magenta}\n");
	}
	return 1;
}

static void
dump_ir_block_graph_sched_pressure(ir_graph *irg, const char *suffix)
{
	DUMP_NODE_EDGE_FUNC old_edge_hook = get_dump_node_edge_hook();

	dump_consts_local(0);
	set_dump_node_edge_hook(sched_pressure_edge_hook);
	dump_ir_block_graph(irg, suffix);
	set_dump_node_edge_hook(old_edge_hook);
}

static void
walker_pressure_annotator(ir_node * bb, void * data)
{
	spill_ilp_t  *si = data;
	ir_node      *irn;
	int           n, i;
	pset         *live = pset_new_ptr_default();
	int           projs = 0;

	be_lv_foreach(si->lv, bb, be_lv_state_end, i) {
		irn = be_lv_get_irn(si->lv, bb, i);

		if (has_reg_class(si, irn)) {
			pset_insert_ptr(live, irn);
		}
	}

	set_irn_link(bb, INT_TO_PTR(pset_count(live)));

	sched_foreach_reverse(bb, irn) {
		if(is_Phi(irn)) {
			set_irn_link(irn, INT_TO_PTR(pset_count(live)));
			continue;
		}

		if(has_reg_class(si, irn)) {
			pset_remove_ptr(live, irn);
			if(is_Proj(irn)) ++projs;
		}

		if(!is_Proj(irn)) projs = 0;

		for (n=get_irn_arity(irn)-1; n>=0; --n) {
			ir_node    *arg = get_irn_n(irn, n);

			if(has_reg_class(si, arg)) pset_insert_ptr(live, arg);
		}
		set_irn_link(irn, INT_TO_PTR(pset_count(live)+projs));
	}

	del_pset(live);
}

static void
dump_pressure_graph(spill_ilp_t * si, const char *suffix)
{
	be_dump(si->birg->irg, suffix, dump_ir_block_graph_sched_pressure);
}

static void
connect_all_remats_with_keep(spill_ilp_t * si)
{
	ir_node   *irn;
	ir_node  **ins,
			 **pos;
	int        n_remats;


	n_remats = pset_count(si->all_possible_remats);
	if(n_remats) {
		ins = obstack_alloc(si->obst, n_remats * sizeof(*ins));

		pos = ins;
		pset_foreach(si->all_possible_remats, irn) {
			*pos = irn;
			++pos;
		}

		si->keep = be_new_Keep(si->cls, si->birg->irg, get_irg_end_block(si->birg->irg), n_remats, ins);

		obstack_free(si->obst, ins);
	}
}

static void
connect_all_spills_with_keep(spill_ilp_t * si)
{
	ir_node   *irn;
	ir_node  **ins,
			 **pos;
	int        n_spills;
	ir_node   *keep;


	n_spills = pset_count(si->spills);
	if(n_spills) {
		ins = obstack_alloc(si->obst, n_spills * sizeof(*ins));

		pos = ins;
		pset_foreach(si->spills, irn) {
			*pos = irn;
			++pos;
		}

		keep = be_new_Keep(si->cls, si->birg->irg, get_irg_end_block(si->birg->irg), n_spills, ins);

		obstack_free(si->obst, ins);
	}
}

/** insert a spill at an arbitrary position */
ir_node *be_spill2(const arch_env_t *arch_env, ir_node *irn, ir_node *insert)
{
	ir_node  *bl    = is_Block(insert) ? insert : get_nodes_block(insert);
	ir_graph *irg   = get_irn_irg(bl);
	ir_node  *frame = get_irg_frame(irg);
	ir_node  *spill;
	ir_node  *next;
	const arch_register_class_t *cls       = arch_get_irn_reg_class(arch_env, irn, -1);
	const arch_register_class_t *cls_frame = arch_get_irn_reg_class(arch_env, frame, -1);

	spill = be_new_Spill(cls, cls_frame, irg, bl, frame, irn);

	/*
	 * search the right insertion point. a spill of a phi cannot be put
	 * directly after the phi, if there are some phis behind the one which
	 * is spilled. Also, a spill of a Proj must be after all Projs of the
	 * same tuple node.
	 *
	 * Here's one special case:
	 * If the spill is in the start block, the spill must be after the frame
	 * pointer is set up. This is done by setting insert to the end of the block
	 * which is its default initialization (see above).
	 */

	if (bl == get_irg_start_block(irg) && sched_get_time_step(frame) >= sched_get_time_step(insert))
		insert = frame;

	for (next = sched_next(insert); is_Phi(next) || is_Proj(next); next = sched_next(insert))
		insert = next;

	sched_add_after(insert, spill);
	return spill;
}

static void
delete_remat(spill_ilp_t * si, ir_node * remat) {
	int       n;
	ir_node  *bad = get_irg_bad(si->birg->irg);

	sched_remove(remat);

	/* kill links to operands */
	for (n=get_irn_arity(remat)-1; n>=-1; --n) {
		set_irn_n(remat, n, bad);
	}
}

static void
clean_remat_info(spill_ilp_t * si)
{
	int            n;
	remat_t       *remat;
	remat_info_t  *remat_info;
	ir_node       *bad = get_irg_bad(si->birg->irg);

	set_foreach(si->remat_info, remat_info) {
		if(!remat_info->remats) continue;

		pset_foreach(remat_info->remats, remat)
		{
			if(remat->proj && get_irn_n_edges(remat->proj) == 0) {
				if(sched_is_scheduled(remat->proj)) {
					sched_remove((ir_node*)remat->proj);
				}
				set_irn_n((ir_node*)remat->proj, -1, bad);
				set_irn_n((ir_node*)remat->proj, 0, bad);
			}

			if(get_irn_n_edges(remat->op) == 0) {
				if(sched_is_scheduled(remat->op)) {
					sched_remove((ir_node*)remat->op);
				}
				for (n=get_irn_arity(remat->op)-1; n>=-1; --n) {
					set_irn_n((ir_node*)remat->op, n, bad);
				}
			}
		}

		if(remat_info->remats) del_pset(remat_info->remats);
		if(remat_info->remats_by_operand) del_pset(remat_info->remats_by_operand);
	}
}

static void
delete_unnecessary_remats(spill_ilp_t * si)
{
	if(opt_keep_alive & KEEPALIVE_REMATS) {
		int       n;
		ir_node  *bad = get_irg_bad(si->birg->irg);

		if(si->keep) {
			for (n=get_irn_arity(si->keep)-1; n>=0; --n) {
				ir_node        *keep_arg = get_irn_n(si->keep, n);
				op_t           *arg_op = get_irn_link(keep_arg);
				lpp_name_t     *name;

				assert(arg_op->is_remat);

				name = si->lpp->vars[arg_op->attr.remat.ilp];

				if(is_zero(name->value)) {
					DBG((si->dbg, LEVEL_3, "\t  deleting remat %+F\n", keep_arg));
					/* TODO check whether reload is preferred over remat (could be bug) */
					delete_remat(si, keep_arg);
				} else {
					if(!arg_op->attr.remat.remat->inverse) {
						if(arg_op->attr.remat.pre) {
							DBG((si->dbg, LEVEL_2, "\t**remat kept: %+F\n", keep_arg));
						} else {
							DBG((si->dbg, LEVEL_2, "\t%%%%remat2 kept: %+F\n", keep_arg));
						}
					} else {
						if(arg_op->attr.remat.pre) {
							DBG((si->dbg, LEVEL_2, "\t**INVERSE remat kept: %+F\n", keep_arg));
						} else {
							DBG((si->dbg, LEVEL_2, "\t%%%%INVERSE remat2 kept: %+F\n", keep_arg));
						}
					}
				}

				set_irn_n(si->keep, n, bad);
			}
		} else {
			DBG((si->dbg, LEVEL_2, "\t  no remats to delete (none have been inserted)\n"));
		}
	} else {
		ir_node  *remat;

		pset_foreach(si->all_possible_remats, remat) {
			op_t           *remat_op = get_irn_link(remat);
			lpp_name_t     *name = si->lpp->vars[remat_op->attr.remat.ilp];

			if(is_zero(name->value)) {
				DBG((si->dbg, LEVEL_3, "\t  deleting remat %+F\n", remat));
				/* TODO check whether reload is preferred over remat (could be bug) */
				delete_remat(si, remat);
			} else {
				if(!remat_op->attr.remat.remat->inverse) {
					if(remat_op->attr.remat.pre) {
						DBG((si->dbg, LEVEL_2, "\t**remat kept: %+F\n", remat));
					} else {
						DBG((si->dbg, LEVEL_2, "\t%%%%remat2 kept: %+F\n", remat));
					}
				} else {
					if(remat_op->attr.remat.pre) {
						DBG((si->dbg, LEVEL_2, "\t**INVERSE remat kept: %+F\n", remat));
					} else {
						DBG((si->dbg, LEVEL_2, "\t%%%%INVERSE remat2 kept: %+F\n", remat));
					}
				}
			}
		}
	}
}

static pset *
get_spills_for_value(spill_ilp_t * si, const ir_node * value)
{
	pset     *spills = pset_new_ptr_default();

	const ir_node  *next;
	defs_t         *defs;

	defs = set_find_def(si->values, value);

	if(defs && defs->spills) {
		for(next = defs->spills; next; next = get_irn_link(next)) {
			pset_insert_ptr(spills, next);
		}
	}

	return spills;
}

static ir_node *
new_r_PhiM_nokeep(ir_graph * irg, ir_node *block, int arity, ir_node **in)
{
	ir_node  *res;

	assert( get_irn_arity(block) == arity );

	res = new_ir_node(NULL, irg, block, op_Phi, mode_M, arity, in);
	res->attr.phi_backedge = new_backedge_arr(irg->obst, arity);

	return res;
}

/**
 * @param before   The node after which the spill will be placed in the schedule
 */
static ir_node *
insert_spill(spill_ilp_t * si, ir_node * irn, const ir_node * value, ir_node * before)
{
	defs_t   *defs;
	ir_node  *spill;
	const arch_env_t *arch_env = si->birg->main_env->arch_env;

	DBG((si->dbg, LEVEL_3, "\t  inserting spill for value %+F after %+F\n", irn, before));

	spill = be_spill2(arch_env, irn, before);

	defs = set_insert_def(si->values, value);
	assert(defs);

	/* enter into the linked list */
	set_irn_link(spill, defs->spills);
	defs->spills = spill;

	if(opt_keep_alive & KEEPALIVE_SPILLS)
		pset_insert_ptr(si->spills, spill);

	return spill;
}

/**
 * @param before   The Phi node which has to be spilled
 */
static ir_node *
insert_mem_phi(spill_ilp_t * si, ir_node * phi)
{
	ir_node   *mem_phi;
	ir_node  **ins;
	defs_t    *defs;
	int        n;

	NEW_ARR_A(ir_node*, ins, get_irn_arity(phi));

	for(n=get_irn_arity(phi)-1; n>=0; --n) {
		ins[n] = si->m_unknown;
	}

	mem_phi =  new_r_PhiM_nokeep(si->birg->irg, get_nodes_block(phi), get_irn_arity(phi), ins);

	defs = set_insert_def(si->values, phi);
	assert(defs);

	/* enter into the linked list */
	set_irn_link(mem_phi, defs->spills);
	defs->spills = mem_phi;

#ifdef SCHEDULE_PHIM
	sched_add_after(phi, mem_phi);
#else
	pset_insert_ptr(si->phims, mem_phi);
#endif

	if(opt_keep_alive & KEEPALIVE_SPILLS)
		pset_insert_ptr(si->spills, mem_phi);


	return mem_phi;
}

/**
 * Add remat to list of defs, destroys link field!
 */
static void
insert_remat(spill_ilp_t * si, ir_node * remat)
{
	defs_t   *defs;
	op_t     *remat_op = get_irn_link(remat);

	assert(remat_op->is_remat);

	defs = set_insert_def(si->values, remat_op->attr.remat.remat->value);
	assert(defs);

	/* enter into the linked list */
	set_irn_link(remat, defs->remats);
	defs->remats = remat;
}


/**
 * Add reload before operation and add to list of defs
 */
static ir_node *
insert_reload(spill_ilp_t * si, const ir_node * value, ir_node * after)
{
	defs_t   *defs;
	ir_node  *reload,
			 *spill;
	const arch_env_t *arch_env = si->birg->main_env->arch_env;

	DBG((si->dbg, LEVEL_3, "\t  inserting reload for value %+F before %+F\n", value, after));

	defs = set_find_def(si->values, value);

	spill = defs->spills;
	assert(spill && "no spill placed before reload");

	reload = be_reload(arch_env, si->cls, after, get_irn_mode(value), spill);

	/* enter into the linked list */
	set_irn_link(reload, defs->remats);
	defs->remats = reload;

	return reload;
}

void perform_memory_operand(spill_ilp_t * si, memoperand_t * memoperand)
{
	defs_t           *defs;
	ir_node          *value = get_irn_n(memoperand->irn, memoperand->pos);
	ir_node          *spill;
	const arch_env_t *arch_env = si->birg->main_env->arch_env;

	DBG((si->dbg, LEVEL_2, "\t  inserting memory operand for value %+F at %+F\n", value, memoperand->irn));

	defs = set_find_def(si->values, value);

	spill = defs->spills;
	assert(spill && "no spill placed before reload");

	arch_perform_memory_operand(arch_env, memoperand->irn, spill, memoperand->pos);
}

void insert_memoperands(spill_ilp_t * si)
{
	memoperand_t   *memoperand;
	lpp_name_t     *name;

	set_foreach(si->memoperands, memoperand) {
		name = si->lpp->vars[memoperand->ilp];
		if(!is_zero(name->value)) {
			perform_memory_operand(si, memoperand);
		}
	}
}

static void
walker_spill_placer(ir_node * bb, void * data) {
	spill_ilp_t   *si = (spill_ilp_t*)data;
	ir_node       *irn;
	spill_bb_t    *spill_bb = get_irn_link(bb);
	pset          *spills_to_do = pset_new_ptr_default();
	spill_t       *spill;

	set_foreach(spill_bb->ilp, spill) {
		lpp_name_t    *name;

		if(is_Phi(spill->irn) && get_nodes_block(spill->irn) == bb) {
			name = si->lpp->vars[spill->mem_in];
			if(!is_zero(name->value)) {
				ir_node   *mem_phi;

				mem_phi = insert_mem_phi(si, spill->irn);

				DBG((si->dbg, LEVEL_2, "\t >>spilled Phi %+F -> %+F\n", spill->irn, mem_phi));
			}
		}

		name = si->lpp->vars[spill->spill];
		if(!is_zero(name->value)) {
			/* place spill directly after definition */
			if(get_nodes_block(spill->irn) == bb) {
				insert_spill(si, spill->irn, spill->irn, spill->irn);
				continue;
			}

			/* place spill at bb start */
			if(spill->reg_in > 0) {
				name = si->lpp->vars[spill->reg_in];
				if(!is_zero(name->value)) {
					insert_spill(si, spill->irn, spill->irn, bb);
					continue;
				}
			}
			/* place spill after a remat */
			pset_insert_ptr(spills_to_do, spill->irn);
		}
	}
	DBG((si->dbg, LEVEL_3, "\t  %d spills to do in block %+F\n", pset_count(spills_to_do), bb));


	for(irn = sched_block_first_nonphi(bb); !sched_is_end(irn); irn = sched_next(irn)) {
		op_t     *op = get_irn_link(irn);

		if(be_is_Spill(irn)) continue;

		if(op->is_remat) {
			/* TODO fix this if we want to support remats with more than two nodes */
			if(get_irn_mode(irn) != mode_T && pset_find_ptr(spills_to_do, op->attr.remat.remat->value)) {
				pset_remove_ptr(spills_to_do, op->attr.remat.remat->value);

				insert_spill(si, irn, op->attr.remat.remat->value, irn);
			}
		} else {
			if(pset_find_ptr(spills_to_do, irn)) {
				pset_remove_ptr(spills_to_do, irn);

				insert_spill(si, irn, irn, irn);
			}
		}

	}

	assert(pset_count(spills_to_do) == 0);

	/* afterwards free data in block */
	del_pset(spills_to_do);
}

static ir_node *
insert_mem_copy(spill_ilp_t * si, ir_node * bb, ir_node * value)
{
	ir_node          *insert_pos = bb;
	ir_node          *spill;
	const arch_env_t *arch_env = si->birg->main_env->arch_env;

	/* find last definition of arg value in block */
	ir_node  *next;
	defs_t   *defs;
	int       last = 0;

	defs = set_find_def(si->values, value);

	if(defs && defs->remats) {
		for(next = defs->remats; next; next = get_irn_link(next)) {
			if(get_nodes_block(next) == bb && sched_get_time_step(next) > last) {
				last = sched_get_time_step(next);
				insert_pos = next;
			}
		}
	}

	if(get_nodes_block(value) == bb && sched_get_time_step(value) > last) {
		last = sched_get_time_step(value);
		insert_pos = value;
	}

	DBG((si->dbg, LEVEL_2, "\t  inserting mem copy for value %+F after %+F\n", value, insert_pos));

	spill = be_spill2(arch_env, is_Block(insert_pos)?value:insert_pos, insert_pos);

	return spill;
}

static void
phim_fixer(spill_ilp_t *si) {
	defs_t  *defs;

	set_foreach(si->values, defs) {
		const ir_node  *phi = defs->value;
		op_t           *op = get_irn_link(phi);
		ir_node        *phi_m = NULL;
		ir_node        *next = defs->spills;
		int             n;

		if(!is_Phi(phi)) continue;

		while(next) {
			if(is_Phi(next) && get_irn_mode(next) == mode_M) {
				phi_m = next;
				break;
			} else {
				next = get_irn_link(next);
			}
		}
		if(!phi_m) continue;

		for(n=get_irn_arity(phi)-1; n>=0; --n) {
			ir_node        *value = get_irn_n(phi, n);
			defs_t         *val_defs = set_find_def(si->values, value);

			/* a spill of this value */
			ir_node      *spill;


			if(opt_memcopies) {
				ir_node    *pred = get_Block_cfgpred_block(get_nodes_block(phi), n);
				lpp_name_t *name = si->lpp->vars[op->attr.live_range.args.copies[n]];

				if(!is_zero(name->value)) {
					spill = insert_mem_copy(si, pred, value);
				} else {
					spill = val_defs->spills;
				}
			} else {
				spill = val_defs->spills;
			}

			assert(spill && "no spill placed before PhiM");
			set_irn_n(phi_m, n, spill);
		}
	}
}

static void
walker_reload_placer(ir_node * bb, void * data) {
	spill_ilp_t   *si = (spill_ilp_t*)data;
	ir_node       *irn;
	spill_bb_t    *spill_bb = get_irn_link(bb);

	/* reloads at end of block */
	if(spill_bb->reloads) {
		keyval_t    *keyval;

		set_foreach(spill_bb->reloads, keyval) {
			ir_node        *irn = (ir_node*)keyval->key;
			ilp_var_t       reload = PTR_TO_INT(keyval->val);
			lpp_name_t     *name;

			name = si->lpp->vars[reload];
			if(!is_zero(name->value)) {
				ir_node    *reload;
				ir_node    *insert_pos = bb;
				ir_node    *prev = sched_block_last_noncf(si, bb);
				op_t       *prev_op = get_irn_link(prev);

				while(be_is_Spill(prev)) {
					prev = sched_prev(prev);
				}

				prev_op = get_irn_link(prev);

				/* insert reload before pre-remats */
				while(!sched_is_end(prev) && !be_is_Reload(prev) && !is_Phi(prev)
						&& prev_op->is_remat && prev_op->attr.remat.pre) {
					insert_pos = prev;

					do {
						prev = sched_prev(prev);
					} while(be_is_Spill(prev));

					prev_op = get_irn_link(prev);

				}

				reload = insert_reload(si, irn, insert_pos);

				if(opt_keep_alive & KEEPALIVE_RELOADS)
					pset_insert_ptr(si->spills, reload);
			}
		}
	}

	/* walk and insert more reloads and collect remats */
	sched_foreach_reverse(bb, irn) {
		op_t     *op = get_irn_link(irn);

		if(be_is_Reload(irn) || be_is_Spill(irn)) continue;
		if(is_Phi(irn)) break;

		if(op->is_remat) {
			if(get_irn_mode(irn) != mode_T) {
				insert_remat(si, irn);
			}
		} else {
			int    n;

			for (n=get_irn_arity(irn)-1; n>=0; --n) {
				ir_node    *arg = get_irn_n(irn, n);

				if(op->attr.live_range.args.reloads && op->attr.live_range.args.reloads[n] != ILP_UNDEF) {
					lpp_name_t    *name;

					name = si->lpp->vars[op->attr.live_range.args.reloads[n]];
					if(!is_zero(name->value)) {
						ir_node    *reload;
						ir_node    *insert_pos = irn;
						ir_node    *prev = sched_prev(insert_pos);
						op_t       *prev_op;

						while(be_is_Spill(prev)) {
							prev = sched_prev(prev);
						}

						prev_op = get_irn_link(prev);

						/* insert reload before pre-remats */
						while(!sched_is_end(prev) && !be_is_Reload(prev) && !is_Phi(prev)
								&& prev_op->is_remat && prev_op->attr.remat.pre) {
							insert_pos = prev;

							do {
								prev = sched_prev(prev);
							} while(be_is_Spill(prev));

							prev_op = get_irn_link(prev);

						}

						reload = insert_reload(si, arg, insert_pos);

						assert(reload && "no reload returned");
						set_irn_n(irn, n, reload);

						if(opt_keep_alive & KEEPALIVE_RELOADS)
							pset_insert_ptr(si->spills, reload);
					}
				}
			}
		}
	}

	del_set(spill_bb->ilp);
	if(spill_bb->reloads) del_set(spill_bb->reloads);
}

static void
walker_collect_used(ir_node * irn, void * data)
{
	bitset_t   *used = data;

	bitset_set(used, get_irn_idx(irn));
}

struct kill_helper {
	bitset_t  *used;
	spill_ilp_t  *si;
};

static void
walker_kill_unused(ir_node * bb, void * data)
{
	struct kill_helper *kh = data;
	ir_node            *bad = get_irg_bad(get_irn_irg(bb));
	ir_node            *irn;


	for(irn=sched_first(bb); !sched_is_end(irn);) {
		ir_node     *next = sched_next(irn);
		int          n;

		if(!bitset_is_set(kh->used, get_irn_idx(irn))) {
			if(be_is_Spill(irn) || be_is_Reload(irn)) {
				DBG((kh->si->dbg, LEVEL_1, "\t SUBOPTIMAL! %+F IS UNUSED (cost: %g)\n", irn, get_cost(kh->si, irn)*execution_frequency(kh->si, bb)));
#if 0
				assert(lpp_get_sol_state(kh->si->lpp) != lpp_optimal && "optimal solution is suboptimal?");
#endif
			}

			sched_remove(irn);

			set_nodes_block(irn, bad);
			for (n=get_irn_arity(irn)-1; n>=0; --n) {
				set_irn_n(irn, n, bad);
			}
		}
		irn = next;
	}
}

#ifndef SCHEDULE_PHIM
static void
kill_unused_phims(spill_ilp_t * si, struct kill_helper * kh)
{
	ir_node  *phi;
	ir_node  *bad = get_irg_bad(si->birg->irg);
	int       n;

	pset_foreach(si->phims, phi) {
		if(!bitset_is_set(kh->used, get_irn_idx(phi))) {

			set_nodes_block(phi, bad);
			for (n=get_irn_arity(phi)-1; n>=0; --n) {
				set_irn_n(phi, n, bad);
			}
		}
	}
}
#endif

static void
kill_all_unused_values_in_schedule(spill_ilp_t * si)
{
	struct kill_helper  kh;

	kh.used = bitset_malloc(get_irg_last_idx(si->birg->irg));
	kh.si = si;

	irg_walk_graph(si->birg->irg, walker_collect_used, NULL, kh.used);
#ifndef SCHEDULE_PHIM
	kill_unused_phims(si, &kh);
#endif
	irg_block_walk_graph(si->birg->irg, walker_kill_unused, NULL, &kh);

	bitset_free(kh.used);
}

void
print_irn_pset(pset * p)
{
	ir_node   *irn;

	pset_foreach(p, irn) {
		ir_printf("%+F\n", irn);
	}
}

void
dump_phi_class(spill_ilp_t *si, ir_node **phiclass, const char * file)
{
    FILE           *f = fopen(file, "w");
    ir_node        *irn;
    interference_t *interference;
    int            i;

    set_break(si->interferences);

    ir_fprintf(f, "digraph phiclass {\n");

    for (i = ARR_LEN(phiclass) - 1; i >= 0; --i) {
        irn = phiclass[i];
        if (is_Phi(irn))
            ir_fprintf(f, "  %F%N [shape=box]\n", irn, irn);
    }

    for (i = ARR_LEN(phiclass) - 1; i >= 0; --i) {
        int n;

        irn = phiclass[i];
        if (! is_Phi(irn))
            continue;

        for (n = get_irn_arity(irn) - 1; n >= 0; --n) {
            ir_node  *arg = get_irn_n(irn, n);

            ir_fprintf(f, "  %F%N -> %F%N\n", irn, irn, arg, arg);
        }
    }

    set_foreach(si->interferences, interference) {
        const ir_node *a = interference->a;
        const ir_node *b = interference->b;
        if (get_phi_class(si->pc, (ir_node *)a) == phiclass) {
            ir_fprintf(f, "  %F%N -> %F%N [color=red,dir=none,style=bold]\n", a, a, b, b);
        }
    }

    ir_fprintf(f, "}");
    fclose(f);
}

static void
rewire_uses(spill_ilp_t * si)
{
	defs_t               *defs;
	ir_nodeset_t         ignore;

	ir_nodeset_init(&ignore);
	ir_nodeset_insert(&ignore, get_irg_end(si->birg->irg));

	/* then fix uses of spills */
	set_foreach(si->values, defs) {
		pset           *reloads;
		pset           *spills;
		const ir_node  *next = defs->remats;
		int remats = 0;

		reloads = pset_new_ptr_default();

		while(next) {
			if(be_is_Reload(next)) {
				pset_insert_ptr(reloads, next);
			} else {
				++remats;
			}
			next = get_irn_link(next);
		}

		spills = get_spills_for_value(si, defs->value);
		DBG((si->dbg, LEVEL_2, "\t  %d remats, %d reloads, and %d spills for value %+F\n", remats, pset_count(reloads), pset_count(spills), defs->value));
		if(pset_count(spills) > 1) {
			be_ssa_construction_env_t senv;
			ir_node *node;
			//assert(pset_count(reloads) > 0);
			//				print_irn_pset(spills);
			//				print_irn_pset(reloads);

			be_ssa_construction_init(&senv, si->birg);
			be_ssa_construction_set_ignore_uses(&senv, &ignore);
			pset_foreach(spills, node) {
				be_ssa_construction_add_copy(&senv, node);
			}
			pset_foreach(spills, node) {
				be_ssa_construction_fix_users(&senv, node);
			}
			be_ssa_construction_update_liveness_phis(&senv, si->lv);
			pset_foreach(spills, node) {
				be_liveness_update(si->lv, node);
			}
			be_ssa_construction_destroy(&senv);
		}

		del_pset(reloads);
		del_pset(spills);
	}

	/* first fix uses of remats and reloads */
	set_foreach(si->values, defs) {
		const ir_node  *next = defs->remats;
		int             orig_kept = 0;

		if(next) {
			be_ssa_construction_env_t senv;

			be_ssa_construction_init(&senv, si->birg);

			if(sched_is_scheduled(defs->value)) {
				be_ssa_construction_add_copy(&senv, (ir_node*) defs->value);
				orig_kept = 1;
			}

			next = defs->remats;
			while(next) {
				be_ssa_construction_add_copy(&senv, (ir_node*) next);
				next = get_irn_link(next);
			}

			if(sched_is_scheduled(defs->value)) {
				be_ssa_construction_fix_users(&senv, (ir_node*) defs->value);
			}

			next = defs->remats;
			while(next) {
				be_ssa_construction_fix_users(&senv, (ir_node*) next);
				next = get_irn_link(next);
			}

			be_ssa_construction_update_liveness_phis(&senv, si->lv);
			if(sched_is_scheduled(defs->value)) {
				be_liveness_update(si->lv, (ir_node*) defs->value);
			}

			next = defs->remats;
			while(next) {
				be_liveness_update(si->lv, (ir_node*) next);
				next = get_irn_link(next);
			}

			be_ssa_construction_destroy(&senv);
		}
	}

	ir_nodeset_destroy(&ignore);
//	remove_unused_defs(si);
}


static void
writeback_results(spill_ilp_t * si)
{
	/* walk through the graph and collect all spills, reloads and remats for a value */

	si->values = new_set(cmp_defs, 4096);

	DBG((si->dbg, LEVEL_1, "Applying results\n"));
	delete_unnecessary_remats(si);
	si->m_unknown = new_r_Unknown(si->birg->irg, mode_M);
	irg_block_walk_graph(si->birg->irg, walker_spill_placer, NULL, si);
	irg_block_walk_graph(si->birg->irg, walker_reload_placer, NULL, si);
	if(opt_memoperands)
		insert_memoperands(si);
	phim_fixer(si);

	/* clean the remat info! there are still back-edges leading there! */
	clean_remat_info(si);

	rewire_uses(si);

	connect_all_spills_with_keep(si);

	del_set(si->values);
}

static int
get_n_regs(spill_ilp_t * si)
{
	int       arch_n_regs = arch_register_class_n_regs(si->cls);

	bitset_t *arch_regs = bitset_malloc(arch_n_regs);
	bitset_t *abi_regs = bitset_malloc(arch_n_regs);

	arch_put_non_ignore_regs(si->birg->main_env->arch_env, si->cls, arch_regs);
    be_abi_put_ignore_regs(si->birg->abi, si->cls, abi_regs);

	bitset_andnot(arch_regs, abi_regs);
	arch_n_regs = bitset_popcnt(arch_regs);

	bitset_free(arch_regs);
	bitset_free(abi_regs);

	DBG((si->dbg, LEVEL_1, "\tArchitecture has %d free registers in class %s\n", arch_n_regs, si->cls->name));
	return arch_n_regs;
}

static void
walker_reload_mover(ir_node * bb, void * data)
{
	spill_ilp_t   *si = data;
	ir_node		  *tmp;

	sched_foreach(bb, tmp) {
		if(be_is_Reload(tmp) && has_reg_class(si, tmp)) {
			ir_node       *reload = tmp;
			ir_node       *irn = tmp;

			/* move reload upwards */

			int pressure = (int)get_irn_link(reload);
			if(pressure < si->n_regs) {
				irn = sched_prev(reload);
				DBG((si->dbg, LEVEL_5, "regpressure before %+F: %d\n", reload, pressure));
				sched_remove(reload);
				pressure = (int)get_irn_link(irn);

				while(pressure < si->n_regs) {
					if( sched_is_end(irn) ||
					   (be_is_Reload(irn) && has_reg_class(si, irn)) ||
					   /* do not move reload before its spill */
					   (irn == be_get_Reload_mem(reload)) ||
					   /* do not move before phi */
					   is_Phi(irn)) break;

					set_irn_link(irn, INT_TO_PTR(pressure+1));
					DBG((si->dbg, LEVEL_5, "new regpressure before %+F: %d\n", irn, pressure+1));
					irn = sched_prev(irn);

					pressure = (int)get_irn_link(irn);
				}

				DBG((si->dbg, LEVEL_3, "putting reload %+F after %+F\n", reload, irn));
				sched_put_after(irn, reload);
			}
		}
	}
}

static void
move_reloads_upward(spill_ilp_t * si)
{
	irg_block_walk_graph(si->birg->irg, walker_reload_mover, NULL, si);
}


/**
 * Walk all irg blocks and check for interfering spills inside of phi classes
 */
static void
luke_meminterferencechecker(ir_node * bb, void * data)
{
	spill_ilp_t *si = (spill_ilp_t*)data;
	int         l1, l2;

	be_lv_foreach(si->lv, bb, be_lv_state_end | be_lv_state_out | be_lv_state_in, l1) {
		ir_node        *a = be_lv_get_irn(si->lv, bb, l1);

		if (! be_is_Spill(a) && (!is_Phi(a) || get_irn_mode(a) != mode_T))
			continue;

		/* a is only interesting if it is in my register class and if it is inside a phi class */
		if (has_reg_class(si, a) && get_phi_class(si->pc, a)) {
			for (l2 = _be_lv_next_irn(si->lv, bb, 0xff, l1 + 1); l2 >= 0; l2 = _be_lv_next_irn(si->lv, bb, 0xff, l2 + 1)) {
				ir_node *b = be_lv_get_irn(si->lv, bb, l2);

				if (! be_is_Spill(b) && (! is_Phi(b) || get_irn_mode(b) != mode_T))
					continue;

				/* a and b are only interesting if they are in the same phi class */
				if (has_reg_class(si, b) && get_phi_class(si->pc, a) == get_phi_class(si->pc, b)) {
					if (values_interfere_in_block(si, bb, a, b)) {
						ir_fprintf(stderr, "$$ Spills interfere in %+F: %+F, %+F \t$$\n", bb, a, b);
					}
				}
			}
		}
	}
}

static void
verify_phiclasses(spill_ilp_t * si)
{
	/* analyze phi classes */
	phi_class_free(si->pc);
	si->pc = phi_class_new_from_irg(si->birg->irg, 0);

	DBG((si->dbg, LEVEL_2, "\t calling memory interference checker\n"));
	irg_block_walk_graph(si->birg->irg, luke_meminterferencechecker, NULL, si);
}

void
be_spill_remat(be_irg_t *birg, const arch_register_class_t *cls)
{
	char            buf[256];
	char            problem_name[256];
	char            dump_suffix[256];
	char            dump_suffix2[256];
	struct obstack  obst;
	spill_ilp_t     si;
	ir_graph       *irg = be_get_birg_irg(birg);

	ir_snprintf(problem_name, sizeof(problem_name), "%F_%s", irg, cls->name);
	ir_snprintf(dump_suffix, sizeof(dump_suffix), "-%s-remats", cls->name);
	ir_snprintf(dump_suffix2, sizeof(dump_suffix2), "-%s-pressure", cls->name);

	FIRM_DBG_REGISTER(si.dbg, "firm.be.ra.spillremat");
	DBG((si.dbg, LEVEL_1, "\n\n\t\t===== Processing %s =====\n\n", problem_name));

	if(opt_verify & VERIFY_DOMINANCE)
		be_check_dominance(irg);

	be_assure_dom_front(birg);
	be_assure_liveness(birg);

	obstack_init(&obst);
	si.obst                = &obst;
	si.birg                = birg;
	si.cls                 = cls;
	si.lpp                 = new_lpp(problem_name, lpp_minimize);
	si.remat_info          = new_set(cmp_remat_info, 4096);
	si.interferences       = new_set(cmp_interference, 32);
	si.memoperands         = new_set(cmp_memoperands, 128);
	si.all_possible_remats = pset_new_ptr_default();
	si.spills              = pset_new_ptr_default();
	si.inverse_ops         = pset_new_ptr_default();
	si.lv                  = birg->lv;
	si.keep                = NULL;
	si.n_regs              = get_n_regs(&si);

	set_irg_link(irg, &si);
	compute_doms(irg);

	/* compute phi classes */
	// phi_class_compute(irg);

	if(opt_dump_flags & DUMP_STATS)
		be_analyze_regpressure(birg, cls, "-pre");

	DBG((si.dbg, LEVEL_2, "\t initializing\n"));
	irg_block_walk_graph(irg, luke_initializer, NULL, &si);

	if(opt_remats) {
		/* collect remats */
		DBG((si.dbg, LEVEL_1, "Collecting remats\n"));
		irg_walk_graph(irg, walker_remat_collector, NULL, &si);
	}

	/* insert possible remats */
	DBG((si.dbg, LEVEL_1, "Inserting possible remats\n"));
	irg_block_walk_graph(irg, walker_remat_insertor, NULL, &si);
	DBG((si.dbg, LEVEL_2, " -> inserted %d possible remats\n", pset_count(si.all_possible_remats)));

	if(opt_keep_alive & KEEPALIVE_REMATS) {
		DBG((si.dbg, LEVEL_1, "Connecting remats with keep and dumping\n"));
		connect_all_remats_with_keep(&si);
		/* dump graph with inserted remats */
		dump_graph_with_remats(irg, dump_suffix);
	}

	/* insert copies for phi arguments not in my regclass */
	irg_walk_graph(irg, walker_regclass_copy_insertor, NULL, &si);

	/* recompute liveness */
	DBG((si.dbg, LEVEL_1, "Recomputing liveness\n"));
	be_liveness_recompute(si.lv);

	/* build the ILP */
	DBG((si.dbg, LEVEL_1, "\tBuilding ILP\n"));
	DBG((si.dbg, LEVEL_2, "\t endwalker\n"));
	irg_block_walk_graph(irg, luke_endwalker, NULL, &si);

	DBG((si.dbg, LEVEL_2, "\t blockwalker\n"));
	irg_block_walk_graph(irg, luke_blockwalker, NULL, &si);

	si.pc = phi_class_new_from_irg(birg->irg, 0);
	if (opt_memcopies) {
		DBG((si.dbg, LEVEL_2, "\t memcopyhandler\n"));
		memcopyhandler(&si);
	}

	if (opt_dump_flags & DUMP_PROBLEM) {
		FILE           *f;
		ir_snprintf(buf, sizeof(buf), "%s-spillremat.ilp", problem_name);
		if ((f = fopen(buf, "wt")) != NULL) {
			lpp_dump_plain(si.lpp, f);
			fclose(f);
		}
	}

	if (opt_dump_flags & DUMP_MPS) {
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "%s-spillremat.mps", problem_name);
		if ((f = fopen(buf, "wt")) != NULL) {
			mps_write_mps(si.lpp, s_mps_fixed, f);
			fclose(f);
		}

		ir_snprintf(buf, sizeof(buf), "%s-spillremat.mst", problem_name);
		if ((f = fopen(buf, "wt")) != NULL) {
			mps_write_mst(si.lpp, s_mps_fixed, f);
			fclose(f);
		}
	}

	lpp_check_startvals(si.lpp);

#ifdef SOLVE
	DBG((si.dbg, LEVEL_1, "\tSolving %s (%d variables, %d constraints)\n", problem_name, si.lpp->var_next, si.lpp->cst_next));
	lpp_set_time_limit(si.lpp, opt_timeout);

	if(opt_log)
		lpp_set_log(si.lpp, stdout);

#ifdef SOLVE_LOCAL
	lpp_solve_cplex(si.lpp);
#else
	lpp_solve_net(si.lpp, LPP_SERVER, LPP_SOLVER);
#endif
	assert(lpp_is_sol_valid(si.lpp)
	       && "solution of ILP must be valid");

	DBG((si.dbg, LEVEL_1, "\t%s: iterations: %d, solution time: %g, objective function: %g, best bound: %g\n", problem_name, si.lpp->iterations, si.lpp->sol_time, is_zero(si.lpp->objval)?0.0:si.lpp->objval, is_zero(si.lpp->best_bound)?0.0:si.lpp->best_bound));

	if(opt_dump_flags & DUMP_SOLUTION) {
		FILE           *f;
		char            buf[256];

		ir_snprintf(buf, sizeof(buf), "%s-spillremat.sol", problem_name);
		if ((f = fopen(buf, "wt")) != NULL) {
			int             i;
			for (i = 0; i < si.lpp->var_next; ++i) {
				lpp_name_t     *name = si.lpp->vars[i];
				fprintf(f, "%20s %4d %10f\n", name->name, name->nr, name->value);
			}
			fclose(f);
		}
	}

#ifndef SCHEDULE_PHIM
	si.phims = pset_new_ptr_default();
#endif
	writeback_results(&si);


#endif				/* SOLVE */

	kill_all_unused_values_in_schedule(&si);

#if !defined(SCHEDULE_PHIM) && defined(SOLVE)
	del_pset(si.phims);
#endif

	if(opt_keep_alive & (KEEPALIVE_SPILLS | KEEPALIVE_RELOADS))
		be_dump(irg, "-spills-placed", dump_ir_block_graph);

	// move reloads upwards
	be_liveness_recompute(si.lv);
	irg_block_walk_graph(irg, walker_pressure_annotator, NULL, &si);
	move_reloads_upward(&si);

	if(opt_memcopies) {
		verify_phiclasses(&si);
	}

	irg_block_walk_graph(irg, walker_pressure_annotator, NULL, &si);

	if(opt_dump_flags & DUMP_PRESSURE)
		dump_pressure_graph(&si, dump_suffix2);

	if(opt_dump_flags & DUMP_STATS)
		be_analyze_regpressure(birg, cls, "-post");

	if(opt_verify & VERIFY_DOMINANCE)
		be_check_dominance(irg);

	free_dom(irg);
	del_set(si.interferences);
	del_pset(si.inverse_ops);
	del_pset(si.all_possible_remats);
	del_set(si.memoperands);
	del_pset(si.spills);
	free_lpp(si.lpp);
	phi_class_free(si.pc);
	obstack_free(&obst, NULL);
	DBG((si.dbg, LEVEL_1, "\tdone.\n"));
}

void be_init_spillremat(void)
{
	static be_spiller_t remat_spiller = {
		be_spill_remat
	};
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp = lc_opt_get_grp(ra_grp, "chordal");
	lc_opt_entry_t *remat_grp = lc_opt_get_grp(chordal_grp, "remat");

	be_register_spiller("remat", &remat_spiller);
	lc_opt_add_table(remat_grp, options);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillremat);

#else				/* WITH_ILP */

static void INLINE
only_that_you_can_compile_without_WITH_ILP_defined(void)
{
}

#endif				/* WITH_ILP */
