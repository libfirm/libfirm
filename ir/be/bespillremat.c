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
#include "phiclass.h"
#include "iredges.h"
#include "execfreq.h"

#include <lpp/lpp.h>
#include <lpp/lpp_net.h>
#include <lpp/lpp_cplex.h>
//#include <lc_pset.h>
#include <libcore/lc_bitset.h>

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch.h"
#include "benode_t.h"
#include "beutil.h"
#include "bespillremat.h"
#include "bespill.h"

#include "bechordal_t.h"

#define BIGM 100000.0

#define DUMP_SOLUTION
#define DUMP_ILP
//#define KEEPALIVE /* keep alive all inserted remats and dump graph with remats */
#define COLLECT_REMATS /* enable rematerialization */
#define COLLECT_INVERSE_REMATS /* enable placement of inverse remats */
#define REMAT_WHILE_LIVE /* only remat values that are live */
//#define NO_ENLARGE_L1V3N355 /* do not remat after the death of some operand */
//#define EXECFREQ_LOOPDEPH /* compute execution frequency from loop depth only */
//#define MAY_DIE_AT_PRE_REMAT /* allow values to die after a pre remat */
#define CHECK_POST_REMAT /* check pressure after post remats (conservative but otherwise we can temporarily exceed the register pressure) */
#define NO_SINGLE_USE_REMATS /* do not repair schedule */
//#define KEEPALIVE_SPILLS
//#define KEEPALIVE_RELOADS
#define GOODWIN_REDUCTION

#define  SOLVE
//#define  SOLVE_LOCAL
#define LPP_SERVER "i44pc52"
#define LPP_SOLVER "cplex"

#define COST_LOAD      10
#define COST_STORE     50
#define COST_REMAT     1

#define ILP_TIMEOUT    90

#define ILP_UNDEF		-1

typedef struct _spill_ilp_t {
	const arch_register_class_t  *cls;
	int                           n_regs;
	const be_chordal_env_t       *chordal_env;
	spill_env_t                  *senv;
	lpp_t                        *lpp;
	struct obstack               *obst;
	set                          *remat_info;
	pset                         *all_possible_remats;
	pset                         *inverse_ops;
#ifdef KEEPALIVE
	ir_node                      *keep;
#endif
	set                          *values; /**< for collecting all definitions of values before running ssa-construction */
	set                          *execfreqs;
	pset                         *spills;
	ir_node                      *m_unknown;
	DEBUG_ONLY(firm_dbg_module_t * dbg);
} spill_ilp_t;

typedef int ilp_var_t;
typedef int ilp_cst_t;

typedef struct _spill_bb_t {
	set          *ilp;
	pset         *copys_needed;
	ilp_var_t    *reloads;
} spill_bb_t;

typedef struct _remat_t {
	const ir_node        *op; /**< for copy_irn */
	const ir_node        *proj; /**< not NULL if the above op produces a tuple */
	const ir_node        *value; /**< the value which is being recomputed by this remat */
	int                   cost; /**< cost of this remat */
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
			remat_t        *remat; /** the remat this op belongs to */
			int             pre; /** 1, if this is a pressure-increasing remat */
		} remat;
		struct {
			ilp_var_t       ilp;
			ir_node        *op; /** the operation this live range belongs to */
			ilp_var_t      *reloads;
		} live_range;
	} attr;
} op_t;

typedef struct _defs_t {
	ir_node   *value;
	ir_node   *spills;  /**< points to the first spill for this value (linked by link field) */
	ir_node   *remats;  /**< points to the first definition for this value (linked by link field) */
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

static defs_t *
set_find_def(set * set, ir_node * value)
{
	defs_t     query;

	query.value = value;
	return set_find(set, &query, sizeof(query), HASH_PTR(value));
}

static defs_t *
set_insert_def(set * set, ir_node * value)
{
	defs_t     query;

	query.value = value;
	query.spills = NULL;
	query.remats = NULL;
	return set_insert(set, &query, sizeof(query), HASH_PTR(value));
}

static spill_t *
set_find_spill(set * set, ir_node * value)
{
	spill_t     query;

	query.irn = value;
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
execution_frequency(const spill_ilp_t * si, const ir_node * irn)
{
	if(si->execfreqs) {
		if(is_Block(irn)) {
			return get_block_execfreq(si->execfreqs, irn);
		} else {
			return get_block_execfreq(si->execfreqs, get_nodes_block(irn));
		}
	} else {
		if(is_Block(irn))
			return exp(get_loop_depth(get_irn_loop(irn)) * log(10));
		else
			return exp(get_loop_depth(get_irn_loop(get_nodes_block(irn))) * log(10));
	}
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
			op = get_irn_n(op, 0);
			proj = dest_value;
		}

		if(!is_rematerializable(si, op))
			return NULL;

		remat = obstack_alloc(si->obst, sizeof(*remat));
		remat->op = op;
		remat->cost = arch_get_op_estimated_cost(si->chordal_env->birg->main_env->arch_env, op);
		remat->value = dest_value;
		remat->proj = proj;
		remat->inverse = 0;
	} else {
		arch_inverse_t     inverse;
		int                i,
						   n;

		/* get the index of the operand we want to retrieve by the inverse op */
		for (i = 0, n = get_irn_arity(op); i < n; ++i) {
			ir_node        *arg = get_irn_n(op, i);

			if(arg == dest_value) break;
		}
		if(i == n) return NULL;

		DBG((si->dbg, LEVEL_5, "\t  requesting inverse op for argument %d of op %+F\n", i, op));

		/* else ask the backend to give an inverse op */
		if(arch_get_inverse(si->chordal_env->birg->main_env->arch_env, op, i, &inverse, si->obst)) {
			int   i;

			DBG((si->dbg, LEVEL_4, "\t  backend gave us an inverse op with %d nodes and cost %d\n", inverse.n, inverse.costs));

			assert(inverse.n > 0 && "inverse op should have at least one node");

			for(i=0; i<inverse.n; ++i) {
				pset_insert_ptr(si->inverse_ops, inverse.nodes[i]);
			}

			if(inverse.n <= 2) {
				remat = obstack_alloc(si->obst, sizeof(*remat));
				remat->op = inverse.nodes[0];
				remat->cost = inverse.costs;
				remat->value = dest_value;
				remat->proj = (inverse.n==2)?inverse.nodes[1]:NULL;
				remat->inverse = 1;

				assert(is_Proj(remat->proj));
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
	int              i,
					 n;

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
	for (i = 0, n = get_irn_arity(remat->op); i < n; ++i) {
		ir_node        *arg = get_irn_n(remat->op, i);

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

static INLINE void
get_remats_from_op(spill_ilp_t * si, const ir_node * op)
{
	int       i,
		      n;
	remat_t *remat;

#ifdef NO_SINGLE_USE_REMATS
	if(has_reg_class(si, op) && (get_irn_n_nonremat_edges(si, op) > 1)) {
#else
	if(has_reg_class(si, op)) {
#endif
		remat = get_remat_from_op(si, op, op);
		if(remat) {
			add_remat(si, remat);
		}
	}

#ifdef COLLECT_INVERSE_REMATS
	/* repeat the whole stuff for each remat retrieved by get_remat_from_op(op, arg)
	   for each arg */
	for (i = 0, n = get_irn_arity(op); i < n; ++i) {
		ir_node        *arg = get_irn_n(op, i);

		if(has_reg_class(si, arg)) {
			/* try to get an inverse remat */
			remat = get_remat_from_op(si, arg, op);
			if(remat) {
				add_remat(si, remat);
			}
		}
	}
#endif

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
    return sched_skip((ir_node*)bb, 0, sched_skip_cf_predicator, (void *) si->chordal_env->birg->main_env->arch_env);
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
  sched_add_after(insert, irn);
}

/**
 * Tells you whether a @p remat can be placed before the irn @p pos
 */
static INLINE int
can_remat_before(const spill_ilp_t * si, const remat_t * remat, const ir_node * pos, const pset * live)
{
	const ir_node   *op = remat->op;
	const ir_node   *prev;
	int        i,
			   n,
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

	for(i=0, n=get_irn_arity(op); i<n && res; ++i) {
		const ir_node   *arg = get_irn_n(op, i);

#ifdef NO_ENLARGE_L1V3N355
		if(has_reg_class(si, arg) && live) {
			res &= pset_find_ptr(live, arg)?1:0;
		} else {
			res &= value_is_defined_before(si, pos, arg);
		}
#else
		res &= value_is_defined_before(si, pos, arg);
#endif
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
	set_nodes_block(copy, bb);
	sched_put_after(pos, copy);

	return copy;
}

static void
insert_remat_after(spill_ilp_t * si, const remat_t * remat, const ir_node * pos, const pset * live)
{
	char     buf[256];

	if(can_remat_after(si, remat, pos, live)) {
		ir_node         *copy,
						*proj_copy;
		op_t            *op;

		DBG((si->dbg, LEVEL_3, "\t  >inserting remat %+F\n", remat->op));

		copy = insert_copy_after(si, remat->op, pos);

//		ir_snprintf(buf, sizeof(buf), "remat2_%N_%N", remat->value, pos);
		ir_snprintf(buf, sizeof(buf), "remat2_%N_%N", copy, pos);
		op = obstack_alloc(si->obst, sizeof(*op));
		op->is_remat = 1;
		op->attr.remat.remat = remat;
		op->attr.remat.pre = 0;
		op->attr.remat.ilp = lpp_add_var(si->lpp, buf, lpp_binary, remat->cost*execution_frequency(si, pos));

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
	}
}

static void
insert_remat_before(spill_ilp_t * si, const remat_t * remat, const ir_node * pos, const pset * live)
{
	char     buf[256];

	if(can_remat_before(si, remat, pos, live)) {
		ir_node         *copy,
						*proj_copy;
		op_t            *op;

		DBG((si->dbg, LEVEL_3, "\t  >inserting remat %+F\n", remat->op));

		copy = insert_copy_before(si, remat->op, pos);

//		ir_snprintf(buf, sizeof(buf), "remat_%N_%N", remat->value, pos);
		ir_snprintf(buf, sizeof(buf), "remat_%N_%N", copy, pos);
		op = obstack_alloc(si->obst, sizeof(*op));
		op->is_remat = 1;
		op->attr.remat.remat = remat;
		op->attr.remat.pre = 1;
		op->attr.remat.ilp = lpp_add_var(si->lpp, buf, lpp_binary, remat->cost*execution_frequency(si, pos));

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
	}
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
is_merge_edge(const ir_node * bb)
{
#ifdef GOODWIN_REDUCTION
	return get_block_n_succs(bb) == 1;
#else
	return 1;
#endif
}

static int
is_diverge_edge(const ir_node * bb)
{
#ifdef GOODWIN_REDUCTION
	return get_Block_n_cfgpreds(bb) == 1;
#else
	return 1;
#endif
}

/**
 * Insert (so far unused) remats into the irg to
 * recompute the potential liveness of all values
 */
static void
walker_remat_insertor(ir_node * bb, void * data)
{
	spill_ilp_t    *si = data;
	spill_bb_t     *spill_bb;
	ir_node        *irn;
	int             i,
					n;
	irn_live_t     *li;
	pset           *live = pset_new_ptr_default();

	DBG((si->dbg, LEVEL_3, "\t Entering %+F\n\n", bb));

	live_foreach(bb, li) {
		ir_node        *value = (ir_node *) li->irn;

		/* add remats at end of block */
		if (live_is_end(li) && has_reg_class(si, value)) {
			pset_insert_ptr(live, value);
		}
	}

	spill_bb = obstack_alloc(si->obst, sizeof(*spill_bb));
	set_irn_link(bb, spill_bb);

	irn = sched_last(bb);
	while(!sched_is_end(irn)) {
		ir_node   *next;
		op_t      *op;
		pset      *args;
		ir_node   *arg;

		next = sched_prev(irn);

		DBG((si->dbg, LEVEL_5, "\t at %+F (next: %+F)\n", irn, next));

		if(is_Phi(irn) || is_Proj(irn)) {
			op_t      *op;

			if(has_reg_class(si, irn)) {
				pset_remove_ptr(live, irn);
			}

			op = obstack_alloc(si->obst, sizeof(*op));
			op->is_remat = 0;
			op->attr.live_range.reloads = NULL;
			op->attr.live_range.ilp = ILP_UNDEF;
			set_irn_link(irn, op);

			irn = next;
			continue;
		}

		op = obstack_alloc(si->obst, sizeof(*op));
		op->is_remat = 0;
		op->attr.live_range.ilp = ILP_UNDEF;
		op->attr.live_range.reloads = obstack_alloc(si->obst, sizeof(*op->attr.live_range.reloads) * get_irn_arity(irn));
		memset(op->attr.live_range.reloads, 0xFF, sizeof(*op->attr.live_range.reloads) * get_irn_arity(irn));
		set_irn_link(irn, op);

		args = pset_new_ptr_default();

		/* collect arguments of op */
		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node        *arg = get_irn_n(irn, i);

			pset_insert_ptr(args, arg);
		}

		/* set args of op live in epilog */
		pset_foreach(args, arg) {
			if(has_reg_class(si, arg)) {
				pset_insert_ptr(live, arg);
			}
		}

		/* insert all possible remats after irn */
		pset_foreach(args, arg) {
			remat_info_t   *remat_info,
						    query;
			remat_t        *remat;

			/* continue if the operand has the wrong reg class
			 */
			if(!has_reg_class(si, arg))
				continue;

			query.irn = arg;
			query.remats = NULL;
			query.remats_by_operand = NULL;
			remat_info = set_find(si->remat_info, &query, sizeof(query), HASH_PTR(arg));

			if(!remat_info) {
				continue;
			}

			/* do not place post remats after jumps */
			if(sched_skip_cf_predicator(irn, si->chordal_env->birg->main_env->arch_env)) continue;

			if(remat_info->remats_by_operand) {
				pset_foreach(remat_info->remats_by_operand, remat) {
					/* do not insert remats producing the same value as one of the operands */
					if(!pset_find_ptr(args, remat->value)) {
						DBG((si->dbg, LEVEL_4, "\t  considering remat %+F with arg %+F\n", remat->op, arg));
#ifdef REMAT_WHILE_LIVE
						if(pset_find_ptr(live, remat->value)) {
							insert_remat_after(si, remat, irn, live);
						}
#else
						insert_remat_after(si, remat, irn, live);
#endif
					}
				}
			}
		}

		/* delete defined value from live set */
		if(has_reg_class(si, irn)) {
			pset_remove_ptr(live, irn);
		}

		/* insert all possible remats before irn */
		pset_foreach(args, arg) {
			remat_info_t   *remat_info,
						    query;
			remat_t        *remat;

			/* continue if the operand has the wrong reg class
			 */
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
					DBG((si->dbg, LEVEL_4, "\t  considering remat %+F for arg %+F\n", remat->op, arg));
#ifdef REMAT_WHILE_LIVE
					if(pset_find_ptr(live, remat->value)) {
						insert_remat_before(si, remat, irn, live);
					}
#else
					insert_remat_before(si, remat, irn, live);
#endif
				}
			}
		}

		del_pset(args);
		irn = next;
	}

	live_foreach(bb, li) {
		ir_node        *value = (ir_node *) li->irn;

		/* add remats at end if successor has multiple predecessors */
		if(is_merge_edge(bb)) {
			/* add remats at end of block */
			if (live_is_end(li) && has_reg_class(si, value)) {
				remat_info_t   *remat_info,
							   query;
				remat_t        *remat;

				query.irn = value;
				query.remats = NULL;
				query.remats_by_operand = NULL;
				remat_info = set_find(si->remat_info, &query, sizeof(query), HASH_PTR(value));

				if(remat_info && remat_info->remats) {
					pset_foreach(remat_info->remats, remat) {
						DBG((si->dbg, LEVEL_4, "\t  considering remat %+F at end of block %+F\n", remat->op, bb));

						insert_remat_before(si, remat, bb, NULL);
					}
				}
			}

		}
		if(is_diverge_edge(bb) > 1) {
			/* add remat2s at beginning of block */
			if ((live_is_in(li) || (is_Phi(value) && get_nodes_block(value)==bb)) && has_reg_class(si, value)) {
				remat_info_t   *remat_info,
							   query;
				remat_t        *remat;

				query.irn = value;
				query.remats = NULL;
				query.remats_by_operand = NULL;
				remat_info = set_find(si->remat_info, &query, sizeof(query), HASH_PTR(value));

				if(remat_info && remat_info->remats) {
					pset_foreach(remat_info->remats, remat) {
						DBG((si->dbg, LEVEL_4, "\t  considering remat %+F at beginning of block %+F\n", remat->op, bb));

						/* put the remat here if all its args are available */
						insert_remat_after(si, remat, bb, NULL);

					}
				}
			}
		}
	}
}

/**
 * Preparation of blocks' ends for Luke Blockwalker(tm)(R)
 */
static void
luke_endwalker(ir_node * bb, void * data)
{
	spill_ilp_t    *si = (spill_ilp_t*)data;
	irn_live_t     *li;
	pset           *live;
	pset           *use_end;
	char            buf[256];
	ilp_cst_t       cst;
	ir_node        *irn;
	spill_bb_t     *spill_bb = get_irn_link(bb);


	live = pset_new_ptr_default();
	use_end = pset_new_ptr_default();

	live_foreach(bb, li) {
		irn = (ir_node *) li->irn;
		if (live_is_end(li) && has_reg_class(si, irn) && !pset_find_ptr(si->all_possible_remats, irn)) {
			op_t      *op;

			pset_insert_ptr(live, irn);
			op = get_irn_link(irn);
			assert(!op->is_remat);
		}
	}

	/* collect values used by cond jumps etc. at bb end (use_end) -> always live */
	/* their reg_out is unimportant because it can always be set */
	sched_foreach_reverse(bb, irn) {
		int   i,
			  n;

		if(!sched_skip_cf_predicator(irn, si->chordal_env->birg->main_env->arch_env)) break;

		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node        *irn_arg = get_irn_n(irn, i);
			if(has_reg_class(si, irn_arg)) {
				pset_insert_ptr(use_end, irn);
			}
		}
	}

	ir_snprintf(buf, sizeof(buf), "check_end_%N", bb);
	cst = lpp_add_cst(si->lpp, buf, lpp_less, si->n_regs - pset_count(use_end));

	spill_bb->ilp = new_set(cmp_spill, 16);

	live_foreach(bb, li) {
		irn = (ir_node *) li->irn;
		if (live_is_end(li) && has_reg_class(si, irn) && !pset_find_ptr(si->all_possible_remats, irn)) {
			spill_t     query,
			           *spill;

			query.irn = irn;
			spill = set_insert(spill_bb->ilp, &query, sizeof(query), HASH_PTR(irn));

			ir_snprintf(buf, sizeof(buf), "reg_out_%N_%N", irn, bb);
			spill->reg_out = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);
			/* if irn is used at the end of the block, then it is live anyway */
			if(!pset_find_ptr(use_end, irn))
				lpp_set_factor_fast(si->lpp, cst, spill->reg_out, 1.0);

			ir_snprintf(buf, sizeof(buf), "mem_out_%N_%N", irn, bb);
			spill->mem_out = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

			if(is_diverge_edge(bb)) {
				ir_snprintf(buf, sizeof(buf), "spill_%N_%N", irn, bb);
				spill->spill = lpp_add_var(si->lpp, buf, lpp_binary, COST_STORE*execution_frequency(si, bb));
			} else {
				spill->spill = ILP_UNDEF;
			}

			spill->reg_in = ILP_UNDEF;
			spill->mem_in = ILP_UNDEF;
		}
	}

	del_pset(live);
	del_pset(use_end);
}

static ir_node *
next_post_remat(const ir_node * irn)
{
	op_t      *op;

	if(is_Block(irn)) {
		irn = sched_block_first_nonphi(irn);
	} else {
		irn = sched_next_op(irn);
	}

	if(sched_is_end(irn))
		return NULL;

	op = (op_t*)get_irn_link(irn);
	if(op->is_remat && !op->attr.remat.pre) {
		return irn;
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

/**
 * Find a remat of value @p value in the prolog of @p pos
 */
static ir_node *
find_pre_remat(const spill_ilp_t * si, const ir_node * value, const ir_node * pos)
{
	while((pos = next_pre_remat(si,pos)) != NULL) {
		op_t   *op;

		op = get_irn_link(pos);
		assert(op->is_remat && op->attr.remat.pre);

		if(op->attr.remat.remat->value == value)
			return (ir_node*)pos;
	}

	return NULL;
}

static spill_t *
add_to_spill_bb(spill_ilp_t * si, ir_node * bb, ir_node * irn)
{
	spill_bb_t  *spill_bb = get_irn_link(bb);
	spill_t     *spill,
				 query;
	char         buf[256];

	query.irn = irn;
	spill = set_find(spill_bb->ilp, &query, sizeof(query), HASH_PTR(irn));
	if(!spill) {
		spill = set_insert(spill_bb->ilp, &query, sizeof(query), HASH_PTR(irn));

		spill->reg_out = ILP_UNDEF;
		spill->reg_in  = ILP_UNDEF;
		spill->mem_in  = ILP_UNDEF;

		ir_snprintf(buf, sizeof(buf), "mem_out_%N_%N", irn, bb);
		spill->mem_out = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

		if(is_diverge_edge(bb)) {
			ir_snprintf(buf, sizeof(buf), "spill_%N_%N", irn, bb);
			spill->spill = lpp_add_var(si->lpp, buf, lpp_binary, COST_STORE*execution_frequency(si, bb));
		} else {
			spill->spill = ILP_UNDEF;
		}
	}

	return spill;
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
	ilp_cst_t       cst;
	spill_bb_t     *spill_bb = get_irn_link(bb);
	int             i;
	ir_node        *tmp;
	spill_t        *spill;


	live = pset_new_ptr_default();

	/* do something at the end of the block */

	/* init live values at end of block */
	live_foreach(bb, li) {
		ir_node        *irn = (ir_node *) li->irn;

		if (live_is_end(li) && has_reg_class(si, irn) && !pset_find_ptr(si->all_possible_remats, irn)) {
			pset_insert_ptr(live, irn);
		}
	}

	if(is_merge_edge(bb)) {
		spill_bb->reloads = obstack_alloc(si->obst, pset_count(live) * sizeof(*spill_bb->reloads));
		memset(spill_bb->reloads, 0xFF, pset_count(live) * sizeof(*spill_bb->reloads));
	} else {
		spill_bb->reloads = NULL;
	}

	i=0;
	live_foreach(bb, li) {
		ir_node        *irn = (ir_node *) li->irn;
		op_t           *op;

		if (live_is_end(li) && has_reg_class(si, irn) && !pset_find_ptr(si->all_possible_remats, irn)) {
			spill = set_find_spill(spill_bb->ilp, irn);
			assert(spill);

			if(spill_bb->reloads) {
				ir_snprintf(buf, sizeof(buf), "reload_%N_%N", bb, irn);
				spill_bb->reloads[i] = lpp_add_var(si->lpp, buf, lpp_binary, COST_LOAD*execution_frequency(si, bb));

				/* reload <= mem_out */
				cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
				lpp_set_factor_fast(si->lpp, cst, spill_bb->reloads[i], 1.0);
				lpp_set_factor_fast(si->lpp, cst, spill->mem_out, -1.0);
			}

			op = get_irn_link(irn);
			assert(!op->is_remat);

			ir_snprintf(buf, sizeof(buf), "lr_%N_%N", irn, bb);
			op->attr.live_range.ilp = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);
			op->attr.live_range.op = bb;

			ir_snprintf(buf, sizeof(buf), "reg_out_%N_%N", bb, irn);
			cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

			/* reg_out - reload - remat - live_range <= 0 */
			lpp_set_factor_fast(si->lpp, cst, spill->reg_out, 1.0);
			if(spill_bb->reloads) lpp_set_factor_fast(si->lpp, cst, spill_bb->reloads[i], -1.0);
			lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.ilp, -1.0);
			foreach_pre_remat(si, bb, tmp) {
				op_t     *remat_op = get_irn_link(tmp);
				if(remat_op->attr.remat.remat->value == irn) {
					lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
				}
			}

			++i;
		}
	}
	DBG((si->dbg, LEVEL_4, "\t   %d values live at end of block %+F\n", pset_count(live), bb));

	sched_foreach_reverse(bb, irn) {
		op_t       *op;
		op_t       *tmp_op;
		int         n,
					k,
					d = 0;
		ilp_cst_t	check_pre,
					check_post;
#ifdef CHECK_POST_REMAT
		ilp_cst_t	check_post_remat;
#endif
		set        *args = new_set(cmp_keyval, get_irn_arity(irn));
		keyval_t   *keyval;

		if(is_Phi(irn))
			break;

		op = get_irn_link(irn);
		/* skip remats */
		if(op->is_remat) continue;
		DBG((si->dbg, LEVEL_4, "\t  at node %+F\n", irn));

		if(has_reg_class(si, irn)) {
			assert(pset_find_ptr(live, irn));
			pset_remove_ptr(live, irn);
		}

		/* init set of irn's arguments */
		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node        *irn_arg = get_irn_n(irn, i);
			if(has_reg_class(si, irn_arg)) {
				set_insert_keyval(args, irn_arg, (void*)i);
			}
		}

#ifdef CHECK_POST_REMAT
		/* check the register pressure after the epilog */
		ir_snprintf(buf, sizeof(buf), "check_post_remat_%N", irn);
		check_post_remat = lpp_add_cst(si->lpp, buf, lpp_less, si->n_regs);

		/* iterate over L\U */
		pset_foreach(live, tmp) {
			if(!set_find_keyval(args, tmp)) {
				/* if a live value is not used by irn */
				tmp_op = get_irn_link(tmp);
//				assert(tmp_op->attr.live_range.op != irn);
				lpp_set_factor_fast(si->lpp, check_post_remat, tmp_op->attr.live_range.ilp, 1.0);
			}
		}
		/* iterate over following remats and remove possibly defined values again from check_post_remat */
		foreach_post_remat(irn, tmp) {
			op_t           *remat_op = get_irn_link(tmp);
			const ir_node  *value = remat_op->attr.remat.remat->value;
			op_t           *val_op = get_irn_link(value);

			assert(remat_op->is_remat && !remat_op->attr.remat.pre);

			/* values that are defined by remat2s are not counted */
#ifdef REMAT_WHILE_LIVE
			assert(val_op->attr.live_range.ilp);
			lpp_set_factor_fast(si->lpp, check_post_remat, val_op->attr.live_range.ilp, 0.0);
#else
			if(val_op->attr.live_range.ilp != ILP_UNDEF) {
				lpp_set_factor_fast(si->lpp, check_post_remat, val_op->attr.live_range.ilp, 0.0);
			}
#endif /* REMAT_WHILE_LIVE */
		}
#endif /* CHECK_POST_REMAT */


		/* new live ranges for values from L\U defined by remat2s or used by remats */
		pset_foreach(live, tmp) {
			ir_node     *value = tmp;//remat_op->attr.remat.remat->value;
			op_t        *value_op = get_irn_link(value);

			if(!set_find_keyval(args, value)) {
				ilp_var_t    prev_lr = ILP_UNDEF;
				ir_node     *remat;
				cst = ILP_UNDEF;

				foreach_post_remat(irn, remat) {
					op_t        *remat_op = get_irn_link(remat);

					/* if value is being rematerialized by this remat */
					if(value == remat_op->attr.remat.remat->value) {
						if(cst == ILP_UNDEF) {
							/* next_live_range <= prev_live_range + sum remat2s */
							ir_snprintf(buf, sizeof(buf), "next_lr_%N_%N", value, irn);
							cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
							ir_snprintf(buf, sizeof(buf), "lr_%N_%N", value, irn);
							prev_lr = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);
							lpp_set_factor_fast(si->lpp, cst, value_op->attr.live_range.ilp, 1.0);
							lpp_set_factor_fast(si->lpp, cst, prev_lr, -1.0);
						}

						lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
					}
				}

#ifdef MAY_DIE_AT_PRE_REMAT
				if(cst == ILP_UNDEF) {
					foreach_pre_remat(si, irn, remat) {
						int          i,
									 n;

						for (i = 0, n = get_irn_arity(remat); i < n; ++i) {
							ir_node        *remat_arg = get_irn_n(remat, i);

							/* if value is being used by this remat */
							if(value == remat_arg) {
								/* next_live_range <= prev_live_range */
								ir_snprintf(buf, sizeof(buf), "lr_%N_%N", value, irn);
								prev_lr = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

								ir_snprintf(buf, sizeof(buf), "next_lr_%N_%N", value, irn);
								cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
								lpp_set_factor_fast(si->lpp, cst, value_op->attr.live_range.ilp, 1.0);
								lpp_set_factor_fast(si->lpp, cst, prev_lr, -1.0);
								goto fertig;
							}
							/* TODO check afterwards whether lr dies after a pre-remat (should not happen) */
						}
					}
				}
fertig:
#endif

				if(prev_lr != ILP_UNDEF) {
					value_op->attr.live_range.ilp = prev_lr;
					value_op->attr.live_range.op = irn;
				}
			}
		}

		/* get count of values in my register class defined by irn */
		/* also add defined values to check_post_remat; do this before iterating over args */
		if(get_irn_mode(irn) == mode_T) {
			ir_node  *proj = sched_next(irn);
			op_t     *proj_op = get_irn_link(proj);

			while(is_Proj(proj)) {
				if(has_reg_class(si, proj)) {
					++d;
#ifdef CHECK_POST_REMAT
					lpp_set_factor_fast(si->lpp, check_post_remat, proj_op->attr.live_range.ilp, 1.0);
#endif
				}
				proj = sched_next(proj);
				proj_op = get_irn_link(proj);
			}
		} else {
			if(has_reg_class(si, irn)) {
				 d = 1;
#ifdef CHECK_POST_REMAT
				 lpp_set_factor_fast(si->lpp, check_post_remat, op->attr.live_range.ilp, 1.0);
#endif
			}
		}
		DBG((si->dbg, LEVEL_4, "\t   %+F produces %d values in my register class\n", irn, d));

		/* count how many regs irn needs for arguments */
		k = set_count(args);

		/* check the register pressure in the prolog */
		/* sum_{L\U} lr <= n - |U| */
		ir_snprintf(buf, sizeof(buf), "check_pre_%N", irn);
		check_pre = lpp_add_cst(si->lpp, buf, lpp_less, si->n_regs - k);

		/* check the register pressure in the epilog */
		ir_snprintf(buf, sizeof(buf), "check_post_%N", irn);
		check_post = lpp_add_cst(si->lpp, buf, lpp_less, si->n_regs - d);

		set_foreach(args, keyval) {
			ilp_var_t       next_lr;
			op_t           *arg_op;
			ilp_var_t       post_use;
			int             p = 0;
			spill_t        *spill;
			ir_node        *arg = keyval->key;

			spill = add_to_spill_bb(si, bb, arg);

			ir_snprintf(buf, sizeof(buf), "lr_%N_%N", arg, irn);
			next_lr = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

			i = (int)keyval->val;
			assert(i<n);

			ir_snprintf(buf, sizeof(buf), "reload_%N_%N", arg, irn);
			op->attr.live_range.reloads[i] = lpp_add_var(si->lpp, buf, lpp_binary, COST_LOAD*execution_frequency(si, bb));

			/* reload <= mem_out */
			cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
			lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.reloads[i], 1.0);
			lpp_set_factor_fast(si->lpp, cst, spill->mem_out, -1.0);

			arg_op = get_irn_link(arg);

			/* requirement: arg must be in register for use */
			/* reload + remat + live_range == 1 */
			ir_snprintf(buf, sizeof(buf), "req_%N_%N", irn, arg);
			cst = lpp_add_cst(si->lpp, buf, lpp_equal, 1.0);

			lpp_set_factor_fast(si->lpp, cst, next_lr, 1.0);
			lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.reloads[i], 1.0);
			foreach_pre_remat(si, irn, tmp) {
				op_t     *remat_op = get_irn_link(tmp);
				if(remat_op->attr.remat.remat->value == arg) {
					lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
				}
			}

			/* the epilog stuff - including post_use, post, post_remat */
			ir_snprintf(buf, sizeof(buf), "post_use_%N_%N", arg, irn);
			post_use = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

			lpp_set_factor_fast(si->lpp, check_post, post_use, 1.0);

			/* arg is live throughout epilog if the next live_range is in a register */
			if(pset_find_ptr(live, arg)) {
				DBG((si->dbg, LEVEL_3, "\t  arg %+F is possibly live in epilog of %+F\n", arg, irn));

				ir_snprintf(buf, sizeof(buf), "post_use_%N_%N-%d", arg, irn, p++);
				cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
				lpp_set_factor_fast(si->lpp, cst, post_use, -1.0);
				lpp_set_factor_fast(si->lpp, cst, arg_op->attr.live_range.ilp, 1.0);

#ifdef CHECK_POST_REMAT
				lpp_set_factor_fast(si->lpp, check_post_remat, arg_op->attr.live_range.ilp, 1.0);
#endif
			}

			/*forall remat2 which use arg add a similar cst*/
			foreach_post_remat(irn, tmp) {
				int         i,
							n;

				for (i = 0, n = get_irn_arity(tmp); i < n; ++i) {
					ir_node    *remat_arg = get_irn_n(tmp, i);
					op_t       *remat_op = get_irn_link(tmp);

					if(remat_arg == arg) {
						DBG((si->dbg, LEVEL_3, "\t  found remat with arg %+F in epilog of %+F\n", arg, irn));

						ir_snprintf(buf, sizeof(buf), "post_use_%N_%N-%d", arg, irn, p++);
						cst = lpp_add_cst(si->lpp, buf, lpp_greater, 0.0);
						lpp_set_factor_fast(si->lpp, cst, post_use, 1.0);
						lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
					}
				}
			}

			/* new live range begins for each argument */
			arg_op->attr.live_range.ilp = next_lr;
			arg_op->attr.live_range.op = irn;

			pset_insert_ptr(live, arg);
		}

		/* start new live ranges for values used by remats */
		foreach_pre_remat(si, irn, tmp) {
			int          i,
						 n;

			for (i = 0, n = get_irn_arity(tmp); i < n; ++i) {
				ir_node        *remat_arg = get_irn_n(tmp, i);
				op_t           *arg_op = get_irn_link(remat_arg);
				ilp_var_t       prev_lr;

				if(!has_reg_class(si, remat_arg)) continue;

				/* if value is becoming live through use by remat */
				if(!pset_find_ptr(live, remat_arg)) {
					ir_snprintf(buf, sizeof(buf), "lr_%N_%N", remat_arg, irn);
					prev_lr = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

					arg_op->attr.live_range.ilp = prev_lr;
					arg_op->attr.live_range.op = irn;

					DBG((si->dbg, LEVEL_4, "  value %+F becoming live through use by remat %+F\n", remat_arg, tmp));

					/* TODO ist das hier die richtige Stelle???? */
					pset_insert_ptr(live, remat_arg);
					add_to_spill_bb(si, bb, remat_arg);
				}
				/* TODO check afterwards whether lr dies after a pre-remat (should not happen) */
			}
		}

		/* iterate over L\U */
		pset_foreach(live, tmp) {
			if(!set_find_keyval(args, tmp)) {
				/* if a live value is not used by irn */
				tmp_op = get_irn_link(tmp);
//				assert(tmp_op->attr.live_range.op != irn);
				lpp_set_factor_fast(si->lpp, check_pre, tmp_op->attr.live_range.ilp, 1.0);
				lpp_set_factor_fast(si->lpp, check_post, tmp_op->attr.live_range.ilp, 1.0);
			}
		}

		/* requirements for remats */
		foreach_pre_remat(si, irn, tmp) {
			op_t        *remat_op = get_irn_link(tmp);
			int          i,
						 n;

			for (i = 0, n = get_irn_arity(tmp); i < n; ++i) {
				ir_node        *remat_arg = get_irn_n(tmp, i);
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

					lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.reloads[index], -1.0);
				}
			}
		}

		/* requirements for remats2
		 *
		 *  TODO unsure if this does the right thing.
		 *  should insert values into set if they do not become live through remat and
		 *  op
		 */
		foreach_post_remat(irn, tmp) {
			op_t        *remat_op = get_irn_link(tmp);
			int          i,
						 n;

			for (i = 0, n = get_irn_arity(tmp); i < n; ++i) {
				ir_node        *remat_arg = get_irn_n(tmp, i);
				op_t           *arg_op = get_irn_link(remat_arg);

				if(!has_reg_class(si, remat_arg)) continue;

				/* only for values in L\U, the others are handled with post_use */
				if(!set_find_keyval(args, remat_arg)) {
					/* remat <= live_rang(remat_arg) */
					ir_snprintf(buf, sizeof(buf), "req_remat2_%N_arg_%N", tmp, remat_arg);
					cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

					/* if value is becoming live through use by remat2 */
					if(!pset_find_ptr(live, remat_arg)) {
						ilp_var_t     lr;

						ir_snprintf(buf, sizeof(buf), "lr_%N_%N", remat_arg, irn);
						lr = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

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

#ifdef CHECK_POST_REMAT
		/* iterate over following remats and add them to check_post_remat */
		foreach_post_remat(irn, tmp) {
			op_t           *remat_op = get_irn_link(tmp);

			assert(remat_op->is_remat && !remat_op->attr.remat.pre);

			lpp_set_factor_fast(si->lpp, check_post_remat, remat_op->attr.remat.ilp, 1.0);
		}
#endif



		DBG((si->dbg, LEVEL_4, "\t   %d values live at %+F\n", pset_count(live), irn));

		pset_foreach(live, tmp) {
			assert(has_reg_class(si, tmp));
		}

		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node        *arg = get_irn_n(irn, i);

			assert(!find_post_remat(arg, irn) && "there should be no post remat for an argument of an op");
		}

		del_set(args);
	}



	/* do something at the beginning of the block */

	/* we are now at the beginning of the basic block, there are only \Phis in front of us */
	DBG((si->dbg, LEVEL_3, "\t   %d values live at beginning of block %+F\n", pset_count(live), bb));

	pset_foreach(live, irn) {
		assert(is_Phi(irn) || get_nodes_block(irn) != bb);
	}

	/* construct mem_outs for all values */

	set_foreach(spill_bb->ilp, spill) {
		ir_snprintf(buf, sizeof(buf), "mem_out_%N_%N", spill->irn, bb);
		cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

		lpp_set_factor_fast(si->lpp, cst, spill->mem_out, 1.0);
		if(spill->spill != ILP_UNDEF) {
			lpp_set_factor_fast(si->lpp, cst, spill->spill, -1.0);
		}

		if(pset_find_ptr(live, spill->irn)) {
			DBG((si->dbg, LEVEL_5, "\t     %+F live at beginning of block %+F\n", spill->irn, bb));

			ir_snprintf(buf, sizeof(buf), "mem_in_%N_%N", spill->irn, bb);
			spill->mem_in = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

			lpp_set_factor_fast(si->lpp, cst, spill->mem_in, -1.0);
		}
	}


	/* L\U is empty at bb start */
	/* arg is live throughout epilog if it is reg_in into this block */

	/* check the register pressure at the beginning of the block
	 * including remats
	 */
	ir_snprintf(buf, sizeof(buf), "check_start_%N", bb);
	cst = lpp_add_cst(si->lpp, buf, lpp_less, si->n_regs);

	pset_foreach(live, irn) {
			spill = set_find_spill(spill_bb->ilp, irn);
			assert(spill);

			ir_snprintf(buf, sizeof(buf), "reg_in_%N_%N", irn, bb);
			spill->reg_in = lpp_add_var(si->lpp, buf, lpp_binary, 0.0);

			lpp_set_factor_fast(si->lpp, cst, spill->reg_in, 1.0);
	}
	foreach_post_remat(bb, irn) {
		op_t     *remat_op = get_irn_link(irn);

		DBG((si->dbg, LEVEL_4, "\t  next post remat: %+F\n", irn));
		assert(remat_op->is_remat && !remat_op->attr.remat.pre);

		lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
	}

	/* forall remat2 add requirements */
	foreach_post_remat(bb, tmp) {
		int         i,
					n;

		for (i = 0, n = get_irn_arity(tmp); i < n; ++i) {
			ir_node    *remat_arg = get_irn_n(tmp, i);
			op_t       *remat_op = get_irn_link(tmp);

			if(!has_reg_class(si, remat_arg)) continue;

			spill = set_find_spill(spill_bb->ilp, remat_arg);
			assert(spill);

			/* TODO verify this is placed correctly */
			ir_snprintf(buf, sizeof(buf), "req_remat2_%N_%N_arg_%N", tmp, bb, remat_arg);
			cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
			lpp_set_factor_fast(si->lpp, cst, spill->reg_in, -1.0);
			lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, 1.0);
		}
	}

	/* mem_in/reg_in for live_in values, especially phis and their arguments */
	pset_foreach(live, irn) {
		int          p = 0,
					 i,
					 n;

		spill = set_find_spill(spill_bb->ilp, irn);
		assert(spill && spill->irn == irn);

		if(is_Phi(irn) && get_nodes_block(irn) == bb) {
			for (i = 0, n = get_Phi_n_preds(irn); i < n; ++i) {
				ilp_cst_t       mem_in,
								reg_in;
				ir_node        *phi_arg = get_Phi_pred(irn, i);
				ir_node        *bb_p = get_Block_cfgpred_block(bb, i);
				spill_bb_t     *spill_bb_p = get_irn_link(bb_p);
				spill_t        *spill_p;

				/* although the phi is in the right regclass one or more of
				 * its arguments can be in a different one or at least to
				 * ignore
				 */
				if(has_reg_class(si, phi_arg)) {
					ir_snprintf(buf, sizeof(buf), "mem_in_%N_%N-%d", irn, bb, p);
					mem_in = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
					ir_snprintf(buf, sizeof(buf), "reg_in_%N_%N-%d", irn, bb, p++);
					reg_in = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

					lpp_set_factor_fast(si->lpp, mem_in, spill->mem_in, 1.0);
					lpp_set_factor_fast(si->lpp, reg_in, spill->reg_in, 1.0);

					spill_p = set_find_spill(spill_bb_p->ilp, phi_arg);
					assert(spill_p);

					lpp_set_factor_fast(si->lpp, mem_in, spill_p->mem_out, -1.0);
					lpp_set_factor_fast(si->lpp, reg_in, spill_p->reg_out, -1.0);
				}
			}
		} else {
			/* else assure the value arrives on all paths in the same resource */

			for (i = 0, n = get_Block_n_cfgpreds(bb); i < n; ++i) {
				ilp_cst_t       mem_in,
								reg_in;
				ir_node        *bb_p = get_Block_cfgpred_block(bb, i);
				spill_bb_t     *spill_bb_p = get_irn_link(bb_p);
				spill_t        *spill_p;

				ir_snprintf(buf, sizeof(buf), "mem_in_%N_%N-%d", irn, bb, p);
				mem_in = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
				ir_snprintf(buf, sizeof(buf), "reg_in_%N_%N-%d", irn, bb, p++);
				reg_in = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

				lpp_set_factor_fast(si->lpp, mem_in, spill->mem_in, 1.0);
				lpp_set_factor_fast(si->lpp, reg_in, spill->reg_in, 1.0);

				spill_p = set_find_spill(spill_bb_p->ilp, irn);
				assert(spill_p);

				lpp_set_factor_fast(si->lpp, mem_in, spill_p->mem_out, -1.0);
				lpp_set_factor_fast(si->lpp, reg_in, spill_p->reg_out, -1.0);
			}
		}
	}

	/* first live ranges from reg_ins */
	pset_foreach(live, irn) {
		op_t      *op = get_irn_link(irn);

		spill = set_find_spill(spill_bb->ilp, irn);
		assert(spill && spill->irn == irn);

		ir_snprintf(buf, sizeof(buf), "first_lr_%N_%N", irn, bb);
		cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);
		lpp_set_factor_fast(si->lpp, cst, op->attr.live_range.ilp, 1.0);
		lpp_set_factor_fast(si->lpp, cst, spill->reg_in, -1.0);

		foreach_post_remat(bb, tmp) {
			op_t     *remat_op = get_irn_link(tmp);

			if(remat_op->attr.remat.remat->value == irn) {
				lpp_set_factor_fast(si->lpp, cst, remat_op->attr.remat.ilp, -1.0);
			}
		}
	}

	/* walk forward now and compute constraints for placing spills */
	/* this must only be done for values that are not defined in this block */
	if(is_diverge_edge(bb)) {
		pset_foreach(live, irn) {
			ir_snprintf(buf, sizeof(buf), "req_spill_%N_%N", irn, bb);
			cst = lpp_add_cst(si->lpp, buf, lpp_less, 0.0);

			spill = set_find_spill(spill_bb->ilp, irn);
			assert(spill);

			lpp_set_factor_fast(si->lpp, cst, spill->spill, 1.0);
			lpp_set_factor_fast(si->lpp, cst, spill->reg_in, -1.0);

			sched_foreach_op(bb, tmp) {
				op_t   *op = get_irn_link(tmp);

				if(is_Phi(tmp)) continue;
				assert(!is_Proj(tmp));

				if(op->is_remat) {
					ir_node   *value = op->attr.remat.remat->value;

					if(value == irn) {
						/* only collect remats up to the first use of a value */
						lpp_set_factor_fast(si->lpp, cst, op->attr.remat.ilp, -1.0);
					}
				} else {
					int i,
						n;

					for (i = 0, n = get_irn_arity(tmp); i < n; ++i) {
						ir_node    *arg = get_irn_n(tmp, i);

						if(arg == irn) {
							/* if a value is used stop collecting remats */
							cst = ILP_UNDEF;
						}
						break;
					}
				}
				if(cst == ILP_UNDEF) break;
			}
		}
	}


	/* if a value is used by a mem-phi, then mem_in of this value is 0 (has to be spilled again into a different slot)
	   mem_in(phi) -> not mem_in(orig_value) TODO: how does this depend on a certain predecessor?
	 */

	/* mem_in of mem-phi has associated costs (but first one is free) */
	/* define n_mem_copies as positive integer in each predecessor block,
	   #mem_in into this block from predecessor block - 1 weighted with SPILL_COST*execfreq(predecessor)
	   TODO
	 */


	del_pset(live);
}


#if 0
	 * Speicherkopienminimierung: teste Speicherwerte auf Interferenz
	 * und weise Spillkontexte zu. Sorge bei Phis dafuer, dass gleiche
	 * Kontexte zusammenfliessen (Operanden und Ergebnis hat gleichen
	 * Kontext)
#endif

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

static int
is_mem_phi(const ir_node * phi, void *data)
{
	spill_ilp_t    *si = data;
//	return is_spilled(si, get_use_head(si, phi)->closest_use);
	return 0;
}

#ifdef KEEPALIVE
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
#endif

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
	DUMP_NODE_EDGE_FUNC old = get_dump_node_edge_hook();

	dump_consts_local(0);
	set_dump_node_edge_hook(sched_pressure_edge_hook);
	dump_ir_block_graph(irg, suffix);
	set_dump_node_edge_hook(old);
}

static void
walker_pressure_annotator(ir_node * bb, void * data)
{
	spill_ilp_t  *si = data;
	ir_node      *irn;
	irn_live_t   *li;
	int           i,
				  n;
	pset         *live = pset_new_ptr_default();
	int           projs = 0;

	live_foreach(bb, li) {
		irn = (ir_node *) li->irn;

		if (live_is_end(li) && has_reg_class(si, irn)) {
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

		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node    *arg = get_irn_n(irn, i);

			if(has_reg_class(si, arg)) pset_insert_ptr(live, arg);
		}
		set_irn_link(irn, INT_TO_PTR(pset_count(live)+projs));
	}

	del_pset(live);
}

static void
dump_pressure_graph(spill_ilp_t * si, const char *suffix)
{
	be_dump(si->chordal_env->irg, suffix, dump_ir_block_graph_sched_pressure);
}

#ifdef KEEPALIVE
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

		si->keep = be_new_Keep(si->chordal_env->cls, si->chordal_env->irg, get_irg_end_block(si->chordal_env->irg), n_remats, ins);

		obstack_free(si->obst, ins);
	}
}
#endif

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

		keep = be_new_Keep(si->chordal_env->cls, si->chordal_env->irg, get_irg_end_block(si->chordal_env->irg), n_spills, ins);

		obstack_free(si->obst, ins);
	}
}

/** insert a spill at an arbitrary position */
ir_node *be_spill2(const arch_env_t *arch_env, ir_node *irn, ir_node *insert, ir_node *ctx)
{
	ir_node *bl     = is_Block(insert)?insert:get_nodes_block(insert);
	ir_graph *irg   = get_irn_irg(bl);
	ir_node *frame  = get_irg_frame(irg);
	ir_node *spill;
	ir_node *next;

	const arch_register_class_t *cls       = arch_get_irn_reg_class(arch_env, irn, -1);
	const arch_register_class_t *cls_frame = arch_get_irn_reg_class(arch_env, frame, -1);

	spill = be_new_Spill(cls, cls_frame, irg, bl, frame, irn, ctx);

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

	if(bl == get_irg_start_block(irg) && sched_get_time_step(frame) >= sched_get_time_step(insert))
		insert = frame;

	for (next = sched_next(insert); is_Phi(next) || is_Proj(next); next = sched_next(insert))
		insert = next;

	sched_add_after(insert, spill);
	return spill;
}

static void
delete_remat(spill_ilp_t * si, ir_node * remat) {
	int       i,
		      n;
	ir_node  *bad = get_irg_bad(si->chordal_env->irg);

	sched_remove(remat);

	/* kill links to operands */
	for (i = -1, n = get_irn_arity(remat); i < n; ++i) {
		set_irn_n(remat, i, bad);
	}
}

static void
clean_remat_info(spill_ilp_t * si)
{
	int            i,
			       n;
	remat_t       *remat;
	remat_info_t  *remat_info;
	ir_node       *bad = get_irg_bad(si->chordal_env->irg);

	set_foreach(si->remat_info, remat_info) {
		if(!remat_info->remats) continue;

		pset_foreach(remat_info->remats, remat)
		{
			if(remat->proj && get_irn_n_edges(remat->proj) == 0) {
				set_irn_n(remat->proj, -1, bad);
				set_irn_n(remat->proj, 0, bad);
			}

			if(get_irn_n_edges(remat->op) == 0) {
				for (i = -1, n = get_irn_arity(remat->op); i < n; ++i) {
					set_irn_n(remat->op, i, bad);
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
#ifdef KEEPALIVE
	int       i,
		      n;
	ir_node  *bad = get_irg_bad(si->chordal_env->irg);

	if(si->keep) {
		ir_node   *end = get_irg_end(si->chordal_env->irg);
		ir_node  **keeps;

		for (i = 0, n = get_irn_arity(si->keep); i < n; ++i) {
			ir_node        *keep_arg = get_irn_n(si->keep, i);
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

			set_irn_n(si->keep, i, bad);
		}
#if 0
		for (i = 0, n = get_End_n_keepalives(end); i < n; ++i) {
			ir_node        *end_arg = get_End_keepalive(end, i);

			if(end_arg != si->keep) {
				obstack_grow(si->obst, &end_arg, sizeof(end_arg));
			}
		}
		keeps = obstack_finish(si->obst);
		set_End_keepalives(end, n-1, keeps);
		obstack_free(si->obst, keeps);
#endif
	} else {
		DBG((si->dbg, LEVEL_2, "\t  no remats to delete (none have been inserted)\n"));
	}
#else
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
#endif
}

/**
 * @param before   The node after which the spill will be placed in the schedule
 */
/* TODO set context properly */
static ir_node *
insert_spill(spill_ilp_t * si, const ir_node * irn, const ir_node * value, const ir_node * before)
{
	defs_t   *defs;
	ir_node  *spill;
	const arch_env_t *arch_env = si->chordal_env->birg->main_env->arch_env;

	DBG((si->dbg, LEVEL_3, "\t  inserting spill for value %+F after %+F\n", irn, before));

	spill = be_spill2(arch_env, irn, before, irn);

	defs = set_insert_def(si->values, value);
	assert(defs);

	/* enter into the linked list */
	set_irn_link(spill, defs->spills);
	defs->spills = spill;

#ifdef KEEPALIVE_SPILLS
	pset_insert_ptr(si->spills, spill);
#endif

	return spill;
}

/**
 * @param before   The Phi node which has to be spilled
 */
static ir_node *
insert_mem_phi(spill_ilp_t * si, const ir_node * phi)
{
	ir_node   *mem_phi;
	ir_node  **ins;
	defs_t    *defs;
	int        i,
			   n;

	NEW_ARR_A(ir_node*, ins, get_irn_arity(phi));

	for(i=0,n=get_irn_arity(phi); i<n; ++i) {
		ins[i] = si->m_unknown;
	}

	mem_phi =  new_r_Phi(si->chordal_env->irg, get_nodes_block(phi), get_irn_arity(phi), ins, mode_M);

	defs = set_insert_def(si->values, phi);
	assert(defs);

	/* enter into the linked list */
	set_irn_link(mem_phi, defs->spills);
	defs->spills = mem_phi;

#ifdef KEEPALIVE_SPILLS
	pset_insert_ptr(si->spills, mem_phi);
#endif

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

#if 0
static void
collect_spills(spill_ilp_t * si, ir_node * value, pset * spills, pset * visited)
{
	ir_node  *next;
	defs_t   *defs;

	defs = set_find_def(si->values, value);

	if(defs && defs->spills) {
		for(next = defs->spills; next; next = get_irn_link(next)) {
			pset_insert_ptr(spills, next);
		}
	} else if (is_Phi(value)) {
		/* recursion */
		if(!pset_find_ptr(visited, value)) {
			int    i,
				   n;

			pset_insert_ptr(visited, value);
			for(i=0, n=get_irn_arity(value); i<n; ++i) {
				ir_node    *arg = get_irn_n(value, i);

				collect_spills(si, arg, spills, visited);
			}
		}
	} else {
//		assert(0 && "Phi operand not spilled");
	}
}
#endif

static pset *
get_spills_for_value(spill_ilp_t * si, ir_node * value)
{
	pset     *spills = pset_new_ptr_default();
//	pset     *visited = pset_new_ptr_default();

//	collect_spills(si, value, spills, visited);
//	del_pset(visited);
	ir_node  *next;
	defs_t   *defs;

	defs = set_find_def(si->values, value);

	if(defs && defs->spills) {
		for(next = defs->spills; next; next = get_irn_link(next)) {
			pset_insert_ptr(spills, next);
		}
	}

	return spills;
}

/**
 * Add reload before operation and add to list of defs
 */
static ir_node *
insert_reload(spill_ilp_t * si, const ir_node * value, const ir_node * after)
{
	defs_t   *defs;
	ir_node  *reload,
			 *spill;
	const arch_env_t *arch_env = si->chordal_env->birg->main_env->arch_env;

	DBG((si->dbg, LEVEL_3, "\t  inserting reload for value %+F before %+F\n", value, after));

	defs = set_find_def(si->values, value);
	/* get a spill of this value */
#if 0
	if((!defs || !defs->spills) && is_Phi(value)) {
		pset  *spills;

		spills = get_spills_for_value(si, value);

		spill = pset_first(spills);
		del_pset(spills);

		if(!defs) {
			defs = set_insert_def(si->values, value);
		}
		defs->spills = spill;
		set_irn_link(spill, NULL);
	} else {
		spill = defs->spills;
	}
#endif
	spill = defs->spills;
	assert(spill && "no spill placed before reload");

	reload = be_reload(arch_env, si->cls, after, get_irn_mode(value), spill);

	/* enter into the linked list */
	set_irn_link(reload, defs->remats);
	defs->remats = reload;

	return reload;
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

		if(spill->spill != ILP_UNDEF) {
			name = si->lpp->vars[spill->spill];
			if(!is_zero(name->value)) {
				if(spill->reg_in > 0) {
					name = si->lpp->vars[spill->reg_in];
					if(!is_zero(name->value)) {
						insert_spill(si, spill->irn, spill->irn, bb);
						continue;
					}
				}
				pset_insert_ptr(spills_to_do, spill->irn);
			}
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

static void
phim_fixer(spill_ilp_t *si) {
	defs_t  *defs;

	set_foreach(si->values, defs) {
		const ir_node  *phi = defs->value;
		ir_node  *phi_m = defs->spills;
		int       i,
				  n;

		if(!is_Phi(phi)) continue;
		if(!phi_m || !is_Phi(phi_m) || get_irn_mode(phi_m) != mode_M) continue;

		for(i=0,n=get_irn_arity(phi); i<n; ++i) {
			const ir_node  *value = get_irn_n(phi, i);
			defs_t         *val_defs = set_find_def(si->values, value);

			/* get a spill of this value */
			ir_node      *spill = val_defs->spills;

			assert(spill && "no spill placed before PhiM");

			set_irn_n(phi_m, i, spill);
		}
	}
}

static void
walker_reload_placer(ir_node * bb, void * data) {
	spill_ilp_t   *si = (spill_ilp_t*)data;
	ir_node       *irn;
	spill_bb_t    *spill_bb = get_irn_link(bb);
	int            i;
	irn_live_t    *li;

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

			for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node    *arg = get_irn_n(irn, i);

				if(op->attr.live_range.reloads && op->attr.live_range.reloads[i] != ILP_UNDEF) {
					lpp_name_t    *name;

					name = si->lpp->vars[op->attr.live_range.reloads[i]];
					if(!is_zero(name->value)) {
						ir_node    *reload;
						ir_node    *insert_pos = irn;
						ir_node    *prev = sched_prev(insert_pos);
						op_t       *prev_op = get_irn_link(prev);

						/* insert reload before pre-remats */
						while(!sched_is_end(prev) && !be_is_Reload(prev) && !be_is_Spill(prev)
								&& prev_op->is_remat && prev_op->attr.remat.pre) {
							insert_pos = prev;

							prev = sched_prev(insert_pos);
							prev_op = get_irn_link(prev);
						}

						reload = insert_reload(si, arg, insert_pos);

						set_irn_n(irn, i, reload);

#ifdef KEEPALIVE_RELOADS
						pset_insert_ptr(si->spills, reload);
#endif
					}
				}
			}
		}
	}

	/* reloads at end of block */
	if(spill_bb->reloads) {
		i=0;
		live_foreach(bb, li) {
			ir_node        *irn = (ir_node *) li->irn;

			if (live_is_end(li) && has_reg_class(si, irn) && !pset_find_ptr(si->all_possible_remats, irn)) {
				lpp_name_t    *name;

				name = si->lpp->vars[spill_bb->reloads[i]];
				if(!is_zero(name->value)) {
					ir_node    *reload;
					ir_node    *insert_pos = bb;
					ir_node    *prev = sched_prev(insert_pos);
					op_t       *prev_op = get_irn_link(prev);

					/* insert reload before pre-remats */
					while(!sched_is_end(prev) && !be_is_Reload(prev) && !be_is_Spill(prev)
							&& prev_op->is_remat && prev_op->attr.remat.pre) {
						insert_pos = prev;

						prev = sched_prev(insert_pos);
						prev_op = get_irn_link(prev);
					}

					reload = insert_reload(si, irn, insert_pos);

#ifdef KEEPALIVE_RELOADS
					pset_insert_ptr(si->spills, reload);
#endif
				}
				++i;
			}
		}
	}

	del_set(spill_bb->ilp);
}

static void
walker_collect_used(ir_node * irn, void * data)
{
	lc_bitset_t   *used = data;

	lc_bitset_set(used, get_irn_idx(irn));
}

static void
walker_kill_unused(ir_node * bb, void * data)
{
	lc_bitset_t     *used = data;
	const ir_node   *bad = get_irg_bad(get_irn_irg(bb));
	ir_node         *irn;


	for(irn=sched_first(bb); !sched_is_end(irn);) {
		ir_node     *next = sched_next(irn);
		int          i,
					 n;

		if(!lc_bitset_is_set(used, get_irn_idx(irn))) {
			assert(!be_is_Spill(irn) && !be_is_Reload(irn) && "something is fishy, spill or remat is unused");

			sched_remove(irn);

			set_nodes_block(irn, bad);
			for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
				set_irn_n(irn, i, bad);
			}
		}
		irn = next;
	}
}

static void
kill_all_unused_values_in_schedule(spill_ilp_t * si)
{
	lc_bitset_t   *used = lc_bitset_malloc(get_irg_last_idx(si->chordal_env->irg));

	irg_walk_graph(si->chordal_env->irg, walker_collect_used, NULL, used);
	irg_block_walk_graph(si->chordal_env->irg, walker_kill_unused, NULL, used);

	lc_bitset_free(used);
}

static void
print_irn_pset(pset * p)
{
	ir_node   *irn;

	pset_foreach(p, irn) {
		ir_printf("%+F\n", irn);
	}
}

static void
rewire_uses(spill_ilp_t * si)
{
	dom_front_info_t     *dfi = be_compute_dominance_frontiers(si->chordal_env->irg);
	defs_t               *defs;

	/* then fix uses of spills */
	set_foreach(si->values, defs) {
		pset     *reloads;
		pset     *spills;
		ir_node  *next = defs->remats;
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
			//assert(pset_count(reloads) > 0);
			//				print_irn_pset(spills);
			//				print_irn_pset(reloads);

			be_ssa_constr_set(dfi, spills);
		}

		del_pset(reloads);
		del_pset(spills);
	}

	/* first fix uses of remats and reloads */
	set_foreach(si->values, defs) {
		pset     *nodes;
		ir_node  *next = defs->remats;

		if(next) {
			nodes = pset_new_ptr_default();
			pset_insert_ptr(nodes, defs->value);

			while(next) {
				pset_insert_ptr(nodes, next);
				next = get_irn_link(next);
			}

			if(pset_count(nodes) > 1) {
				DBG((si->dbg, LEVEL_4, "\t    %d new definitions for value %+F\n", pset_count(nodes)-1, defs->value));
				be_ssa_constr_set(dfi, nodes);
			}

			del_pset(nodes);
		}
	}

//	remove_unused_defs(si);

	be_free_dominance_frontiers(dfi);
}

static void
writeback_results(spill_ilp_t * si)
{
	/* walk through the graph and collect all spills, reloads and remats for a value */

	si->values = new_set(cmp_defs, 4096);

	DBG((si->dbg, LEVEL_1, "Applying results\n"));
	delete_unnecessary_remats(si);
	si->m_unknown = new_r_Unknown(si->chordal_env->irg, mode_M);
	irg_block_walk_graph(si->chordal_env->irg, walker_spill_placer, NULL, si);
	phim_fixer(si);
	irg_block_walk_graph(si->chordal_env->irg, walker_reload_placer, NULL, si);

	/* clean the remat info! there are still back-edges leading there! */
	clean_remat_info(si);

	rewire_uses(si);

	connect_all_spills_with_keep(si);

	del_set(si->values);
}

static int
get_n_regs(spill_ilp_t * si)
{
	int     arch_n_regs = arch_register_class_n_regs(si->cls);
	int     free = 0;
	int     i;

	for(i=0; i<arch_n_regs; i++) {
		if(!arch_register_type_is(&si->cls->regs[i], ignore)) {
			free++;
		}
	}

	DBG((si->dbg, LEVEL_1, "\tArchitecture has %d free registers in class %s\n", free, si->cls->name));
	return free;
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
					if(sched_is_end(irn) || (be_is_Reload(irn) && has_reg_class(si, irn))) break;

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
	irg_block_walk_graph(si->chordal_env->irg, walker_reload_mover, NULL, si);
}

void
be_spill_remat(const be_chordal_env_t * chordal_env)
{
	char            problem_name[256];
	char            dump_suffix[256];
	char            dump_suffix2[256];
	char            dump_suffix3[256];
	struct obstack  obst;
	spill_ilp_t     si;

	ir_snprintf(problem_name, sizeof(problem_name), "%F_%s", chordal_env->irg, chordal_env->cls->name);
	ir_snprintf(dump_suffix, sizeof(dump_suffix), "-%s-remats", chordal_env->cls->name);
	ir_snprintf(dump_suffix2, sizeof(dump_suffix2), "-%s-pressure", chordal_env->cls->name);
	ir_snprintf(dump_suffix3, sizeof(dump_suffix3), "-%s-reloads_moved", chordal_env->cls->name);

	FIRM_DBG_REGISTER(si.dbg, "firm.be.ra.spillremat");
	DBG((si.dbg, LEVEL_1, "\n\n\t\t===== Processing %s =====\n\n", problem_name));

	obstack_init(&obst);
	si.chordal_env = chordal_env;
	si.obst = &obst;
	si.senv = be_new_spill_env(chordal_env, is_mem_phi, &si);
	si.cls = chordal_env->cls;
	si.lpp = new_lpp(problem_name, lpp_minimize);
	si.remat_info = new_set(cmp_remat_info, 4096);
	si.all_possible_remats = pset_new_ptr_default();
	si.spills = pset_new_ptr_default();
	si.inverse_ops = pset_new_ptr_default();
#ifndef EXECFREQ_LOOPDEPH
	si.execfreqs = compute_execfreq(chordal_env->irg);
#else
	si.execfreqs = NULL;
#endif
#ifdef KEEPALIVE
	si.keep = NULL;
#endif
	si.n_regs = get_n_regs(&si);

	set_irg_link(chordal_env->irg, &si);
	compute_doms(chordal_env->irg);

#ifdef COLLECT_REMATS
	/* collect remats */
	DBG((si.dbg, LEVEL_1, "Collecting remats\n"));
	irg_walk_graph(chordal_env->irg, walker_remat_collector, NULL, &si);
#endif

	/* insert possible remats */
	DBG((si.dbg, LEVEL_1, "Inserting possible remats\n"));
	irg_block_walk_graph(chordal_env->irg, walker_remat_insertor, NULL, &si);
	DBG((si.dbg, LEVEL_2, " -> inserted %d possible remats\n", pset_count(si.all_possible_remats)));

#ifdef KEEPALIVE
	DBG((si.dbg, LEVEL_1, "Connecting remats with keep and dumping\n"));
	connect_all_remats_with_keep(&si);
	/* dump graph with inserted remats */
	dump_graph_with_remats(chordal_env->irg, dump_suffix);
#endif


	/* recompute liveness */
	DBG((si.dbg, LEVEL_1, "Recomputing liveness\n"));
	be_liveness(chordal_env->irg);

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

		ir_snprintf(buf, sizeof(buf), "%s-spillremat.ilp", problem_name);
		if ((f = fopen(buf, "wt")) != NULL) {
			lpp_dump_plain(si.lpp, f);
			fclose(f);
		}
	}
#endif

#ifdef SOLVE
	DBG((si.dbg, LEVEL_1, "\tSolving %F\n", chordal_env->irg));
	lpp_set_time_limit(si.lpp, ILP_TIMEOUT);

#ifdef SOLVE_LOCAL
	lpp_solve_cplex(si.lpp);
#else
	lpp_solve_net(si.lpp, LPP_SERVER, LPP_SOLVER);
#endif
	assert(lpp_is_sol_valid(si.lpp)
	       && "solution of ILP must be valid");

	DBG((si.dbg, LEVEL_1, "\t%s: iterations: %d, solution time: %g, objective function: %g\n", problem_name, si.lpp->iterations, si.lpp->sol_time, is_zero(si.lpp->objval)?0.0:si.lpp->objval));

#ifdef DUMP_SOLUTION
	{
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
#endif

	writeback_results(&si);

#endif				/* SOLVE */

	kill_all_unused_values_in_schedule(&si);

#if defined(KEEPALIVE_SPILLS) || defined(KEEPALIVE_RELOADS)
	be_dump(chordal_env->irg, "-spills-placed", dump_ir_block_graph);
#endif

	be_liveness(chordal_env->irg);
	irg_block_walk_graph(chordal_env->irg, walker_pressure_annotator, NULL, &si);

	dump_pressure_graph(&si, dump_suffix2);

	// TODO fix temporarily exceeded regpressure due to remat2s

	// TODO insert copys to fix interferences in memory

	// move reloads upwards
	move_reloads_upward(&si);
	irg_block_walk_graph(chordal_env->irg, walker_pressure_annotator, NULL, &si);
	dump_pressure_graph(&si, dump_suffix3);

	free_dom(chordal_env->irg);
	del_pset(si.inverse_ops);
	del_pset(si.all_possible_remats);
	del_pset(si.spills);
#ifndef EXECFREQ_LOOPDEPH
	free_execfreq(si.execfreqs);
#endif
	free_lpp(si.lpp);
	obstack_free(&obst, NULL);
//	exit(0);
}

#else				/* WITH_ILP */

static void
only_that_you_can_compile_without_WITH_ILP_defined(void)
{
}

#endif				/* WITH_ILP */
