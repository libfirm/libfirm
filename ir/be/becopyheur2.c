/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       More experiments on coalescing.
 * @author      Sebastian Hack
 * @date        14.04.2006
 */
#include "lc_opts.h"
#include "lc_opts_enum.h"

#include <stdlib.h>
#include <limits.h>

#include "belive.h"
#include "beirg.h"
#include "list.h"
#include "pdeq.h"
#include "raw_bitset.h"

#include "debug.h"
#include "bitfiddle.h"

#include "irgraph_t.h"
#include "irnode_t.h"
#include "util.h"
#include "irtools.h"
#include "irnodemap.h"
#include "be_t.h"
#include "bemodule.h"
#include "beabi.h"
#include "benode.h"
#include "becopyopt.h"
#include "becopyopt_t.h"
#include "bechordal_t.h"

#define DUMP_BEFORE 1
#define DUMP_AFTER  2
#define DUMP_CLOUD  4
#define DUMP_ALL    2 * DUMP_CLOUD - 1

static unsigned dump_flags      = 0;
static int      subtree_iter    = 4;
static int      max_depth       = 20;
static double   constr_factor   = 0.9;

static const lc_opt_enum_mask_items_t dump_items[] = {
	{ "before",  DUMP_BEFORE },
	{ "after",   DUMP_AFTER  },
	{ "cloud",   DUMP_CLOUD  },
	{ "all",     DUMP_ALL    },
	{ NULL,      0 }
};

static lc_opt_enum_mask_var_t dump_var = {
	&dump_flags, dump_items
};

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_ENUM_MASK("dump", "dump ifg cloud",                                         &dump_var),
	LC_OPT_ENT_INT      ("iter", "iterations for subtree nodes",                           &subtree_iter),
	LC_OPT_ENT_DBL      ("cf",   "factor of constraint importance (between 0.0 and 1.0)",  &constr_factor),
	LC_OPT_ENT_INT      ("max",  "maximum recursion depth",                                &max_depth),
	LC_OPT_LAST
};

/*
  ____  _             _
 / ___|| |_ __ _ _ __| |_
 \___ \| __/ _` | '__| __|
  ___) | || (_| | |  | |_
 |____/ \__\__,_|_|   \__|

*/

#define INFEASIBLE(cost) ((cost) == INT_MAX)

typedef unsigned col_t;

typedef struct co2_irn_t       co2_irn_t;
typedef struct co2_cloud_t     co2_cloud_t;
typedef struct co2_cloud_irn_t co2_cloud_irn_t;

typedef struct {
	col_t col;
	int costs;
} col_cost_pair_t;

typedef struct {
	ir_nodemap       map;
	struct obstack   obst;
	copy_opt_t      *co;
	unsigned const  *allocatable_regs;
	co2_irn_t       *touched;
	int              visited;
	int              n_regs;
	struct list_head cloud_head;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
} co2_t;

struct co2_irn_t {
	const ir_node   *irn;
	affinity_node_t *aff;
	co2_irn_t       *touched_next;
	col_t            tmp_col;
	col_t            orig_col;
	unsigned const  *admissible;
	unsigned         fixed          : 1;
	unsigned         tmp_fixed      : 1;
	struct list_head changed_list;
};

struct co2_cloud_irn_t {
	struct co2_irn_t   inh;
	co2_cloud_t       *cloud;
	int                visited;
	int                index;
	co2_cloud_irn_t   *mst_parent;
	int                mst_costs;
	int                mst_n_childs;
	co2_cloud_irn_t  **mst_childs;
	int               *col_costs;
	int                costs;
	int               *fronts;
	int               *color_badness;
	col_cost_pair_t   *tmp_coloring;
	struct list_head   cloud_list;
	struct list_head   mst_list;
};

struct co2_cloud_t {
	co2_t            *env;
	struct obstack    obst;
	int               costs;
	int               mst_costs;
	int               inevit;
	int               best_costs;
	int               n_memb;
	int               ticks;
	double            freedom;
	co2_cloud_irn_t  *master;
	co2_cloud_irn_t  *mst_root;
	co2_cloud_irn_t **seq;
	struct list_head  members_head;
	struct list_head  list;
};

typedef struct {
	co2_cloud_irn_t *src, *tgt;
	int costs;
} edge_t;

#define FRONT_BASE(ci,col)  ((ci)->fronts + col * (ci)->mst_n_childs)

static co2_irn_t *get_co2_irn(co2_t *env, const ir_node *node)
{
	co2_irn_t *ci = ir_nodemap_get(co2_irn_t, &env->map, node);
	if (ci == NULL) {
		ci = OALLOCZ(&env->obst, co2_irn_t);

		INIT_LIST_HEAD(&ci->changed_list);
		ci->touched_next = env->touched;
		ci->orig_col     = get_irn_col(node);
		env->touched     = ci;
		ci->irn          = node;
		ci->aff          = NULL;

		arch_register_req_t const *const req = arch_get_irn_register_req(node);
		ci->admissible = arch_register_req_is(req, limited) ? req->limited : env->allocatable_regs;

		ir_nodemap_insert(&env->map, node, ci);
	}
	return ci;
}

static co2_cloud_irn_t *get_co2_cloud_irn(co2_t *env, const ir_node *node)
{
	co2_cloud_irn_t *ci = ir_nodemap_get(co2_cloud_irn_t, &env->map, node);
	if (ci == NULL) {
		ci = OALLOCZ(&env->obst, co2_cloud_irn_t);

		INIT_LIST_HEAD(&ci->inh.changed_list);
		ci->inh.touched_next = env->touched;
		ci->inh.orig_col     = get_irn_col(node);
		env->touched         = &ci->inh;
		ci->inh.irn          = node;
		ci->inh.aff          = get_affinity_info(env->co, node);

		INIT_LIST_HEAD(&ci->cloud_list);
		ci->mst_parent = ci;

		ir_nodemap_insert(&env->map, node, ci);
	}
	return ci;
}

#define CLOUD_WEIGHT(c) ((1 - constr_factor) * (c)->costs + constr_factor * (c)->freedom)

static int cmp_clouds_gt(const void *a, const void *b)
{
	const co2_cloud_t * const *p = (const co2_cloud_t*const*)a;
	const co2_cloud_t * const *q = (const co2_cloud_t*const*)b;
	double c = CLOUD_WEIGHT(*p);
	double d = CLOUD_WEIGHT(*q);
	return QSORT_CMP(d, c);
}

/**
 * An order on color/costs pairs.
 * If the costs are equal, we use the color as a kind of normalization.
 */
static int col_cost_pair_lt(const void *a, const void *b)
{
	const col_cost_pair_t *p = (const col_cost_pair_t*)a;
	const col_cost_pair_t *q = (const col_cost_pair_t*)b;
	int c = p->costs;
	int d = q->costs;
	return QSORT_CMP(c, d);
}

static int cmp_edges(const void *a, const void *b)
{
	const edge_t *p = (const edge_t*)a;
	const edge_t *q = (const edge_t*)b;
	return QSORT_CMP(q->costs, p->costs);
}

static col_t get_col(co2_t *env, const ir_node *irn)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	return ci->tmp_fixed ? ci->tmp_col : ci->orig_col;
}

static inline int color_is_fix(co2_t *env, const ir_node *irn)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	return ci->fixed || ci->tmp_fixed;
}

static inline int is_color_admissible(co2_irn_t *const ci, col_t const col)
{
	unsigned const *const bs = ci->admissible;
	return rbitset_is_set(bs, col);
}

static void incur_constraint_costs(ir_node const *const irn, col_cost_pair_t *const col_costs, int const costs)
{
	const arch_register_req_t *req = arch_get_irn_register_req(irn);

	if (arch_register_req_is(req, limited)) {
		unsigned const n_regs   = req->cls->n_regs;
		unsigned const n_constr = rbitset_popcount(req->limited, n_regs);
		for (unsigned i = 0; i < n_regs; ++i) {
			if (rbitset_is_set(req->limited, i)) {
				col_costs[i].costs = add_saturated(col_costs[i].costs, costs / n_constr);
			}
		}
	}
}

/**
 * Determine costs which shall indicate how cheap/expensive it is to try
 * to assign a node some color.
 * The costs are computed for all colors. INT_MAX means that it is impossible
 * to give the node that specific color.
 *
 * @param env       The co2 this pointer.
 * @param irn       The node.
 * @param col_costs An array of colors x costs where the costs are written to.
 */
static void determine_color_costs(co2_t *env, co2_irn_t *ci, col_cost_pair_t *col_costs)
{
	const ir_node *irn = ci->irn;
	be_ifg_t *ifg      = env->co->cenv->ifg;
	int n_regs         = env->co->cls->n_regs;
	affinity_node_t *a = ci->aff;

	neighbours_iter_t it;
	int i;

	for (i = 0; i < n_regs; ++i) {
		col_costs[i].col   = i;
		col_costs[i].costs = 0;
	}

	if (a) {
		co_gs_foreach_neighb(a, n) {
			if (color_is_fix(env, n->irn)) {
				col_t col = get_col(env, n->irn);
				col_costs[col].costs = add_saturated(col_costs[col].costs, -n->costs * 128);
			}

			incur_constraint_costs(n->irn, col_costs, -n->costs);
		}
	}

	be_ifg_foreach_neighbour(ifg, &it, irn, pos) {
		col_t col = get_col(env, pos);
		if (color_is_fix(env, pos)) {
			col_costs[col].costs  = INT_MAX;
		} else {
			incur_constraint_costs(pos, col_costs, INT_MAX);
			col_costs[col].costs = add_saturated(col_costs[col].costs, 8 * be_ifg_degree(ifg, pos));
		}
	}

	/* Set the costs to infinity for each color which is not allowed at this node. */
	unsigned const *const admissible = ci->admissible;
	rbitset_foreach_clear(admissible, n_regs, elm) {
		col_costs[elm].costs  = INT_MAX;
	}
}

static void single_color_cost(co2_t *env, co2_irn_t *ci, col_t col, col_cost_pair_t *seq)
{
	int n_regs = env->co->cls->n_regs;
	int i;

	for (i = 0; i < n_regs; ++i) {
		seq[i].col   = i;
		seq[i].costs = INT_MAX;
	}

	(void) ci;
	assert(is_color_admissible(ci, col));
	seq[col].col = 0;
	seq[0].col   = col;
	seq[0].costs = 0;
}

static void reject_coloring(struct list_head *h)
{
	list_for_each_entry(co2_irn_t, pos, h, changed_list)
		pos->tmp_fixed = 0;
}

static void materialize_coloring(struct list_head *h)
{
	list_for_each_entry(co2_irn_t, pos, h, changed_list) {
		pos->orig_col  = pos->tmp_col;
		pos->tmp_fixed = 0;
	}
}

static int change_color_not(co2_t *env, const ir_node *irn, col_t not_col, struct list_head *parent_changed, int depth);

static int recolor(co2_t *env, const ir_node *irn, col_cost_pair_t *col_list, struct list_head *parent_changed, int depth)
{
	int n_regs         = env->co->cls->n_regs;
	be_ifg_t *ifg      = env->co->cenv->ifg;
	co2_irn_t *ci      = get_co2_irn(env, irn);
	int res            = 0;

	int i;

	if (depth >= max_depth)
	  return 0;

	for (i = 0; i < n_regs; ++i) {
		col_t tgt_col  = col_list[i].col;
		unsigned costs = col_list[i].costs;
		int neigh_ok   = 1;

		struct list_head changed;
		neighbours_iter_t it;

		DBG((env->dbg, LEVEL_3, "\t\t%2{firm:indent}trying color %d(%d) on %+F\n", depth, tgt_col, costs, irn));

		/* If the costs for that color (and all successive) are infinite, bail out we won't make it anyway. */
		if (INFEASIBLE(costs)) {
			DB((env->dbg, LEVEL_4, "\t\t%2{firm:indent}color %d infeasible\n", depth, tgt_col));
			ci->tmp_fixed = 0;
			return 0;
		}

		/* Set the new color of the node and mark the node as temporarily fixed. */
		ci->tmp_col     = tgt_col;
		ci->tmp_fixed   = 1;

		/*
		If that color has costs > 0, there's at least one neighbor having that color,
		so we will try to change the neighbors' colors, too.
		*/
		INIT_LIST_HEAD(&changed);
		list_add(&ci->changed_list, &changed);

		be_ifg_foreach_neighbour(ifg, &it, irn, n) {

			/* try to re-color the neighbor if it has the target color. */
			if (get_col(env, n) == tgt_col) {
				struct list_head tmp;

				/*
				Try to change the color of the neighbor and record all nodes which
				get changed in the tmp list. Add this list to the "changed" list for
				that color. If we did not succeed to change the color of the neighbor,
				we bail out and try the next color.
				*/
				INIT_LIST_HEAD(&tmp);
				neigh_ok = change_color_not(env, n, tgt_col, &tmp, depth + 1);
				list_splice(&tmp, &changed);
				if (!neigh_ok) {
					be_ifg_neighbours_break(&it);
					break;
				}
			}
		}

		/*
		We managed to assign the target color to all neighbors, so from the perspective
		of the current node, every thing was ok and we can return safely.
		*/
		if (neigh_ok) {
			DBG((env->dbg, LEVEL_3, "\t\t%2{firm:indent}color %d(%d) was ok\n", depth, tgt_col, costs));
			list_splice(&changed, parent_changed);
			res = 1;
			break;
		}

		/*
		If not, that color did not succeed and we unfix all nodes we touched
		by traversing the changed list and setting tmp_fixed to 0 for these nodes.
		*/
		else
			reject_coloring(&changed);
	}

	return res;
}

static int change_color_not(co2_t *env, const ir_node *irn, col_t not_col, struct list_head *parent_changed, int depth)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	int res       = 0;
	col_t col     = get_col(env, irn);

	DBG((env->dbg, LEVEL_3, "\t\t%2{firm:indent}clearing %+F(%d) of color %d\n", depth, irn, col, not_col));

	/* the node does not have to forbidden color. That's fine, mark it as visited and return. */
	if (col != not_col) {
		if (!ci->tmp_fixed) {
			ci->tmp_col     = col;
			ci->tmp_fixed   = 1;
		}

		list_add(&ci->changed_list, parent_changed);
		return 1;
	}

	/* The node has the color it should not have _and_ has not been visited yet. */
	if (!color_is_fix(env, irn)) {
		int n_regs            = env->co->cls->n_regs;
		col_cost_pair_t *csts = ALLOCAN(col_cost_pair_t, n_regs);

		/* Get the costs for giving the node a specific color. */
		determine_color_costs(env, ci, csts);

		/* Since the node must not have the not_col, set the costs for that color to "infinity" */
		csts[not_col].costs = INT_MAX;

		/* sort the colors according costs, cheapest first. */
		QSORT(csts, n_regs, col_cost_pair_lt);

		/* Try recoloring the node using the color list. */
		res = recolor(env, irn, csts, parent_changed, depth);
	}

	/* If we came here, everything went ok. */
	return res;
}

static int change_color_single(co2_t *env, const ir_node *irn, col_t tgt_col, struct list_head *parent_changed, int depth)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	col_t col     = get_col(env, irn);
	int res       = 0;

	DBG((env->dbg, LEVEL_3, "\t\t%2{firm:indent}trying to set %+F(%d) to color %d\n", depth, irn, col, tgt_col));

	/* the node has the wanted color. That's fine, mark it as visited and return. */
	if (col == tgt_col) {
		if (!ci->tmp_fixed) {
			ci->tmp_col     = col;
			ci->tmp_fixed   = 1;
			list_add(&ci->changed_list, parent_changed);
		}

		res = 1;
		goto end;
	}

	if (!color_is_fix(env, irn) && is_color_admissible(ci, tgt_col)) {
		int n_regs           = env->co->cls->n_regs;
		col_cost_pair_t *seq = ALLOCAN(col_cost_pair_t, n_regs);

		/* Get the costs for giving the node a specific color. */
		single_color_cost(env, ci, tgt_col, seq);

		/* Try recoloring the node using the color list. */
		res = recolor(env, irn, seq, parent_changed, depth);

	}

end:
	DB((env->dbg, LEVEL_3, "\t\t%2{firm:indent}color %d %s for %+F\n", depth, tgt_col, res ? "was ok" : "failed", irn));
	return res;
}

/**
 * Examine the costs of the current coloring concerning a MST subtree.
 * @param ci  The subtree root.
 * @param col The color of @p ci.
 * @return    The best coloring for that subtree under the assumption that @p ci has color @p col.
 */
static int examine_subtree_coloring(co2_cloud_irn_t *ci, col_t col)
{
	int *front = FRONT_BASE(ci, col);
	int cost   = 0;
	int i;

	for (i = 0; i < ci->mst_n_childs; ++i) {
		co2_cloud_irn_t *chld = ci->mst_childs[i];
		col_t chld_col        = front[i];

		cost += examine_subtree_coloring(chld, chld_col);
		cost += col != chld_col ? chld->mst_costs : 0;
	}

	return cost;
}

/**
 * Determine color badnesses of a node.
 * Badness means that it is unlikely that the node in question can
 * obtain a color. The higher the badness, the more unlikely it is that
 * the node can be assigned that color.
 * @param ci      The node.
 * @param badness An integer array as long as there are registers.
 * @note          The array <code>badness</code> is not cleared.
 */
static void node_color_badness(co2_cloud_irn_t *ci, int *badness)
{
	co2_t *const env    = ci->cloud->env;
	size_t const n_regs = env->n_regs;

	neighbours_iter_t it;

	{
		unsigned const *const bs = ci->inh.admissible;
		rbitset_foreach_clear(bs, n_regs, elm)
			badness[elm] = ci->costs;
	}

	/* Use constrained/fixed interfering neighbors to influence the color badness */
	be_ifg_foreach_neighbour(env->co->cenv->ifg, &it, ci->inh.irn, irn) {
		co2_irn_t *ni = get_co2_irn(env, irn);

		unsigned const *const bs = ni->admissible;
		if (rbitset_popcount(bs, n_regs) == 1) {
			size_t const c = rbitset_next_max(bs, 0, n_regs, true);
			badness[c] += ci->costs;
		} else if (ni->fixed) {
			col_t c = get_col(env, ni->irn);
			badness[c] += ci->costs;
		}
	}
}

/**
 * Determine the badness of a MST subtree.
 * The badness is written into the <code>color_badness</code> array of each node and accumulated in the parents.
 * @see node_color_badness() for a definition of badness.
 * @param ci    The root of the subtree.
 * @param depth Depth for debugging purposes.
 */
static void determine_color_badness(co2_cloud_irn_t *ci, int depth)
{
	co2_t *env     = ci->cloud->env;
	int i, j;

	node_color_badness(ci, ci->color_badness);

	/* Collect the color badness for the whole subtree */
	for (i = 0; i < ci->mst_n_childs; ++i) {
		co2_cloud_irn_t *child = ci->mst_childs[i];
		determine_color_badness(child, depth + 1);

		for (j = 0; j < env->n_regs; ++j)
			ci->color_badness[j] += child->color_badness[j];
	}

	for (j = 0; j < env->n_regs; ++j)
		DBG((env->dbg, LEVEL_2, "%2{firm:indent}%+F col %d badness %d\n", depth, ci->inh.irn, j, ci->color_badness[j]));
}

/**
 * Unfix all nodes in a MST subtree.
 */
static void unfix_subtree(co2_cloud_irn_t *ci)
{
	int i;

	ci->inh.fixed = 0;
	for (i = 0; i < ci->mst_n_childs; ++i)
		unfix_subtree(ci->mst_childs[i]);
}

static int coalesce_top_down(co2_cloud_irn_t *ci, int child_nr, int depth)
{
	co2_t *env           = ci->cloud->env;
	col_cost_pair_t *seq = ALLOCAN(col_cost_pair_t, env->n_regs);
	int is_root          = ci->mst_parent == ci;
	col_t parent_col     = is_root ? (col_t) -1 : get_col(env, ci->mst_parent->inh.irn);
	int min_badness      = INT_MAX;
	int best_col_costs   = INT_MAX;
	int best_col         = -1;
	int n_regs           = env->n_regs;
	int n_iter           = is_root ? MIN(n_regs, subtree_iter) : 1;

	struct list_head changed;
	int ok, i, j;

	for (i = 0; i < n_regs; ++i) {
		int badness = ci->color_badness[i];

		seq[i].col   = i;
		seq[i].costs = is_color_admissible(&ci->inh, i) ? badness : INT_MAX;

		min_badness = MIN(min_badness, badness);
	}

	/* If we are not the root and the parent's color is allowed for this node give it top prio. */
	if (!is_root && is_color_admissible(&ci->inh, parent_col))
		seq[parent_col].costs = min_badness - 1;

	/* Sort the colors. The will be processed in that ordering. */
	QSORT(seq, env->n_regs, col_cost_pair_lt);

	DBG((env->dbg, LEVEL_2, "\t%2{firm:indent}starting top-down coalesce for %+F\n", depth, ci->inh.irn));
	INIT_LIST_HEAD(&changed);
	for (i = 0; i < (best_col < 0 ? n_regs : n_iter); ++i) {
		col_t col    = seq[i].col;
		int add_cost = !is_root && col != parent_col ? ci->mst_costs : 0;

		int subtree_costs, sum_costs;

		DBG((env->dbg, LEVEL_2, "\t%2{firm:indent}%+F trying color %d\n", depth, ci->inh.irn, col));

		unfix_subtree(ci);
		INIT_LIST_HEAD(&changed);
		ok = change_color_single(env, ci->inh.irn, col, &changed, depth);
		if (ok) {
			materialize_coloring(&changed);
			ci->inh.fixed = 1;
		}

		else
			continue;

		for (j = 0; j < ci->mst_n_childs; ++j) {
			co2_cloud_irn_t *child = ci->mst_childs[j];
			ok = coalesce_top_down(child, j, depth + 1) >= 0;
			if (ok)
				child->inh.fixed = 1;
			else
				break;
		}

		/* If the subtree could not be colored, we have to try another color. */
		if (!ok)
			continue;

		subtree_costs      = examine_subtree_coloring(ci, col);
		sum_costs          = subtree_costs + add_cost;
		DBG((env->dbg, LEVEL_2, "\t%2{firm:indent}-> %+F costing %d + %d is ok.\n", depth, ci->inh.irn, subtree_costs, add_cost));

		if (sum_costs < best_col_costs) {
			best_col           = col;
			best_col_costs     = sum_costs;
			ci->col_costs[col] = subtree_costs;
		}

		if (sum_costs == 0)
			break;
	}

	if (!is_root) {
		int *front = FRONT_BASE(ci->mst_parent, parent_col);
		front[child_nr] = best_col;
	}

	return best_col;
}

static void populate_cloud(co2_t *env, co2_cloud_t *cloud, affinity_node_t *a, int curr_costs)
{
	co2_cloud_irn_t *ci = get_co2_cloud_irn(env, a->irn);
	int costs           = 0;

	if (ci->cloud)
		return;

	/* mark the node as visited and add it to the cloud. */
	ci->cloud   = cloud;
	list_add(&ci->cloud_list, &cloud->members_head);

	DB((env->dbg, LEVEL_2, "\t%+F\n", ci->inh.irn));

	/* determine the nodes costs */
	co_gs_foreach_neighb(a, n) {
		costs += n->costs;
		DB((env->dbg, LEVEL_3, "\t\tneigh %+F cost %d\n", n->irn, n->costs));
		if (be_values_interfere(a->irn, n->irn))
			cloud->inevit += n->costs;
	}

	/* add the node's cost to the total costs of the cloud. */
	ci->costs        = costs;
	cloud->costs    += costs;
	cloud->freedom  += rbitset_popcount(ci->inh.admissible, env->n_regs);
	cloud->n_memb   += 1;

	/* If this is the heaviest node in the cloud, set it as the cloud's master. */
	if (costs >= curr_costs) {
		curr_costs    = costs;
		cloud->master = ci;
	}

	/* add all the neighbors of the node to the cloud. */
	co_gs_foreach_neighb(a, n) {
		affinity_node_t *an = get_affinity_info(env->co, n->irn);
		assert(an);
		populate_cloud(env, cloud, an, curr_costs);
	}
}

static co2_cloud_t *new_cloud(co2_t *env, affinity_node_t *a)
{
	co2_cloud_t *cloud = OALLOC(&env->obst, co2_cloud_t);
	int i;

	DBG((env->dbg, LEVEL_2, "new cloud with %+F\n", a->irn));
	memset(cloud, 0, sizeof(cloud[0]));
	INIT_LIST_HEAD(&cloud->members_head);
	INIT_LIST_HEAD(&cloud->list);
	list_add(&cloud->list, &env->cloud_head);
	cloud->best_costs = INT_MAX;
	cloud->env = env;
	env->visited++;
	populate_cloud(env, cloud, a, 0);
	cloud->freedom = (cloud->n_memb * env->n_regs) / cloud->freedom;

	/* Also allocate space for the node sequence and compute that sequence. */
	cloud->seq = OALLOCN(&env->obst, co2_cloud_irn_t*, cloud->n_memb);

	i = 0;
	list_for_each_entry(co2_cloud_irn_t, ci, &cloud->members_head, cloud_list) {
		ci->index       = i;
		cloud->seq[i++] = ci;
	}
	DBG((env->dbg, LEVEL_2, "cloud cost %d, freedom %f\n", cloud->costs, cloud->freedom));

	return cloud;
}

static void apply_coloring(co2_cloud_irn_t *ci, col_t col, int depth)
{
	const ir_node *irn = ci->inh.irn;
	int *front   = FRONT_BASE(ci, col);
	int i;
	struct list_head changed;

	INIT_LIST_HEAD(&changed);

	DBG((ci->cloud->env->dbg, LEVEL_2, "%2{firm:indent}setting %+F to %d\n", depth, irn, col));
	change_color_single(ci->cloud->env, irn, col, &changed, depth);
	materialize_coloring(&changed);

	for (i = 0; i < ci->mst_n_childs; ++i) {
		apply_coloring(ci->mst_childs[i], front[i], depth + 1);
	}
}

static co2_cloud_irn_t *find_mst_root(co2_cloud_irn_t *ci)
{
	while (ci != ci->mst_parent)
		ci = ci->mst_parent;
	return ci;
}


static void process_cloud(co2_cloud_t *cloud)
{
	co2_t *env  = cloud->env;
	int n_regs  = env->n_regs;
	int n_edges = 0;
	int *mst_edges = XMALLOCNZ(int, cloud->n_memb * cloud->n_memb);
	pdeq *q;

	edge_t *edges;
	int i;
	int best_col;

	/* Collect all edges in the cloud on an obstack and sort the increasingly */
	obstack_init(&cloud->obst);
	for (i = 0; i < cloud->n_memb; ++i) {
		co2_cloud_irn_t *ci = cloud->seq[i];

		co_gs_foreach_neighb(ci->inh.aff, n) {
			co2_cloud_irn_t *ni = get_co2_cloud_irn(cloud->env, n->irn);
			if (ci->index < ni->index) {
				edge_t e;
				e.src   = ci;
				e.tgt   = ni;
				e.costs = n->costs;
				obstack_grow(&cloud->obst, &e, sizeof(e));
				n_edges++;
			}
		}
	}
	edges = (edge_t*)obstack_finish(&cloud->obst);
	QSORT(edges, n_edges, cmp_edges);

	/* Compute the maximum spanning tree using Kruskal/Union-Find */
	DBG((env->dbg, LEVEL_2, "computing spanning tree of cloud with master %+F\n", cloud->master->inh.irn));
	for (i = 0; i < n_edges; ++i) {
		edge_t *e        = &edges[i];
		co2_cloud_irn_t *rs = find_mst_root(e->src);
		co2_cloud_irn_t *rt = find_mst_root(e->tgt);

		/* if the union/find roots are different */
		if (rs != rt) {
			int si = e->src->index;
			int ti = e->tgt->index;

			/* unify the sets */
			rs->mst_parent = rt;
			DBG((env->dbg, LEVEL_2, "\tadding edge %+F -- %+F cost %d\n", rs->inh.irn, rt->inh.irn, e->costs));

			/* this edge is in the MST, so set it in the bitset. */
			mst_edges[si * cloud->n_memb + ti] = e->costs;
			mst_edges[ti * cloud->n_memb + si] = e->costs;
		}
	}
	obstack_free(&cloud->obst, edges);

	cloud->master->mst_parent = cloud->master;
	cloud->mst_root = cloud->master;
	q = new_pdeq1(cloud->master);
	while (!pdeq_empty(q)) {
		co2_cloud_irn_t *ci = (co2_cloud_irn_t*)pdeq_getl(q);
		int ofs    = ci->index * cloud->n_memb;
		int end    = ofs + cloud->n_memb;
		int i;

		ci->mst_n_childs = 0;
		for (i = ofs; i < end; ++i) {
			if (mst_edges[i] != 0) {
				int other = i - ofs;
				co2_cloud_irn_t *child = cloud->seq[i - ofs];

				/* put the child to the worklist */
				pdeq_putr(q, child);

				/* make ci the parent of the child and add the child to the children array of the parent */
				child->mst_parent = ci;
				child->mst_costs  = mst_edges[i];
				ci->mst_n_childs++;
				obstack_ptr_grow(&cloud->obst, child);

				mst_edges[other * cloud->n_memb + ci->index] = 0;
				mst_edges[i] = 0;
			}
		}

		obstack_ptr_grow(&cloud->obst, NULL);
		ci->mst_childs = (co2_cloud_irn_t**)obstack_finish(&cloud->obst);
	}
	del_pdeq(q);
	free(mst_edges);


	DBG((env->dbg, LEVEL_3, "mst:\n"));
	for (i = 0; i < cloud->n_memb; ++i) {
		DEBUG_ONLY(co2_cloud_irn_t *ci = cloud->seq[i];)
		DBG((env->dbg, LEVEL_3, "\t%+F -> %+F\n", ci->inh.irn, ci->mst_parent->inh.irn));
	}

	for (i = 0; i < cloud->n_memb; ++i) {
		co2_cloud_irn_t *ci = cloud->seq[i];
		int n_childs = ci->mst_n_childs;
		int j;

		ci->col_costs       = OALLOCNZ(&cloud->obst, int,             n_regs);
		ci->tmp_coloring    = OALLOCNZ(&cloud->obst, col_cost_pair_t, n_regs);
		ci->fronts          = OALLOCNZ(&cloud->obst, int,             n_regs * n_childs);
		ci->color_badness   = OALLOCNZ(&cloud->obst, int,             n_regs);

		for (j = 0; j < env->n_regs; j++)
			ci->col_costs[j] = INT_MAX;
	}

	determine_color_badness(cloud->mst_root, 0);
	best_col = coalesce_top_down(cloud->mst_root, -1, 0);
	unfix_subtree(cloud->mst_root);
	apply_coloring(cloud->mst_root, best_col, 0);

	/* The coloring should represent the one with the best costs. */
	//materialize_coloring(&changed);
	DBG((env->dbg, LEVEL_2, "\tbest coloring for root %+F was %d costing %d\n",
		cloud->mst_root->inh.irn, best_col, examine_subtree_coloring(cloud->mst_root, best_col)));

	/* Fix all nodes in the cloud. */
	for (i = 0; i < cloud->n_memb; ++i)
		cloud->seq[i]->inh.fixed = 1;

	/* Free all space used while optimizing this cloud. */
	obstack_free(&cloud->obst, NULL);
}

#ifdef DEBUG_libfirm
static int cloud_costs(co2_cloud_t *cloud)
{
	int i, costs = 0;

	for (i = 0; i < cloud->n_memb; ++i) {
		co2_irn_t *ci = (co2_irn_t *) cloud->seq[i];
		col_t col = get_col(cloud->env, ci->irn);
		co_gs_foreach_neighb(ci->aff, n) {
			col_t n_col = get_col(cloud->env, n->irn);
			costs += col != n_col ? n->costs : 0;
		}
	}

	return costs / 2;
}
#endif

static void writeback_colors(co2_t *env)
{
	co2_irn_t *irn;

	for (irn = env->touched; irn; irn = irn->touched_next) {
		const arch_register_t *reg = arch_register_for_index(env->co->cls, irn->orig_col);
		arch_set_irn_register((ir_node*)irn->irn, reg);
	}
}

static void process(co2_t *env)
{
	co2_cloud_t **clouds;
	int n_clouds;
	int i;
	DEBUG_ONLY(int init_costs  = 0;)
	DEBUG_ONLY(int all_costs   = 0;)
	DEBUG_ONLY(int final_costs = 0;)

	n_clouds = 0;
	co_gs_foreach_aff_node(env->co, a) {
		co2_cloud_irn_t *ci = get_co2_cloud_irn(env, a->irn);

		if (!ci->cloud) {
			new_cloud(env, a);
			n_clouds++;
		}
	}

	i = 0;
	clouds = XMALLOCN(co2_cloud_t*, n_clouds);
	list_for_each_entry(co2_cloud_t, pos, &env->cloud_head, list)
		clouds[i++] = pos;
	QSORT(clouds, n_clouds, cmp_clouds_gt);

	for (i = 0; i < n_clouds; ++i) {
		DEBUG_ONLY(init_costs += cloud_costs(clouds[i]);)

		/* Process the cloud. */
		process_cloud(clouds[i]);

		DEBUG_ONLY(all_costs   += clouds[i]->costs;)
		DEBUG_ONLY(final_costs += cloud_costs(clouds[i]);)
	}

	DB((env->dbg, LEVEL_1, "all costs: %d, init costs: %d, final costs: %d\n", all_costs, init_costs, final_costs));

	free(clouds);
}

static int co_solve_heuristic_new(copy_opt_t *co)
{
	co2_t env;

	ir_nodemap_init(&env.map, co->irg);
	obstack_init(&env.obst);
	env.touched          = NULL;
	env.visited          = 0;
	env.co               = co;
	env.n_regs           = co->cls->n_regs;
	env.allocatable_regs = co->cenv->allocatable_regs->data;
	FIRM_DBG_REGISTER(env.dbg, "firm.be.co2");
	INIT_LIST_HEAD(&env.cloud_head);

	process(&env);

	writeback_colors(&env);
	obstack_free(&env.obst, NULL);
	ir_nodemap_destroy(&env.map);
	return 0;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copyheur2)
void be_init_copyheur2(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp = lc_opt_get_grp(ra_grp, "chordal");
	lc_opt_entry_t *co2_grp = lc_opt_get_grp(chordal_grp, "co2");

	static co_algo_info copyheur = {
		co_solve_heuristic_new, 0
	};

	lc_opt_add_table(co2_grp, options);
	be_register_copyopt("heur2", &copyheur);
}
