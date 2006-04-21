
/**
 * More experiments on coalescing.
 * @author Sebastian Hack
 * @date   14.04.2006
 */

#include <stdlib.h>
#include <limits.h>

#include "list.h"
#include "bitset.h"
#include "debug.h"

#include "irphase_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irprintf.h"

#include "beabi.h"
#include "benode_t.h"
#include "becopyopt.h"
#include "becopyopt_t.h"
#include "bechordal_t.h"

typedef unsigned col_t;

typedef struct _co2_irn_t co2_irn_t;

typedef struct {
	phase_t ph;
	copy_opt_t *co;
	bitset_t *ignore_regs;
	co2_irn_t *touched;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
} co2_t;

struct _co2_irn_t {
	ir_node       *irn;
	co2_irn_t     *touched_next;
	int            costs;
	col_t          tmp_col;
	col_t          orig_col;
	unsigned       fixed     : 1;
	unsigned       tmp_fixed : 1;
	struct list_head changed_list;
};

#define get_co2_irn(co2, irn)   ((co2_irn_t *) phase_get_or_set_irn_data(&co2->ph, irn))

static void co2_irn_init(phase_t *ph, ir_node *irn, void *data)
{
	co2_t *env    = (co2_t *) ph;
	co2_irn_t *ci = data;

	memset(ci, 0, sizeof(ci[0]));
	INIT_LIST_HEAD(&ci->changed_list);
	ci->irn          = irn;
	ci->touched_next = env->touched;
	ci->orig_col     = get_irn_col(env->co, irn);
	env->touched     = ci;
}


static int co2_irn_cmp(const void *a, const void *b)
{
	const co2_irn_t **p = a;
	const co2_irn_t **q = b;
	return (*q)->costs - (*p)->costs;
}

typedef struct {
	col_t col;
	int costs;
} col_cost_pair_t;

/**
 * An order on color/costs pairs.
 * If the costs are equal, we use the color as a kind of normalization.
 */
static int col_cost_pair_lt(const void *a, const void *b)
{
	const col_cost_pair_t *p = a;
	const col_cost_pair_t *q = b;
	int cost_diff = p->costs - q->costs;

	return cost_diff;
	// return cost_diff != 0 ? cost_diff : p->col - q->col;
}

static col_t get_col(co2_t *env, ir_node *irn)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	return ci->tmp_fixed ? ci->tmp_col : ci->orig_col;
}

static INLINE color_is_fix(co2_t *env, ir_node *irn)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	return ci->fixed || ci->tmp_fixed;
}

static void incur_constraint_costs(co2_t *env, ir_node *irn, col_cost_pair_t *col_costs, int costs)
{
	bitset_t *aux = bitset_alloca(env->co->cls->n_regs);
	arch_register_req_t req;

	arch_get_register_req(env->co->aenv, &req, irn, BE_OUT_POS(0));

	if(arch_register_req_is(&req, limited)) {
		bitset_pos_t elm;
		int n_constr;

		req.limited(req.limited_env, aux);
		n_constr = bitset_popcnt(aux);
		bitset_foreach(aux, elm)
			col_costs[elm].costs += costs / n_constr;
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
static void determine_color_costs(co2_t *env, ir_node *irn, col_cost_pair_t *col_costs)
{
	be_ifg_t *ifg      = env->co->cenv->ifg;
	int n_regs         = env->co->cls->n_regs;
	bitset_t *forb     = bitset_alloca(n_regs);
	affinity_node_t *a = get_affinity_info(env->co, irn);

	arch_register_req_t req;
	bitset_pos_t elm;
	ir_node *pos;
	void *it;
	int i;

	/* Put all forbidden colors into the aux bitset. */
	arch_get_register_req(env->co->aenv, &req, irn, BE_OUT_POS(0));
	if(arch_register_req_is(&req, limited)) {
		req.limited(req.limited_env, forb);
		bitset_flip_all(forb);
	}
	else
		bitset_copy(forb, env->ignore_regs);

	for(i = 0; i < n_regs; ++i) {
		col_costs[i].col   = i;
		col_costs[i].costs = 0;
	}

	if(a) {
		neighb_t *n;

		co_gs_foreach_neighb(a, n) {
			co2_irn_t *ni = get_co2_irn(env, n->irn);

			if(ni->fixed) {
				col_t col = get_col(env, n->irn);
				col_costs[col].costs -= 100 * n->costs;
			}

			incur_constraint_costs(env, n->irn, col_costs, -n->costs);
		}
	}

	it = be_ifg_neighbours_iter_alloca(ifg);
	be_ifg_foreach_neighbour(ifg, it, irn, pos) {
		col_t col = get_col(env, pos);
		if(color_is_fix(env, pos))
			col_costs[col].costs = INT_MAX;
		else {
			incur_constraint_costs(env, pos, col_costs, INT_MAX);
			col_costs[col].costs++;
		}
	}

	/* Set the costs to infinity for each color which is not allowed at this node. */
	bitset_foreach(forb, elm)
		col_costs[elm].costs = INT_MAX;

}

static int curr_costs(co2_t *env, affinity_node_t *a)
{
	col_t a_col = get_col(env, a->irn);
	int costs   = 0;
	neighb_t *n;

	co_gs_foreach_neighb(a, n) {
		col_t n_col = get_col(env, n->irn);
		costs += n_col != a_col ? n->costs : 0;
	}

	return costs;
}

static void reject_coloring(struct list_head *h)
{
	co2_irn_t *pos;

	list_for_each_entry(co2_irn_t, pos, h, changed_list)
		pos->tmp_fixed = 0;
}

static void materialize_coloring(co2_t *env, struct list_head *h)
{
	const arch_register_class_t *cls = env->co->cls;
	const arch_env_t *aenv           = env->co->aenv;
	co2_irn_t *pos;

	list_for_each_entry(co2_irn_t, pos, h, changed_list) {
		pos->orig_col = pos->tmp_col;
		pos->tmp_fixed = 0;
	}
}

static int change_color_not(co2_t *env, ir_node *irn, col_t not_col, struct list_head *parent_changed, int depth);


static INLINE int recolor(co2_t *env, ir_node *irn, col_cost_pair_t *col_list, int n_regs, struct list_head *parent_changed, int depth)
{
	be_ifg_t *ifg = env->co->cenv->ifg;
	co2_irn_t *ci = get_co2_irn(env, irn);
	int res       = 0;

	int i;

	for(i = 0; i < n_regs; ++i) {
		col_t tgt_col  = col_list[i].col;
		unsigned costs = col_list[i].costs;
		int neigh_ok   = 1;

		struct list_head changed;
		ir_node *n;
		void *it;

		DBG((env->dbg, LEVEL_2, "\t%2Dtrying color %d(%d) on %+F\n", depth, tgt_col, costs, irn));

		/* If the costs for that color (and all successive) are infinite, bail out we won't make it anyway. */
		if(costs == INT_MAX) {
			ci->tmp_fixed = 0;
			return 0;
		}

		/* Set the new color of the node and mark the node as temporarily fixed. */
		ci->tmp_col   = tgt_col;
		ci->tmp_fixed = 1;

		/*
		If that color has costs > 0, there's at least one neighbor having that color,
		so we will try to change the neighbors' colors, too.
		*/
		INIT_LIST_HEAD(&changed);
		list_add(&ci->changed_list, &changed);

		it = be_ifg_neighbours_iter_alloca(ifg);
		be_ifg_foreach_neighbour(ifg, it, irn, n) {

			/* try to re-color the neighbor if it has the target color. */
			if(get_col(env, n) == tgt_col) {
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
				if(!neigh_ok)
					break;
			}
		}


		/*
		We managed to assign the target color to all neighbors, so from the perspective
		of the current node, every thing was ok and we can return safely.
		*/
		if(neigh_ok) {
			DBG((env->dbg, LEVEL_2, "\t%2Dcolor %d(%d) was ok\n", depth, tgt_col, costs));
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

static int change_color_not(co2_t *env, ir_node *irn, col_t not_col, struct list_head *parent_changed, int depth)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	int res       = 0;
	col_t col     = get_col(env, irn);

	DBG((env->dbg, LEVEL_2, "\t%2Dclearing %+F(%d) of color %d\n", depth, irn, col, not_col));

	/* the node does not have to forbidden color. That's fine, mark it as visited and return. */
	if(col != not_col) {
		if(!ci->tmp_fixed) {
			ci->tmp_col   = col;
			ci->tmp_fixed = 1;
		}

		list_add(&ci->changed_list, parent_changed);
		return 1;
	}

	/* The node has the color it should not have _and_ has not been visited yet. */
	if(!color_is_fix(env, irn)) {
		int n_regs            = env->co->cls->n_regs;
		col_cost_pair_t *csts = alloca(n_regs * sizeof(csts[0]));

		/* Get the costs for giving the node a specific color. */
		determine_color_costs(env, irn, csts);

		/* Since the node must not have the not_col, set the costs for that color to "infinity" */
		csts[not_col].costs = INT_MAX;

		/* sort the colors according costs, cheapest first. */
		qsort(csts, n_regs, sizeof(csts[0]), col_cost_pair_lt);

		/* Try recoloring the node using the color list. */
		res = recolor(env, irn, csts, n_regs, parent_changed, depth);
	}

	/* If we came here, everything went ok. */
	return res;
}

/**
 * Try to bring a node to a certain color.
 */
static int try_color(co2_t *env, ir_node *irn, col_t tgt_col, struct list_head *changed)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	be_ifg_t *ifg = env->co->cenv->ifg;

	ir_node *n;
	void *it;

	ci->tmp_fixed = 1;
	ci->tmp_col   = tgt_col;
	list_add(&ci->changed_list, changed);

	it = be_ifg_neighbours_iter_alloca(ifg);
	be_ifg_foreach_neighbour(ifg, it, irn, n) {
		col_t c = get_col(env, n);

		/* If the neighbor has the target color, re-color the neighbor. */
		if(c == tgt_col) {
			int ok = change_color_not(env, n, tgt_col, changed, 1);
			if(!ok)
				return 0;
		}
	}

	return 1;
}

static INLINE int costs_sufficient(co2_irn_t *irn, int costs)
{
	return costs == -irn->costs;
}

static void process_affinity_node(co2_t *env, co2_irn_t *ci)
{
	int n_regs               = env->co->cls->n_regs;
	col_cost_pair_t *col_seq = alloca(n_regs * sizeof(col_seq[0]));
	affinity_node_t *a       = get_affinity_info(env->co, ci->irn);
	int best_cost            = curr_costs(env, a);
	col_t best_col           = ci->orig_col;

	neighb_t *n;
	int i;

	assert(a != NULL && "This node must be an affinity node");

	/* If that node has already been fixed, leave it alone. */
	if(ci->fixed)
		return;

	DB((env->dbg, LEVEL_1, "affinity node %+F cost %d\n", ci->irn, ci->costs));

	/* determine the order in which the colors shall be tried. */
	determine_color_costs(env, ci->irn, col_seq);
	qsort(col_seq, n_regs, sizeof(col_seq[0]), col_cost_pair_lt);

	/* Try the colors. */
	for(i = 0; i < n_regs; ++i) {
		col_t col = col_seq[i].col;
		int costs = col_seq[i].costs;

		struct list_head changed;
		int ok;

		DB((env->dbg, LEVEL_2, "\tbest color %d incurring costs %d\n", best_col, best_cost));

		/* Also, if the costs are not more optimizable, we do not try additional colors and finish this node. */
		if(best_cost == 0)
			break;

		if(costs == INT_MAX) {
			DB((env->dbg, LEVEL_1, "\tall following colors after %d will be infeasible\n", col));
			break;
		}

		INIT_LIST_HEAD(&changed);

		DB((env->dbg, LEVEL_1, "\ttrying color %d costing %d\n", col, costs));

		/* try to assign the same color to the node and all its neighbors. */
		ok = try_color(env, a->irn, col, &changed);

		if(!ok) {
			DBG((env->dbg, LEVEL_2, "\t-> failed.\n"));
			reject_coloring(&changed);
			continue;
		}

		/*
		Evaluate the recoloring and mark it is as new best if it was better
		as the best current known solution.
		*/
		costs = curr_costs(env, a);
		DBG((env->dbg, LEVEL_2, "\t-> cost: %d\n", costs));

		if(costs < best_cost) {
			best_cost = costs;
			best_col  = col;

			materialize_coloring(env, &changed);
		}

		/* If we had a better coloring already, reject the current one. */
		else
			reject_coloring(&changed);

	}

	/* We found the definite color for this node, so fix it. */
	ci->fixed = 1;

	DB((env->dbg, LEVEL_1, "\tusing %d(%d)\n", best_col, best_cost));

	/* Now, investigate all affinity neighbors of this node. */
	if(0) {
		co2_irn_t **neighbors = alloca(a->degree * sizeof(neighbors[0]));

		i = 0;
		co_gs_foreach_neighb(a, n)
			neighbors[i++] = get_co2_irn(env, n->irn);

		qsort(neighbors, a->degree, sizeof(neighbors[0]), co2_irn_cmp);
		for(i = 0; i < a->degree; ++i)
			process_affinity_node(env, neighbors[i]);
	}
}

static void process(co2_t *env)
{
	struct obstack obst;
	affinity_node_t *an;
	co2_irn_t **nodes;
	int i, n;

	obstack_init(&obst);

	n = 0;
	co_gs_foreach_aff_node(env->co, an) {
		ir_node *irn   = an->irn;
		co2_irn_t *ci  = get_co2_irn(env, irn);

		neighb_t *neighb;

		co_gs_foreach_neighb(an, neighb)
			ci->costs += neighb->costs;

		obstack_ptr_grow(&obst, ci);
		n++;
	}

	nodes = obstack_finish(&obst);

	/* sort the nodes according to processing order. */
	qsort(nodes, n, sizeof(nodes[0]), co2_irn_cmp);

	for(i = 0; i < n; ++i) {
		if(!nodes[i]->fixed)
			process_affinity_node(env, nodes[i]);
	}

	obstack_free(&obst, NULL);
}

static void writeback_colors(co2_t *env)
{
	const arch_env_t *aenv = env->co->aenv;
	co2_irn_t *irn;

	for(irn = env->touched; irn; irn = irn->touched_next) {
		const arch_register_t *reg = arch_register_for_index(env->co->cls, irn->orig_col);
		arch_set_irn_register(aenv, irn->irn, reg);
	}
}

void co_solve_heuristic_new(copy_opt_t *co)
{
	co2_t env;

	phase_init(&env.ph, "co2", co->cenv->birg->irg, sizeof(co2_irn_t), PHASE_DEFAULT_GROWTH, co2_irn_init);
	env.touched     = NULL;
	env.co          = co;
	env.ignore_regs = bitset_alloca(co->cls->n_regs);
	arch_put_non_ignore_regs(co->aenv, co->cls, env.ignore_regs);
	bitset_flip_all(env.ignore_regs);
	be_abi_put_ignore_regs(co->cenv->birg->abi, co->cls, env.ignore_regs);
	FIRM_DBG_REGISTER(env.dbg, "firm.be.co2");

	process(&env);
	writeback_colors(&env);
	phase_free(&env.ph);
}
