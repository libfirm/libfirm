
/**
 * More experiments on coalescing.
 * @author Sebastian Hack
 * @date   14.04.2006
 */

#include <stdlib.h>
#include <limits.h>

#include "list.h"
#include "pdeq.h"
#include "bitset.h"

#include "debug.h"
#include "bitfiddle.h"

#include "irphase_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irprintf.h"

#include "beabi.h"
#include "benode_t.h"
#include "becopyopt.h"
#include "becopyopt_t.h"
#include "bechordal_t.h"

#define INFEASIBLE(cost) ((cost) == INT_MAX)

static be_ifg_dump_dot_cb_t ifg_dot_cb;

typedef unsigned col_t;

typedef struct _co2_irn_t   co2_irn_t;
typedef struct _co2_cloud_t co2_cloud_t;

typedef struct {
	phase_t ph;
	copy_opt_t *co;
	bitset_t *ignore_regs;
	co2_irn_t *touched;
	int visited;
	struct list_head cloud_head;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
} co2_t;

struct _co2_irn_t {
	ir_node         *irn;
	co2_cloud_t     *cloud;
	co2_irn_t       *touched_next;
	affinity_node_t *aff;
	int              costs;
	col_t            tmp_col;
	col_t            orig_col;
	int              visited;
	int              rank;
	unsigned         fixed     : 1;
	unsigned         tmp_fixed : 1;
	struct list_head changed_list;
	struct list_head cloud_list;
};

struct _co2_cloud_t {
	int costs;
	int inevit;
	int best_costs;
	int n_memb;
	int max_degree;
	co2_irn_t *master;
	co2_irn_t **seq;
	col_t      *best_cols;
	struct list_head members_head;
	struct list_head list;
};

#define NEIGHBOR_FIXED   1
#define NEIGHBOR_CONSTR  2
#define SELF_CONSTR      4
#define DONT_WANT        8

typedef struct {
	col_t col;
	int costs;
	unsigned flags;
} col_cost_pair_t;

#define get_co2_irn(co2, irn)   ((co2_irn_t *) phase_get_or_set_irn_data(&co2->ph, irn))

static void co2_irn_init(phase_t *ph, const ir_node *irn, void *data)
{
	co2_t *env    = (co2_t *) ph;
	co2_irn_t *ci = data;

	memset(ci, 0, sizeof(ci[0]));
	INIT_LIST_HEAD(&ci->changed_list);
	INIT_LIST_HEAD(&ci->cloud_list);
	ci->irn          = irn;
	ci->touched_next = env->touched;
	ci->orig_col     = get_irn_col(env->co, irn);
	ci->aff          = get_affinity_info(env->co, irn);
	env->touched     = ci;
}


static int co2_irn_cmp(const void *a, const void *b)
{
	const co2_irn_t **p = a;
	const co2_irn_t **q = b;
	return (*q)->costs - (*p)->costs;
}

static int cmp_clouds(const void *a, const void *b)
{
	const co2_cloud_t **p = a;
	const co2_cloud_t **q = b;
	return (*q)->costs - (*p)->costs;
}

static co2_cloud_t *new_cloud(co2_t *env)
{
	co2_cloud_t *cloud = phase_alloc(&env->ph, sizeof(cloud[0]));
	memset(cloud, 0, sizeof(cloud[0]));
	INIT_LIST_HEAD(&cloud->members_head);
	INIT_LIST_HEAD(&cloud->list);
	cloud->best_costs = INT_MAX;
	return cloud;
}

/**
 * An order on color/costs pairs.
 * If the costs are equal, we use the color as a kind of normalization.
 */
static int col_cost_pair_lt(const void *a, const void *b)
{
	const col_cost_pair_t *p = a;
	const col_cost_pair_t *q = b;
	int c = p->costs;
	int d = q->costs;

	return (c > d) - (c < d);
}

const char *flag_str(unsigned int fl)
{
	static char buf[10];

	buf[0] = fl & NEIGHBOR_CONSTR ? 'c' : '-';
	buf[1] = fl & NEIGHBOR_FIXED  ? 'n' : '-';
	buf[2] = fl & SELF_CONSTR     ? 'C' : '-';
	buf[3] = fl & DONT_WANT       ? 'd' : '-';
	buf[4] = '\0';
	return buf;
}

static col_t get_col(co2_t *env, ir_node *irn)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	return ci->tmp_fixed ? ci->tmp_col : ci->orig_col;
}

static INLINE int color_is_fix(co2_t *env, ir_node *irn)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	return ci->fixed || ci->tmp_fixed;
}

static bitset_t *admissible_colors(co2_t *env, co2_irn_t *ci, bitset_t *bs)
{
	arch_register_req_t req;

	arch_get_register_req(env->co->aenv, &req, ci->irn, BE_OUT_POS(0));
	if(arch_register_req_is(&req, limited))
		req.limited(req.limited_env, bs);
	else {
		bitset_copy(bs, env->ignore_regs);
		bitset_flip_all(bs);
	}

	return bs;
}

static int is_color_admissible(co2_t *env, co2_irn_t *ci, col_t col)
{
	bitset_t *bs = bitset_alloca(env->co->cls->n_regs);
	admissible_colors(env, ci, bs);
	return bitset_is_set(bs, col);
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
		bitset_foreach(aux, elm) {
			col_costs[elm].costs  = add_saturated(col_costs[elm].costs, costs / n_constr);
			col_costs[elm].flags |= NEIGHBOR_CONSTR;
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
	ir_node *irn       = ci->irn;
	be_ifg_t *ifg      = env->co->cenv->ifg;
	int n_regs         = env->co->cls->n_regs;
	bitset_t *forb     = bitset_alloca(n_regs);
	affinity_node_t *a = get_affinity_info(env->co, irn);

	bitset_pos_t elm;
	ir_node *pos;
	void *it;
	int i;

	if(get_irn_node_nr(irn) == 2040) {
		printf("Hallo");
	}

	/* Put all forbidden colors into the aux bitset. */
	admissible_colors(env, ci, forb);
	bitset_flip_all(forb);

	for(i = 0; i < n_regs; ++i) {
		col_costs[i].col   = i;
		col_costs[i].costs = 0;
		col_costs[i].flags = 0;
	}

	if(a) {
		neighb_t *n;

		co_gs_foreach_neighb(a, n) {
			if(color_is_fix(env, n->irn)) {
				col_t col = get_col(env, n->irn);
				col_costs[col].costs = add_saturated(col_costs[col].costs, -n->costs * 128);
			}

			incur_constraint_costs(env, n->irn, col_costs, -n->costs);
		}
	}

	it = be_ifg_neighbours_iter_alloca(ifg);
	be_ifg_foreach_neighbour(ifg, it, irn, pos) {
		col_t col = get_col(env, pos);
		if(color_is_fix(env, pos)) {
			col_costs[col].costs  = INT_MAX;
			col_costs[col].flags |= NEIGHBOR_FIXED;
		}
		else {
			incur_constraint_costs(env, pos, col_costs, INT_MAX);
			col_costs[col].costs = add_saturated(col_costs[col].costs, 8 * be_ifg_degree(ifg, pos));
		}
	}

	/* Set the costs to infinity for each color which is not allowed at this node. */
	bitset_foreach(forb, elm) {
		col_costs[elm].costs  = INT_MAX;
		col_costs[elm].flags |= SELF_CONSTR;
	}

}

static void single_color_cost(co2_t *env, col_t col, col_cost_pair_t *seq)
{
	int n_regs = env->co->cls->n_regs;
	int i;

	for(i = 0; i < n_regs; ++i) {
		seq[i].col   = i;
		seq[i].costs = INT_MAX;
		seq[i].flags = 0;
		seq[i].flags = DONT_WANT;
	}

	seq[col].col = 0;
	seq[0].col   = col;
	seq[0].costs = 0;
	seq[0].flags = 0;
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

static int cloud_costs(co2_t *env, co2_cloud_t *cloud)
{
	int costs = 0;
	co2_irn_t *ci;

	list_for_each_entry(co2_irn_t, ci, &cloud->members_head, cloud_list) {
		affinity_node_t *a = get_affinity_info(env->co, ci->irn);
		costs += curr_costs(env, a);
	}

	return costs;
}

static void reject_coloring(struct list_head *h)
{
	co2_irn_t *pos;

	list_for_each_entry(co2_irn_t, pos, h, changed_list)
		pos->tmp_fixed = 0;
}

static void materialize_coloring(struct list_head *h)
{
	co2_irn_t *pos;

	list_for_each_entry(co2_irn_t, pos, h, changed_list) {
		pos->orig_col = pos->tmp_col;
		pos->tmp_fixed = 0;
	}
}

typedef struct {
	co2_irn_t *ci;
	col_t col;
} col_entry_t;

static col_entry_t *save_coloring(struct obstack *obst, struct list_head *changed)
{
	co2_irn_t *pos;
	col_entry_t ent;

	list_for_each_entry(co2_irn_t, pos, changed, changed_list) {
		ent.ci  = pos;
		ent.col = pos->tmp_col;
		pos->tmp_col = 0;
		obstack_grow(obst, &ent, sizeof(ent));
	}
	memset(&ent, 0, sizeof(ent));
	obstack_grow(obst, &ent, sizeof(ent));
	return obstack_finish(obst);
}

static int change_color_not(co2_t *env, ir_node *irn, col_t not_col, struct list_head *parent_changed, int depth);
static int change_color_single(co2_t *env, ir_node *irn, col_t tgt_col, struct list_head *parent_changed, int depth);

static int recolor(co2_t *env, ir_node *irn, col_cost_pair_t *col_list, struct list_head *parent_changed, int depth)
{
	int n_regs         = env->co->cls->n_regs;
	be_ifg_t *ifg      = env->co->cenv->ifg;
	co2_irn_t *ci      = get_co2_irn(env, irn);
	int res            = 0;
	int n_aff          = 0;

	int i;

	for(i = 0; i < n_regs; ++i) {
		col_t tgt_col  = col_list[i].col;
		unsigned costs = col_list[i].costs;
		int neigh_ok   = 1;

		struct list_head changed;
		ir_node *n;
		void *it;

		DBG((env->dbg, LEVEL_3, "\t\t%2{firm:indent}trying color %d(%d) on %+F\n", depth, tgt_col, costs, irn));

		/* If the costs for that color (and all successive) are infinite, bail out we won't make it anyway. */
		if(INFEASIBLE(costs)) {
			DB((env->dbg, LEVEL_4, "\t\t%2{firm:indent}color %d infeasible due to %s\n", depth, tgt_col, flag_str(col_list[i].flags)));
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

static int change_color_not(co2_t *env, ir_node *irn, col_t not_col, struct list_head *parent_changed, int depth)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	int res       = 0;
	col_t col     = get_col(env, irn);

	DBG((env->dbg, LEVEL_3, "\t\t%2{firm:indent}clearing %+F(%d) of color %d\n", depth, irn, col, not_col));

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
		determine_color_costs(env, ci, csts);

		/* Since the node must not have the not_col, set the costs for that color to "infinity" */
		csts[not_col].costs = INT_MAX;

		/* sort the colors according costs, cheapest first. */
		qsort(csts, n_regs, sizeof(csts[0]), col_cost_pair_lt);

		/* Try recoloring the node using the color list. */
		res = recolor(env, irn, csts, parent_changed, depth);
	}

	/* If we came here, everything went ok. */
	return res;
}

static int change_color_single(co2_t *env, ir_node *irn, col_t tgt_col, struct list_head *parent_changed, int depth)
{
	co2_irn_t *ci = get_co2_irn(env, irn);
	col_t col     = get_col(env, irn);
	int res       = 0;

	DBG((env->dbg, LEVEL_3, "\t\t%2{firm:indent}trying to set %+F(%d) to color %d\n", depth, irn, col, tgt_col));

	/* If the color is already fix, bail out. */
	if(color_is_fix(env, irn))
		return 0;

	/* the node has the wanted color. That's fine, mark it as visited and return. */
	if(col == tgt_col) {
		if(!ci->tmp_fixed) {
			ci->tmp_col   = col;
			ci->tmp_fixed = 1;
		}

		list_add(&ci->changed_list, parent_changed);
		DB((env->dbg, LEVEL_3, "\t\tok\n"));
		return 1;
	}

	else {
		int n_regs           = env->co->cls->n_regs;
		col_cost_pair_t *seq = alloca(n_regs * sizeof(seq[0]));

		/* Get the costs for giving the node a specific color. */
		single_color_cost(env, tgt_col, seq);

		/* Try recoloring the node using the color list. */
		res = recolor(env, irn, seq, parent_changed, depth);

		DB((env->dbg, LEVEL_3, "\t\tcolor %d %s for %+F\n", tgt_col, res ? "was ok" : "failed", irn));
	}

	return res;
}


#if 0
static void try_color(co2_t *env, co2_irn_t *ci, col_t col, struct list_head *parent_changed)
{
	be_ifg_t *ifg            = env->co->cenv->ifg;
	int n_regs               = env->co->cls->n_regs;
	col_cost_pair_t *col_seq = alloca(n_regs * sizeof(col_seq[0]));
	affinity_node_t *a       = get_affinity_info(env->co, ci->irn);
	co2_irn_t **nbs          = alloca(a->degree * sizeof(nbs[0]));
	int ok = 0;

	col_t new_col;
	neighb_t *n;

	assert(a != NULL && "This node must be an affinity node");

	/* If that node has already been fixed, leave it alone. */
	if(color_is_fix(env, ci->irn) || !is_color_admissible(env, ci, col)) {
		// DB((env->dbg, LEVEL_2, "\t-> color is already fix: %d\n", get_col(env, ci->irn)));
		return;
	}

	DB((env->dbg, LEVEL_1, "\taffinity node %+F cost %d trying color %d\n", ci->irn, ci->costs, col));

	single_color_cost(env, col, col_seq);
	recolor(env, ci->irn, col_seq, parent_changed, 0);
	new_col = get_col(env, ci->irn);

	ci->tmp_fixed = 1;
	ci->tmp_col   = new_col;

	DB((env->dbg, LEVEL_2, "\t-> has color %d now. %d wanted\n", new_col, col));

	i = 0;
	co_gs_foreach_neighb(a, n)
		nbs[i++] = get_co2_irn(env, n->irn);

	co_gs_foreach_neighb(a, n) {
		co2_irn_t *ni = get_co2_irn(env, n->irn);
		col_t tgt_col = be_ifg_connected(ifg, ci->irn, ni->irn) ? get_col(env, ni->irn) : new_col;
		try_color(env, ni, tgt_col, parent_changed);
	}
}

static void process_cloud(co2_t *env, co2_cloud_t *cloud)
{
	int n_regs            = env->co->cls->n_regs;
	col_cost_pair_t *cols = alloca(n_regs * sizeof(cols[0]));
	int best_costs        = cloud_costs(env, cloud);
	int best_col          = 0;

	struct list_head changed;
	co2_irn_t *ci;
	int i;


	i = 0;
	DB((env->dbg, LEVEL_2, "processing cloud with costs %d and master %+F containing:\n", cloud->costs, cloud->master->irn));
	list_for_each_entry(co2_irn_t, ci, &cloud->members_head, cloud_list) {
		DB((env->dbg, LEVEL_2, "\t%+F %d\n", ci->irn, ci->costs));
	}

	determine_color_costs(env, cloud->master, cols);
	qsort(cols, n_regs, sizeof(cols[0]), col_cost_pair_lt);

	best_col = cols[0].col;
	for(i = 0; i < n_regs; ++i) {
		col_t col  = cols[i].col;
		int reject = 1;
		int costs;

		INIT_LIST_HEAD(&changed);
		DBG((env->dbg, LEVEL_2, "\n\ttrying color %d. current costs: %d\n", col, best_costs));

		/* try to recolor all the cloud members. */
		try_color(env, cloud->master, col, &changed);

		/* recoloring of all nodes did succeed. measure the costs and decide if the coloring shall be kept. */
		costs = cloud_costs(env, cloud);

		/* materialize the new coloring. */
		if(costs < best_costs) {
			materialize_coloring(&changed);
			best_costs = costs;
			best_col   = col;
			reject     = 0;
		}

		/* We won't get the cloud any better so stop it. */
		if(costs == 0)
			break;

		if(reject)
			reject_coloring(&changed);
	}

	DB((env->dbg, LEVEL_2, "\tfinished cloud with costs %d\n", best_costs));

	/* fix all cloud members */
	list_for_each_entry(co2_irn_t, ci, &cloud->members_head, cloud_list) {
		ci->fixed = 1;
	}

	{
		char buf[256];
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "ifg_%F_%s_cloud_%F.dot", env->co->irg, env->co->cls->name, cloud->master);
		if(f = fopen(buf, "wt")) {
			be_ifg_dump_dot(env->co->cenv->ifg, env->co->irg, f, &ifg_dot_cb, env);
			fclose(f);
		}
	}
}

static void try_affinity_node(co2_t *env, co2_irn_t *ci, col_t preferred, struct list_head *parent_changed)
{
	ir_node *irn = ci->irn;

	if(!color_is_fix(env, irn)) {
		int n_regs      = env->co->cls->n_regs;
		bitset_t *tried = bitset_alloca(n_regs);
		bitset_t *adm   = bitset_alloca(n_regs);
		col_cost_pair_t *seq = alloca(n_regs * sizeof(seq[0]));

		affinity_node_t *a = get_affinity_info(env->co, irn);
		int best_costs  = cloud_costs(env, ci->cloud);
		int best_col    = get_col(env, ci->irn);

		int i;

		determine_color_costs(env, ci, seq);
		if(!INFEASIBLE(seq[preferred].costs))
			seq[preferred].costs = INT_MIN;

		qsort(seq, n_regs, sizeof(seq[0]), col_cost_pair_lt);

		for(i = 0; i < n_regs; ++i) {
			col_t col = seq[i].col;

			struct list_head changed;
			int ok, costs;

			INIT_LIST_HEAD(&changed);
			ok  = change_color_single(env, irn, col, &changed, 0);
			col = get_col(env, irn);

			if(!bitset_is_set(tried, col)) {
				neighb_t *n;

				if(!ci->tmp_col) {
					ci->tmp_col   = col;
					ci->tmp_fixed = 1;
					list_add(&ci->changed_list, &changed);
				}

				co_gs_foreach_neighb(a, n) {
					co2_irn_t *ni = get_co2_irn(env, n->irn);
					try_affinity_node(env, ni, col, &changed);
				}

				examine_coloring(env, ci->cloud);
				reject_coloring(&changed);

				bitset_set(tried, col);
			}
		}
	}
}
#endif

static void examine_cloud_coloring(co2_t *env, co2_cloud_t *cloud)
{
	int costs = cloud_costs(env, cloud);

	if(costs < cloud->best_costs) {
		int i;

		for(i = 0; i < cloud->n_memb; ++i)
			cloud->best_cols[i] = get_col(env, cloud->seq[i]->irn);

		cloud->best_costs = costs;
	}
}

static int color_change_balance(co2_t *env, co2_irn_t *ci, bitset_t *tried_colors)
{
	col_t col = get_col(env, ci->irn);
	neighb_t *n;
	int balance = 0;

	co_gs_foreach_neighb(ci->aff, n) {
		col_t nc  = get_col(env, n->irn);
		int fixed = color_is_fix(env, n->irn);

		if(nc == col)
			balance -= n->costs;
		else if(!fixed || !bitset_is_set(tried_colors, nc))
			balance += n->costs;
	}

	DBG((env->dbg, LEVEL_4, "\t\tbalance for changing %+F color %d\n", ci->irn, balance));
	return balance;
}

static void adjust_start_colors(co2_t *env, co2_cloud_t *cloud, col_cost_pair_t *seq)
{
	int n_regs    = env->co->cls->n_regs;
	bitset_t *adm = bitset_alloca(n_regs);
	bitset_pos_t col;
	int i;

	for(i = 0; i < cloud->n_memb; ++i) {
		co2_irn_t *ci = cloud->seq[i];
		int n_constr;

		/* Prefer precolored neighbors. */
		bitset_clear_all(adm);
		admissible_colors(env, ci, adm);
		n_constr = bitset_popcnt(adm);

		bitset_foreach(adm, col) {
			seq[col].costs = add_saturated(seq[col].costs, -128 * (n_regs - n_constr));
		}

		bitset_foreach_clear(adm, col) {
			seq[col].costs = add_saturated(seq[col].costs, 128);
		}
	}

	admissible_colors(env, cloud->master, adm);
	bitset_flip_all(adm);

	bitset_foreach(adm, col)
		seq[col].costs = INT_MAX;
}

static int process_node(co2_t *env, co2_cloud_t *cloud, int index)
{
	struct list_head changed;
	int res = 0;

	if(index < cloud->n_memb) {
		co2_irn_t *ci           = cloud->seq[index];
		int n_regs              = env->co->cls->n_regs;
		col_cost_pair_t *seq    = alloca(n_regs * sizeof(seq[0]));
		bitset_t *cols_tried    = bitset_alloca(n_regs);
		int done                = 0;
		//col_cost_pair_t *single = alloca(n_regs * sizeof(seq[0]));

		int i;

		determine_color_costs(env, ci, seq);
		if(index == 0)
			adjust_start_colors(env, cloud, seq);

#if 0
		if(index == 0) {
			col_t col     = get_col(env, ci->irn);
			int min_costs = INT_MAX;
			int i;

			for(i = 0; i < n_regs; ++i)
				min_costs = MIN(min_costs, seq[i].costs);

			seq[col].costs = min_costs - 1;
		}
#endif

		qsort(seq, n_regs, sizeof(seq[0]), col_cost_pair_lt);

#if 0
		if(index == cloud->n_memb - 1) {
			for(i = 0; i < n_regs; ++i)
				if(seq[i].costs >= 0)
					seq[i].costs = INT_MAX;
		}
#endif


		for(i = 0; i < n_regs && !done; ++i) {
			col_t col = seq[i].col;
			int costs = seq[i].costs;
			int ok;

			/*
				if all affinity neighbors fixed,
				try only color changes to affinity colors.
				all other colors do no good.
			*/

			DB((env->dbg, LEVEL_2, "\t%2{firm:indent}trying %+F index %d for color %d\n", index, ci->irn, index, col));
			if(INFEASIBLE(costs)) {
				DBG((env->dbg, LEVEL_2, "\t%2{firm:indent}-> color is infeasible due to %s\n", index, flag_str(seq[i].flags)));
				break;
			}

			bitset_set(cols_tried, col);
			INIT_LIST_HEAD(&changed);
			ok = change_color_single(env, ci->irn, col, &changed, 0);
			DB((env->dbg, LEVEL_2, "\t%2{firm:indent}-> %s\n", index, ok ? "ok" : "failed"));

			/* if we succeeded changing the color, we will figure out the next node. */
			if(ok) {
				int finish;

				/* materialize the coloring and fix the node's color. */
				ci->fixed = 1;

				/* process the next nodes. if the function returns one, we found an optimal coloring already, so get out. */
				finish = process_node(env, cloud, index + 1);

				/* if this is the last node in the coloring sequence, examine the coloring */
				if(index == cloud->n_memb - 1) {
					examine_cloud_coloring(env, cloud);
					DB((env->dbg, LEVEL_2, "\t%2{firm:indent}-> current best coloring %d\n", index, cloud->best_costs));
					if(cloud->best_costs == cloud->inevit) {
						done = 1;
						res  = 1;
					}
				}

				/* unfix the node. */
				reject_coloring(&changed);
				ci->fixed = 0;

				if(finish || color_change_balance(env, ci, cols_tried) <= 0) {
					res  = finish;
					done = 1;
				}
			}

		}
	}

	return res;
}

static co2_irn_t **get_neighb_arr(co2_t *env, co2_irn_t *ci, co2_irn_t **nbs)
{
	int i;
	neighb_t *n;

	i = 0;
	co_gs_foreach_neighb(ci->aff, n) {
		nbs[i++] = get_co2_irn(env, n->irn);
	}

	qsort(nbs, ci->aff->degree, sizeof(nbs[0]), co2_irn_cmp);
	return nbs;
}

static void determine_coloring_sequence(co2_t *env, co2_cloud_t *cloud)
{
	pdeq *q         = new_pdeq1(cloud->master);
	bitset_t *seen  = bitset_malloc(get_irg_last_idx(env->co->irg));
	co2_irn_t **nbs = alloca(cloud->max_degree * sizeof(nbs[0]));
	int i, j;

	j = 0;
	bitset_set(seen, get_irn_idx(cloud->master->irn));
	while(!pdeq_empty(q)) {
		co2_irn_t *curr = pdeq_getl(q);

		cloud->seq[j++] = curr;
		get_neighb_arr(env, curr, nbs);

		for(i = 0; i < curr->aff->degree; ++i) {
			co2_irn_t *ni = nbs[i];
			int idx       = get_irn_idx(ni->irn);
			if(!bitset_is_set(seen, idx)) {
				pdeq_putr(q, ni);
				bitset_set(seen, idx);
			}
		}
	}

	del_pdeq(q);
	bitset_free(seen);
}

static void populate_cloud(co2_t *env, co2_cloud_t *cloud, affinity_node_t *a, int curr_costs)
{
	be_ifg_t *ifg = env->co->cenv->ifg;
	co2_irn_t *ci = get_co2_irn(env, a->irn);
	int costs     = 0;
	neighb_t *n;

	if(ci->visited >= env->visited)
		return;

	/* mark the node as visited and add it to the cloud. */
	ci->visited = env->visited;
	ci->cloud   = cloud;
	list_add(&ci->cloud_list, &cloud->members_head);

	DB((env->dbg, LEVEL_3, "%+F\n", ci->irn));

	/* determine the nodes costs */
	co_gs_foreach_neighb(a, n) {
		costs += n->costs;
		DB((env->dbg, LEVEL_3, "\t%+F\n", n->irn));
		if(be_ifg_connected(ifg, a->irn, n->irn))
			cloud->inevit += n->costs;
	}

	/* add the node's cost to the total costs of the cloud. */
	ci->costs          = costs;
	cloud->costs      += costs;
	cloud->max_degree  = MAX(cloud->max_degree, ci->aff->degree);
	cloud->n_memb++;

	/* If this is the heaviest node in the cloud, set it as the cloud's master. */
	if(costs >= curr_costs) {
		cloud->master = ci;
		curr_costs    = costs;
	}

	/* add all the neighbors of the node to the cloud. */
	co_gs_foreach_neighb(a, n) {
		affinity_node_t *an = get_affinity_info(env->co, n->irn);
		assert(an);
		populate_cloud(env, cloud, an, curr_costs);
	}
}

static void init_cloud(co2_t *env, co2_cloud_t *cloud, affinity_node_t *a)
{
	env->visited++;
	populate_cloud(env, cloud, a, 0);

	cloud->best_cols = phase_alloc(&env->ph, cloud->n_memb * sizeof(cloud->best_cols[0]));
	cloud->seq       = phase_alloc(&env->ph, cloud->n_memb * sizeof(cloud->seq[0]));
	env->visited++;
	cloud->seq[0] = cloud->master;
	determine_coloring_sequence(env, cloud);
}

static void process_cloud(co2_t *env, co2_cloud_t *cloud, int nr)
{
	struct list_head changed;
	int i;

	/* initialize the best coloring. */
	examine_cloud_coloring(env, cloud);

	DB((env->dbg, LEVEL_1, "\nnew cloud\nall costs %d, initial costs %d, inevit %d\n", cloud->costs, cloud->best_costs, cloud->inevit));
	for(i = 0; i < cloud->n_memb; ++i) {
		co2_irn_t *ci = cloud->seq[i];
		DB((env->dbg, LEVEL_1, "\tmember %+F cost %d col %d\n", ci->irn, ci->costs, get_col(env, ci->irn)));
	}

	process_node(env, cloud, 0);
	DB((env->dbg, LEVEL_1, "final coloring costs %d\n", cloud->best_costs));

	/* re-try the best coloring. */
	INIT_LIST_HEAD(&changed);
	for(i = 0; i < cloud->n_memb; ++i) {
		co2_irn_t *ci = cloud->seq[i];
		col_t col     = cloud->best_cols[i];

		int ok;

		DB((env->dbg, LEVEL_2, "\tsetting %+F to %d\n", ci->irn, col));
		ok = change_color_single(env, ci->irn, col, &changed, 0);
		assert(ok);
		ci->fixed = 1;
	}
	materialize_coloring(&changed);

	{
		co2_irn_t *ci;
		int some_fixed = 0;
		for(ci = env->touched; ci; ci = ci->touched_next) {
			if(ci->tmp_fixed) {
				some_fixed = 1;
				ir_printf("%+F is still temp fixed\n", ci->irn);
			}
		}
		assert(!some_fixed);
	}

	{
		char buf[256];
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "ifg_%F_%s_cloud_%d.dot", env->co->irg, env->co->cls->name, nr);
		if(f = fopen(buf, "wt")) {
			be_ifg_dump_dot(env->co->cenv->ifg, env->co->irg, f, &ifg_dot_cb, env);
			fclose(f);
		}
	}

}

static void process(co2_t *env)
{
	affinity_node_t *a;
	co2_cloud_t *pos;
	co2_cloud_t **clouds;
	int n_clouds;
	int i;
	int init_costs  = 0;
	int all_costs   = 0;
	int final_costs = 0;


	n_clouds = 0;
	co_gs_foreach_aff_node(env->co, a) {
		co2_irn_t *ci = get_co2_irn(env, a->irn);

		if(!ci->cloud) {
			co2_cloud_t *cloud = new_cloud(env);

			init_cloud(env, cloud, a);
			list_add(&cloud->list, &env->cloud_head);
			n_clouds++;
		}
	}

	i = 0;
	clouds = xmalloc(n_clouds * sizeof(clouds[0]));
	list_for_each_entry(co2_cloud_t, pos, &env->cloud_head, list)
		clouds[i++] = pos;
	qsort(clouds, n_clouds, sizeof(clouds[0]), cmp_clouds);

	for(i = 0; i < n_clouds; ++i) {
		init_costs  += cloud_costs(env, clouds[i]);
		process_cloud(env, clouds[i], i);
		all_costs   += clouds[i]->costs;
		final_costs += clouds[i]->best_costs;
	}

	DB((env->dbg, LEVEL_1, "all costs: %d, init costs: %d, final costs: %d\n", all_costs, init_costs, final_costs));

	xfree(clouds);
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

/*
  ___ _____ ____   ____   ___ _____   ____                        _
 |_ _|  ___/ ___| |  _ \ / _ \_   _| |  _ \ _   _ _ __ ___  _ __ (_)_ __   __ _
  | || |_ | |  _  | | | | | | || |   | | | | | | | '_ ` _ \| '_ \| | '_ \ / _` |
  | ||  _|| |_| | | |_| | |_| || |   | |_| | |_| | | | | | | |_) | | | | | (_| |
 |___|_|   \____| |____/ \___/ |_|   |____/ \__,_|_| |_| |_| .__/|_|_| |_|\__, |
                                                           |_|            |___/
*/

static const char *get_dot_color_name(int col)
{
	static const char *names[] = {
		"blue",
		"red",
		"green",
		"yellow",
		"cyan",
		"magenta",
		"orange",
		"chocolate",
		"beige",
		"navy",
		"darkgreen",
		"darkred",
		"lightPink",
		"chartreuse",
		"lightskyblue",
		"linen",
		"pink",
		"lightslateblue",
		"mintcream",
		"red",
		"darkolivegreen",
		"mediumblue",
		"mistyrose",
		"salmon",
		"darkseagreen",
		"mediumslateblue"
		"moccasin",
		"tomato",
		"forestgreen",
		"darkturquoise",
		"palevioletred"
	};

	return col < sizeof(names)/sizeof(names[0]) ? names[col] : "white";
}

static const char *get_dot_shape_name(co2_t *env, co2_irn_t *ci)
{
	arch_register_req_t req;

	arch_get_register_req(env->co->aenv, &req, ci->irn, BE_OUT_POS(0));
	if(arch_register_req_is(&req, limited))
		return "diamond";

	if(ci->fixed)
		return "rectangle";

	if(ci->tmp_fixed)
		return "hexagon";

	return "ellipse";
}

static void ifg_dump_graph_attr(FILE *f, void *self)
{
	fprintf(f, "overlay=false");
}

static int ifg_is_dump_node(void *self, ir_node *irn)
{
	co2_t *env = self;
	return !arch_irn_is(env->co->aenv, irn, ignore);
}

static void ifg_dump_node_attr(FILE *f, void *self, ir_node *irn)
{
	co2_t *env    = self;
	co2_irn_t *ci = get_co2_irn(env, irn);

	ir_fprintf(f, "label=\"%+F,%d\" style=filled color=%s shape=%s", irn, ci->costs,
		get_dot_color_name(get_col(env, irn)), get_dot_shape_name(env, ci));
}

static void ifg_dump_at_end(FILE *file, void *self)
{
	co2_t *env = self;
	affinity_node_t *a;

	co_gs_foreach_aff_node(env->co, a) {
		int idx = get_irn_idx(a->irn);
		neighb_t *n;

		co_gs_foreach_neighb(a, n) {
			int nidx = get_irn_idx(n->irn);

			if(idx < nidx) {
				const char *style = get_col(env, a->irn) == get_col(env, n->irn) ? "dashed" : "dotted";
				fprintf(file, "\tn%d -- n%d [label=\"%d\" style=%s weight=0.01];\n", idx, nidx, n->costs, style);
			}
		}
	}

}

static be_ifg_dump_dot_cb_t ifg_dot_cb = {
	ifg_is_dump_node,
	ifg_dump_graph_attr,
	ifg_dump_node_attr,
	NULL,
	NULL,
	ifg_dump_at_end
};

void co_solve_heuristic_new(copy_opt_t *co)
{
	co2_t env;
	FILE *f;

	phase_init(&env.ph, "co2", co->cenv->birg->irg, sizeof(co2_irn_t), PHASE_DEFAULT_GROWTH, co2_irn_init);
	env.touched     = NULL;
	env.visited     = 0;
	env.co          = co;
	env.ignore_regs = bitset_alloca(co->cls->n_regs);
	arch_put_non_ignore_regs(co->aenv, co->cls, env.ignore_regs);
	bitset_flip_all(env.ignore_regs);
	be_abi_put_ignore_regs(co->cenv->birg->abi, co->cls, env.ignore_regs);
	FIRM_DBG_REGISTER(env.dbg, "firm.be.co2");
	INIT_LIST_HEAD(&env.cloud_head);

	if(f = be_chordal_open(co->cenv, "ifg_before_", "dot")) {
		be_ifg_dump_dot(co->cenv->ifg, co->irg, f, &ifg_dot_cb, &env);
		fclose(f);
	}

	process(&env);

	if(f = be_chordal_open(co->cenv, "ifg_after_", "dot")) {
		be_ifg_dump_dot(co->cenv->ifg, co->irg, f, &ifg_dot_cb, &env);
		fclose(f);
	}

	writeback_colors(&env);
	phase_free(&env.ph);
}
