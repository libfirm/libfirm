/**
 * Author:      Daniel Grund
 * Date:		28.02.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * ILP formalization using G=(V, E, Q):
 *  - 1 class of variables: equal color vars
 *  - Path constraints
 *  - Clique path constraints
 *
 *
 *	\min \sum_{ (i,j) \in Q }  w_ij y_ij
 *
 *		y_ij				=  1			(i,j) \in E
 *
 *		\sum_c y_nc			=  |C| - 1		n \in N, c \in C
 *
 *		y_nc				=  1			n \in N, c \not\in C(n)
 *
 *		\sum_{e \in p} y_e	>= 1			p \in P		path constraints
 *
 *		\sum_{e \in cp} y_e	>= |cp| - 1		cp \in CP	clique-path constraints
 *
 *		y_ij \in N,   w_ij \in R^+
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef WITH_ILP

#include "becopyilp_t.h"
#include "beifg_t.h"
#include "irtools.h"

#define DEBUG_LVL 1

typedef struct _local_env_t {
	firm_dbg_module_t *dbg;
	double time_limit;
	int first_x_var, last_x_var;
	pmap *nr_2_irn;
} local_env_t;

static void build_coloring_cstr(ilp_env_t *ienv) {
	be_ifg_t *ifg = ienv->co->cenv->ifg;
	void *iter = be_ifg_nodes_iter_alloca(ifg);
	bitset_t *colors;
	ir_node *irn;
	char buf[16];

	colors = bitset_alloca(arch_register_class_n_regs(ienv->co->cls));

	be_ifg_foreach_node(ifg, iter, irn)
		if (!sr_is_removed(ienv->sr, irn)) {
			int col, cst_idx;
			arch_register_req_t req;
			int curr_node_color = get_irn_col(ienv->co, irn);
			int node_nr = (int)get_irn_node_nr(irn);
			local_env_t *lenv = ienv->env;

			pmap_insert(lenv->nr_2_irn, INT_TO_PTR(node_nr), irn);

			arch_get_register_req(ienv->co->aenv, &req, irn, -1);

			/* get assignable colors */
			if (arch_register_req_is(&req, limited))
				req.limited(req.limited_env, colors);
			else
				arch_put_non_ignore_regs(ienv->co->aenv, req.cls, colors);

			/* add the coloring constraint */
			cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_equal, 1.0);

			bitset_foreach(colors, col) {
				int var_idx = lpp_add_var(ienv->lp, name_cdd(buf, 'x', node_nr, col), lpp_binary, 0.0);
				lpp_set_start_value(ienv->lp, var_idx, (col == curr_node_color) ? 1.0 : 0.0);
				lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1);

				lenv->last_x_var = var_idx;
				if (lenv->first_x_var == -1)
					lenv->first_x_var = var_idx;
			}

			/* add register constraint constraints */
			bitset_foreach_clear(colors, col) {
				int cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_equal, 0.0);
				int var_idx = lpp_add_var(ienv->lp, name_cdd(buf, 'x', node_nr, col), lpp_binary, 0.0);
				lpp_set_start_value(ienv->lp, var_idx, 0.0);
				lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1);

				lenv->last_x_var = var_idx;
			}
		}
}

static void build_interference_cstr(ilp_env_t *ienv) {
	lpp_t *lpp = ienv->lp;
	be_ifg_t *ifg = ienv->co->cenv->ifg;
	int n_colors = arch_register_class_n_regs(ienv->co->cls);
	int i, col;

	void *iter = be_ifg_cliques_iter_alloca(ifg);
	ir_node *clique = alloca(sizeof(*clique) * n_colors);
	int size;

	char buf[16];

	/* for each maximal clique */
	be_ifg_foreach_clique(ifg, iter, &clique, &size) {

		if (size < 2)
			continue;

		/* for all colors */
		for (col=0; col<n_colors; ++col) {
			int cst_idx = lpp_add_cst(lpp, NULL, lpp_less, 1.0);

			/* for each member of this clique */
			for (i=0; i<size, ++i) {
				ir_node *irn = clique[i];

				if (!sr_is_removed(ienv->sr, irn)) {
					int var_idx = lpp_get_var_idx(lpp, name_cdd(buf, 'x', (int)get_irn_node_nr(irn), col));
					lpp_set_factor_fast(lpp, cst_idx, var_idx, 1);
				}
			}
		}
	}
}

static void build_affinity_cstr(ilp_env_t *ienv) {
	unit_t *curr;
	int n_colors = arch_register_class_n_regs(ienv->co->cls);

	/* for all optimization units */
	list_for_each_entry(unit_t, curr, &ienv->co->units, units) {
		ir_node *root, *arg;
		int root_nr, arg_nr, i, col, y_idx, root_idx, arg_idx;
		char buf[16];
		int root_col, arg_col;

		root = curr->nodes[0];
		root_nr = (int) get_irn_node_nr(root);
		root_col = get_irn_col(ienv->co, root);

		for (i = 1; i < curr->node_count; ++i) {
			arg = curr->nodes[i];
			arg_nr = (int) get_irn_node_nr(arg);
			arg_col = get_irn_col(ienv->co, arg);

			/* add a new affinity variable */
			y_idx = lpp_add_var(ienv->lp, name_cdd_sorted(buf, 'y', root_nr, arg_nr), lpp_binary, curr->costs[i]);
			lpp_set_start_value(ienv->lp, y_idx, (root_col==arg_col) ? 0.0 : 1.0);

			/* add constraints relating the affinity var to the color vars */
			for (col=0; col<n_colors; ++col) {
				int cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_less, 0.0);
				root_idx = lpp_get_var_idx(ienv->lp, name_cdd(buf, 'x', root_nr, col));
				arg_idx  = lpp_get_var_idx(ienv->lp, name_cdd(buf, 'x', arg_nr,  col));

				lpp_set_factor_fast(ienv->lp, cst_idx, root_idx,  1.0);
				lpp_set_factor_fast(ienv->lp, cst_idx, arg_idx,  -1.0);
				lpp_set_factor_fast(ienv->lp, cst_idx, root_idx, -1.0);
			}
		}
	}
}

static void build_path_cstr(ilp_env_t *ienv) {

}

static void build_clique_path_cstr(ilp_env_t *ienv) {

}

static void ilp2_build(ilp_env_t *ienv) {
	local_env_t *lenv = ienv->env;
	int lower_bound;

	ienv->lp = new_lpp(ienv->co->name, lpp_minimize);
	build_coloring_cstr(ienv);
	build_interference_cstr(ienv);
	build_affinity_cstr(ienv);
	build_path_cstr(ienv);
	build_clique_path_cstr(ienv);

	lower_bound = co_get_lower_bound(ienv->co) - co_get_inevit_copy_costs(ienv->co);
	lpp_set_bound(ienv->lp, lower_bound);
	lpp_set_time_limit(ienv->lp, lenv->time_limit);
}

static void ilp2_apply(ilp_env_t *ienv) {
	local_env_t *lenv = ienv->env;
	double sol[];
	lpp_sol_state_t state;
	int count;

	count = lenv->last_x_var - lenv->first_x_var + 1;
	sol = xmalloc(count * sizeof(sol[0]));
	state = lpp_get_solution(ienv->lp, sol, lenv->first_x_var, lenv->last_x_var);
	if (state != lpp_optimal) {
		printf("WARNING %s: Solution state is not 'optimal': %d\n", ienv->co->name, state);
		assert(state >= lpp_feasible && "The solution should at least be feasible!");
	}

	for (i=0; i<count; ++i) {
		char c;
		int nodenr, color;
		char var_name[16];

		if (sol[i] > 1-EPSILON) { /* split variable name into components */
			lpp_get_var_name(ienv->lp, lenv->first_x_var+i, var_name, sizeof(var_name));

			if (sscanf(var_name, "x_%d_%d", &nodenr, &color) == 2) {
				ir_node *irn = pmap_get(lenv->nr_2_irn, INT_TO_PTR(nodenr));
				assert(irn && "This node number must be present in the map");

				set_irn_col(ienv->co, irn, color);
			} else
				assert(0 && "This should be a x-var");
		}
	}

#ifdef COPYOPT_STAT
	/* TODO adapt to multiple possible ILPs */
	copystat_add_ilp_time((int)(1000.0*lpp_get_sol_time(pi->curr_lp)));  //now we have ms
	copystat_add_ilp_vars(lpp_get_var_count(pi->curr_lp));
	copystat_add_ilp_csts(lpp_get_cst_count(pi->curr_lp));
	copystat_add_ilp_iter(lpp_get_iter_cnt(pi->curr_lp));
#endif
}

int co_solve_ilp2(copy_opt_t *co, double time_limit) {
	lpp_sol_state_t sol_state;
	ilp_env_t *ienv;
	local_env_t my;

	my.time_limit  = time_limit;
	my.first_x_var = -1;
	my.last_x_var  = -1;
	my.nr_2_irn    = pmap_create();
	my.dbg         = firm_dbg_register("ir.be.coilp2");
	firm_dbg_set_mask(my.dbg, DEBUG_LVL);

	ienv = new_ilp_env(co, ilp2_build, ilp2_apply, &my);

	sol_state = ilp_go(ienv);

	pmap_destroy(my.nr_2_irn);
	free_ilp_env(ienv);

	return sol_state == lpp_optimal;
}

#else /* WITH_ILP */

static void only_that_you_can_compile_without_WITH_ILP_defined(void) {
}

#endif /* WITH_ILP */
