/**
 * Author:      Daniel Grund
 * Date:		28.02.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Common stuff used by all ILP fomulations.
 *
 */

#include "becopyilp_t.h"
#include "beifg_t.h"

/******************************************************************************
    _____ _                        _            _   _
   / ____(_)                      | |          | | (_)
  | (___  _ _______   _ __ ___  __| |_   _  ___| |_ _  ___  _ __
   \___ \| |_  / _ \ | '__/ _ \/ _` | | | |/ __| __| |/ _ \| '_ \
   ____) | |/ /  __/ | | |  __/ (_| | |_| | (__| |_| | (_) | | | |
  |_____/|_/___\___| |_|  \___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

 *****************************************************************************/


size_red_t *new_size_red(copy_opt_t *co) {
	size_red_t *res = malloc(sizeof(*res));

	res->co = co;
	res->all_removed = pset_new_ptr_default();
	res->col_suff = NULL;
	obstack_init(&res->ob);

	return res;
}

/**
 * Checks if a node is simplicial in the graph heeding the already removed nodes.
 */
static INLINE int sr_is_simplicial(size_red_t *sr, const ir_node *ifn) {
	int i, o, size = 0;
	ir_node **all, *curr;
	be_ifg_t *ifg = sr->co->cenv->ifg;
	void *iter = be_ifg_neighbours_iter_alloca(ifg);

	all = alloca(be_ifg_degree(ifg, ifn) * sizeof(*all));

	/* get all non-removed neighbors */
	be_ifg_foreach_neighbour(ifg, iter, ifn, curr)
		if (!sr_is_removed(sr, curr))
			all[size++] = curr;

	/* check if these form a clique */
	for (i=0; i<size; ++i)
		for (o=i+1; o<size; ++o)
			if (!be_ifg_connected(ifg, all[i], all[o]))
				return 0;

	/* all edges exist so this is a clique */
	return 1;
}

void sr_remove(size_red_t *sr) {
	ir_node *irn;
	int redo = 1;
	int n_nodes = 0;
	const be_ifg_t *ifg = sr->co->cenv->ifg;
	void *iter = be_ifg_neighbours_iter_alloca(ifg);

	while (redo) {
		redo = 0;
		be_ifg_foreach_node(ifg, iter, irn) {
			arch_register_req_t req;

			arch_get_register_req(sr->co->aenv, &req, irn, -1);

			if (!arch_register_req_is(&req, limited) && !sr_is_removed(sr, irn) && !co_gs_is_optimizable(sr->co, irn)) {
          	 	if (sr_is_simplicial(sr, irn)) {
					coloring_suffix_t *cs = obstack_alloc(&sr->ob, sizeof(*cs));

					cs->irn = irn;
					cs->next = sr->col_suff;
					sr->col_suff = cs;

					pset_insert_ptr(sr->all_removed, irn);

					redo = 1;
          	 	}
			}
		}
	}
}

void sr_reinsert(size_red_t *sr) {
	coloring_suffix_t *cs;
	be_ifg_t *ifg        = sr->co->cenv->ifg;
	bitset_t *used_cols  = bitset_alloca(arch_register_class_n_regs(sr->co->cls));
	void *iter           = be_ifg_neighbours_iter_alloca(ifg);

	/* color the removed nodes in right order */
	for (cs = sr->col_suff; cs; cs = cs->next) {
		int free_col;
		ir_node *other, *irn;

		/* get free color by inspecting all neighbors */
		irn = cs->irn;
		bitset_clear_all(used_cols);

		be_ifg_foreach_neighbour(ifg, iter, irn, other) {
			if (!sr_is_removed(sr, other)) /* only inspect nodes which are in graph right now */
				bitset_set(used_cols, get_irn_col(sr->co, other));
		}

		/* now all bits not set are possible colors */
		free_col = bitset_next_clear(used_cols, 0);
		assert(free_col != -1 && "No free color found. This can not be.");
		set_irn_col(sr->co, irn, free_col);
		pset_remove_ptr(sr->all_removed, irn); /* irn is back in graph again */
	}
}

void free_size_red(size_red_t *sr) {
	del_pset(sr->all_removed);
	obstack_free(&sr->ob, NULL);
	free(sr);
}

/******************************************************************************
    _____                      _        _____ _      _____
   / ____|                    (_)      |_   _| |    |  __ \
  | |  __  ___ _ __   ___ _ __ _  ___    | | | |    | |__) |
  | | |_ |/ _ \ '_ \ / _ \ '__| |/ __|   | | | |    |  ___/
  | |__| |  __/ | | |  __/ |  | | (__   _| |_| |____| |
   \_____|\___|_| |_|\___|_|  |_|\___| |_____|______|_|

 *****************************************************************************/

#include <stdio.h>

ilp_env_t *new_ilp_env(copy_opt_t *co, ilp_callback build, ilp_callback apply, void *env) {
	ilp_env_t *res = malloc(sizeof(*res));
	assert(res);

	res->co = co;
	res->build = build;
	res->apply = apply;
	res->env = env;
	res->sr = new_size_red(co);

	return res;
}

lpp_sol_state_t ilp_go(ilp_env_t *ienv) {
	FILE *f;
	char buf[256];

	sr_remove(ienv->sr);

	ienv->build(ienv);

#ifdef LPP_SOLVE_NET
	lpp_solve_net(ienv->lp, LPP_HOST, LPP_SOLVER);
#else
	lpp_solve_cplex(ienv->lp);
#endif

	snprintf(buf, sizeof(buf), "%s.ilp", ienv->co->name);
	f = fopen(buf, "wt");
	lpp_dump_plain(ienv->lp, f);
	fclose(f);

	ienv->apply(ienv);

	sr_reinsert(ienv->sr);

	return lpp_get_sol_state(ienv->lp);
}

void free_ilp_env(ilp_env_t *ienv) {
	free_size_red(ienv->sr);
	free_lpp(ienv->lp);
	free(ienv);
}
