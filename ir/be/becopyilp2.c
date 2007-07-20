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

/**
 * @file
 * @brief       ILP based copy minimization.
 * @author      Daniel Grund
 * @date        28.02.2006
 * @version     $Id$
 *
 * ILP formalization using G=(V, E, Q):
 *  - 2 class of variables: coloring vars x_ic   and   equal color vars y_ij
 *  - Path constraints
 *  - Clique-star constraints
 *
 *
 *	\min \sum_{ (i,j) \in Q }  w_ij y_ij
 *
 *		\sum_c x_nc			=  1			n \in N, c \in C
 *
 *		x_nc				=  0			n \in N, c \not\in C(n)
 *
 *		\sum x_nc			<= 1			x_nc \in Clique \in AllCliques,  c \in C
 *
 *		\sum_{e \in p} y_e	>= 1			p \in P		path constraints
 *
 *		\sum_{e \in cs} y_e	>= |cs| - 1		cs \in CP	clique-star constraints
 *
 *		x_nc, y_ij \in N,   w_ij \in R^+
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "firm_config.h"

#ifdef WITH_ILP

#include "bitset.h"
#include "raw_bitset.h"
#include "pdeq.h"

#include "irtools.h"
#include "irgwalk.h"
#include "becopyilp_t.h"
#include "beifg_t.h"
#include "besched_t.h"
#include "benodesets.h"

#define DEBUG_LVL 1

typedef struct _local_env_t {
	double time_limit;
	int first_x_var, last_x_var;
	pmap *nr_2_irn;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
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
			bitset_pos_t col;
			int cst_idx;
			const arch_register_req_t *req;
			int curr_node_color = get_irn_col(ienv->co, irn);
			int node_nr = (int)get_irn_node_nr(irn);
			local_env_t *lenv = ienv->env;

			pmap_insert(lenv->nr_2_irn, INT_TO_PTR(node_nr), irn);

			req = arch_get_register_req(ienv->co->aenv, irn, -1);

			/* get assignable colors */
			if (arch_register_req_is(req, limited)) {
				rbitset_copy_to_bitset(req->limited, colors);
			} else {
				arch_register_class_put(req->cls, colors);
				// bitset_andnot(colors, ienv->co->cenv->ignore_colors);
			}

			/* add the coloring constraint */
			cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_equal, 1.0);

			bitset_foreach(colors, col) {
				int var_idx = lpp_add_var(ienv->lp, name_cdd(buf, 'x', node_nr, col), lpp_binary, 0.0);
				lpp_set_start_value(ienv->lp, var_idx, (col == (unsigned) curr_node_color) ? 1.0 : 0.0);
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
	ir_node **clique = alloca(sizeof(*clique) * n_colors);
	int size;

	char buf[16];

	/* for each maximal clique */
	be_ifg_foreach_clique(ifg, iter, clique, &size) {
		int realsize = 0;

		for (i=0; i<size; ++i)
			if (!sr_is_removed(ienv->sr, clique[i]))
				++realsize;

		if (realsize < 2)
			continue;

		/* for all colors */
		for (col=0; col<n_colors; ++col) {
			int cst_idx = lpp_add_cst(lpp, NULL, lpp_less, 1.0);

			/* for each member of this clique */
			for (i=0; i<size; ++i) {
				ir_node *irn = clique[i];

				if (!sr_is_removed(ienv->sr, irn)) {
					int var_idx = lpp_get_var_idx(lpp, name_cdd(buf, 'x', (int)get_irn_node_nr(irn), col));
					lpp_set_factor_fast(lpp, cst_idx, var_idx, 1);
				}
			}
		}
	}
}


/**
 * TODO: Remove the dependency of the opt-units data structure
 *       by walking over all affinity edges. Graph structure
 *       does not provide this walker, yet.
 */
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
				lpp_set_factor_fast(ienv->lp, cst_idx, y_idx, -1.0);
			}
		}
	}
}

/**
 * Helping stuff for build_clique_star_cstr
 */
typedef struct _edge_t {
	ir_node *n1, *n2;
} edge_t;

static int compare_edge_t(const void *k1, const void *k2, size_t size) {
	const edge_t *e1 = k1;
	const edge_t *e2 = k2;
	(void) size;

	return ! (e1->n1 == e2->n1   &&   e1->n2 == e2->n2);
}

#define HASH_EDGE(e) (nodeset_hash((e)->n1) ^ nodeset_hash((e)->n2))

static INLINE edge_t *add_edge(set *edges, ir_node *n1, ir_node *n2, int *counter) {
	edge_t new_edge;

	if (PTR_TO_INT(n1) < PTR_TO_INT(n2)) {
		new_edge.n1 = n1;
		new_edge.n2 = n2;
	} else {
		new_edge.n1 = n2;
		new_edge.n2 = n1;
	}
	(*counter)++;
	return set_insert(edges, &new_edge, sizeof(new_edge), HASH_EDGE(&new_edge));
}

static INLINE edge_t *find_edge(set *edges, ir_node *n1, ir_node *n2) {
	edge_t new_edge;

	if (PTR_TO_INT(n1) < PTR_TO_INT(n2)) {
		new_edge.n1 = n1;
		new_edge.n2 = n2;
	} else {
		new_edge.n1 = n2;
		new_edge.n2 = n1;
	}
	return set_find(edges, &new_edge, sizeof(new_edge), HASH_EDGE(&new_edge));
}

static INLINE void remove_edge(set *edges, ir_node *n1, ir_node *n2, int *counter) {
	edge_t new_edge, *e;

	if (PTR_TO_INT(n1) < PTR_TO_INT(n2)) {
		new_edge.n1 = n1;
		new_edge.n2 = n2;
	} else {
		new_edge.n1 = n2;
		new_edge.n2 = n1;
	}
	e = set_find(edges, &new_edge, sizeof(new_edge), HASH_EDGE(&new_edge));
	if (e) {
		e->n1 = NULL;
		e->n2 = NULL;
		(*counter)--;
	}
}

#define pset_foreach(pset, irn)  for(irn=pset_first(pset); irn; irn=pset_next(pset))

/**
 * Search for an interference clique and an external node
 * with affinity edges to all nodes of the clique.
 * At most 1 node of the clique can be colored equally with the external node.
 */
static void build_clique_star_cstr(ilp_env_t *ienv) {
	affinity_node_t *aff;

	/* for each node with affinity edges */
	co_gs_foreach_aff_node(ienv->co, aff) {
		struct obstack ob;
		neighb_t *nbr;
		ir_node *center = aff->irn;
		ir_node **nodes;
		set *edges;
		int i, o, n_nodes, n_edges;

		obstack_init(&ob);
		edges = new_set(compare_edge_t, 8);

		/* get all affinity neighbours */
		n_nodes = 0;
		co_gs_foreach_neighb(aff, nbr) {
			obstack_ptr_grow(&ob, nbr->irn);
			++n_nodes;
		}
		nodes = obstack_finish(&ob);

		/* get all interference edges between these */
		n_edges = 0;
		for (i=0; i<n_nodes; ++i)
			for (o=0; o<i; ++o)
				if (be_ifg_connected(ienv->co->cenv->ifg, nodes[i], nodes[o]))
					add_edge(edges, nodes[i], nodes[o], &n_edges);

		/* cover all these interference edges with maximal cliques */
		while (n_edges) {
			edge_t *e;
			pset *clique = pset_new_ptr(8);
			int growed;

			/* get 2 starting nodes to form a clique */
			for (e=set_first(edges); !e->n1; e=set_next(edges))
				/*nothing*/ ;

			/* we could be stepped out of the loop before the set iterated to the end */
			set_break(edges);

			pset_insert_ptr(clique, e->n1);
			pset_insert_ptr(clique, e->n2);
			remove_edge(edges, e->n1, e->n2, &n_edges);

			/* while the clique is growing */
			do {
				growed = 0;

				/* search for a candidate to extend the clique */
				for (i=0; i<n_nodes; ++i) {
					ir_node *member, *cand = nodes[i];
					int is_cand;

					/* if its already in the clique try the next */
					if (pset_find_ptr(clique, cand))
						continue;

					/* are there all necessary interferences? */
					is_cand = 1;
					pset_foreach(clique, member) {
						if (!find_edge(edges, cand, member)) {
							is_cand = 0;
							pset_break(clique);
							break;
						}
					}

					/* now we know if we have a clique extender */
					if (is_cand) {
						/* first remove all covered edges */
						pset_foreach(clique, member)
							remove_edge(edges, cand, member, &n_edges);

						/* insert into clique */
						pset_insert_ptr(clique, cand);
						growed = 1;
						break;
					}
				}
			} while (growed);

			/* now the clique is maximal. Finally add the constraint */
			{
				ir_node *member;
				int var_idx, cst_idx, center_nr, member_nr;
				char buf[16];

				cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_greater, pset_count(clique)-1);
				center_nr = get_irn_node_nr(center);

				pset_foreach(clique, member) {
					member_nr = get_irn_node_nr(member);
					var_idx = lpp_get_var_idx(ienv->lp, name_cdd_sorted(buf, 'y', center_nr, member_nr));
					lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1.0);
				}
			}

			del_pset(clique);
		}

		del_set(edges);
		obstack_free(&ob, NULL);
	}
}


static void extend_path(ilp_env_t *ienv, pdeq *path, ir_node *irn) {
	be_ifg_t *ifg = ienv->co->cenv->ifg;
	int i, len;
	ir_node **curr_path;
	affinity_node_t *aff;
	neighb_t *nbr;

	/* do not walk backwards or in circles */
	if (pdeq_contains(path, irn))
		return;

	/* insert the new irn */
	pdeq_putr(path, irn);



	/* check for forbidden interferences */
	len = pdeq_len(path);
	curr_path = alloca(len * sizeof(*curr_path));
	pdeq_copyl(path, (const void **)curr_path);

	for (i=1; i<len; ++i)
		if (be_ifg_connected(ifg, irn, curr_path[i]))
			goto end;



	/* check for terminating interference */
	if (be_ifg_connected(ifg, irn, curr_path[0])) {

		/* One node is not a path. */
		/* And a path of length 2 is covered by a clique star constraint. */
		if (len > 2) {
			/* finally build the constraint */
			int cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_greater, 1.0);
			for (i=1; i<len; ++i) {
				char buf[16];
				int nr_1    = get_irn_node_nr(curr_path[i-1]);
				int nr_2    = get_irn_node_nr(curr_path[i]);
				int var_idx = lpp_get_var_idx(ienv->lp, name_cdd_sorted(buf, 'y', nr_1, nr_2));
				lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1.0);
			}
		}

		/* this path cannot be extended anymore */
		goto end;
	}



	/* recursively extend the path */
	aff = get_affinity_info(ienv->co, irn);
	co_gs_foreach_neighb(aff, nbr)
		extend_path(ienv, path, nbr->irn);


end:
	/* remove the irn */
	pdeq_getr(path);

}

/**
 *  Search a path of affinity edges, whose ends are connected
 *  by an interference edge and there are no other interference
 *  edges in between.
 *  Then at least one of these affinity edges must break.
 */
static void build_path_cstr(ilp_env_t *ienv) {
	affinity_node_t *aff_info;

	/* for each node with affinity edges */
	co_gs_foreach_aff_node(ienv->co, aff_info) {
		pdeq *path = new_pdeq();

		extend_path(ienv, path, aff_info->irn);

		del_pdeq(path);
	}
}

static void ilp2_build(ilp_env_t *ienv) {
	local_env_t *lenv = ienv->env;
	int lower_bound;

	ienv->lp = new_lpp(ienv->co->name, lpp_minimize);
	build_coloring_cstr(ienv);
	build_interference_cstr(ienv);
	build_affinity_cstr(ienv);
	build_clique_star_cstr(ienv);
	build_path_cstr(ienv);

	lower_bound = co_get_lower_bound(ienv->co) - co_get_inevit_copy_costs(ienv->co);
	lpp_set_bound(ienv->lp, lower_bound);
	lpp_set_time_limit(ienv->lp, lenv->time_limit);
}

static void ilp2_apply(ilp_env_t *ienv) {
	local_env_t *lenv = ienv->env;
	double *sol;
	lpp_sol_state_t state;
	int i, count;

	/* first check if there was sth. to optimize */
	if (lenv->first_x_var >= 0) {

		count = lenv->last_x_var - lenv->first_x_var + 1;
		sol = xmalloc(count * sizeof(sol[0]));
		state = lpp_get_solution(ienv->lp, sol, lenv->first_x_var, lenv->last_x_var);
		if (state != lpp_optimal) {
			printf("WARNING %s: Solution state is not 'optimal': %d\n", ienv->co->name, state);
			assert(state >= lpp_feasible && "The solution should at least be feasible!");
		}

		for (i=0; i<count; ++i) {
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
	}

#ifdef COPYOPT_STAT
	/* TODO adapt to multiple possible ILPs */
	copystat_add_ilp_time((int)(1000.0*lpp_get_sol_time(pi->curr_lp)));  //now we have ms
	copystat_add_ilp_vars(lpp_get_var_count(pi->curr_lp));
	copystat_add_ilp_csts(lpp_get_cst_count(pi->curr_lp));
	copystat_add_ilp_iter(lpp_get_iter_cnt(pi->curr_lp));
#endif
}

int co_solve_ilp2(copy_opt_t *co) {
	lpp_sol_state_t sol_state;
	ilp_env_t *ienv;
	local_env_t my;

	ASSERT_OU_AVAIL(co); //See build_clique_st
	ASSERT_GS_AVAIL(co);

	my.time_limit  = 0;
	my.first_x_var = -1;
	my.last_x_var  = -1;
	my.nr_2_irn    = pmap_create();
	FIRM_DBG_REGISTER(my.dbg, "firm.be.coilp2");

	ienv = new_ilp_env(co, ilp2_build, ilp2_apply, &my);

	sol_state = ilp_go(ienv);

	pmap_destroy(my.nr_2_irn);
	free_ilp_env(ienv);

	return sol_state == lpp_optimal;
}

#else /* WITH_ILP */

static INLINE void only_that_you_can_compile_without_WITH_ILP_defined(void) {
}

#endif /* WITH_ILP */
