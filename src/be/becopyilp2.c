/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       ILP based copy minimization.
 * @author      Daniel Grund
 * @date        28.02.2006
 *
 * ILP formalization using G=(V, E, Q):
 *  - 2 class of variables: coloring vars x_ic   and   equal color vars y_ij
 *  - Path constraints
 *  - Clique-star constraints
 *
 *
 * \min \sum_{ (i,j) \in Q }  w_ij y_ij
 *
 *     \sum_c x_nc           =  1           n \in N, c \in C
 *
 *     x_nc                  =  0           n \in N, c \not\in C(n)
 *
 *     \sum x_nc            <=  1           x_nc \in Clique \in AllCliques,  c \in C
 *
 *     \sum_{e \in p} y_e   >=  1           p \in P      path constraints
 *
 *     \sum_{e \in cs} y_e  >= |cs| - 1     cs \in CP    clique-star constraints
 *
 *     x_nc, y_ij \in N,   w_ij \in R^+
 */
#include "bearch.h"
#include "becopyilp_t.h"
#include "becopyopt_t.h"
#include "belive.h"
#include "bemodule.h"
#include "debug.h"
#include "irprintf.h"
#include "panic.h"
#include "pdeq.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct local_env_t {
	int first_x_var;
	int last_x_var;
} local_env_t;

static unsigned check_alignment_constraints(ir_node *node)
{
	const arch_register_req_t *req = arch_get_irn_register_req(node);
	return req->width != 1;
}

static void make_color_var_name(char *buf, size_t buf_size,
                                const ir_node *node, unsigned color)
{
	unsigned const node_idx = get_irn_idx(node);
	snprintf(buf, buf_size, "x_%u_%u", node_idx, color);
}

static void build_coloring_cstr(ilp_env_t *ienv)
{
	local_env_t    *const lenv               = (local_env_t*)ienv->env;
	be_ifg_t       *const ifg                = ienv->co->cenv->ifg;
	unsigned        const n_regs             = ienv->co->cls->n_regs;
	unsigned const *const allocatable_colors = ienv->co->cenv->allocatable_regs->data;

	unsigned *const colors = rbitset_alloca(n_regs);
	be_ifg_foreach_node(ifg, irn) {
		if (sr_is_removed(ienv, irn))
			continue;

		unsigned const has_alignment_cstr = check_alignment_constraints(irn);

		arch_register_req_t const *const req = arch_get_irn_register_req(irn);

		/* get assignable colors */
		if (req->limited) {
			rbitset_copy(colors, req->limited, n_regs);
		} else {
			rbitset_copy(colors, allocatable_colors, n_regs);
		}

		/* add the coloring constraint */
		int      const cst_idx         = lpp_add_cst(ienv->lp, NULL, lpp_equal, 1.0);
		unsigned const curr_node_color = get_irn_col(irn);
		for (unsigned col = 0; col < n_regs; ++col) {
			if (!rbitset_is_set(colors, col)
				|| (has_alignment_cstr && ((col % req->width) != 0)))
				continue;

			char buf[32];
			make_color_var_name(buf, sizeof(buf), irn, col);
			int const var_idx = lpp_add_var(ienv->lp, buf, lpp_binary, 0.0);
			lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1);

			double const val = col == curr_node_color ? 1.0 : 0.0;
			lpp_set_start_value(ienv->lp, var_idx, val);

			lenv->last_x_var = var_idx;
			if (lenv->first_x_var == -1)
				lenv->first_x_var = var_idx;
		}

		/* add register constraint constraints */
		for (unsigned col = 0; col < n_regs; ++col) {
			if (rbitset_is_set(colors, col)
				// for aligned variable, we set the unaligned part to 0
				&& (!has_alignment_cstr || ((col % req->width) == 0)))
				continue;

			char buf[32];
			make_color_var_name(buf, sizeof(buf), irn, col);
			int const cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_equal, 0.0);
			int const var_idx = lpp_add_var(ienv->lp, buf, lpp_binary, 0.0);
			lpp_set_start_value(ienv->lp, var_idx, 0.0);
			lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1);

			lenv->last_x_var = var_idx;
		}
	}
}

static void build_interference_cstr(ilp_env_t *const ienv)
{
	lpp_t          *const lpp                = ienv->lp;
	be_ifg_t       *const ifg                = ienv->co->cenv->ifg;
	unsigned        const n_colors           = ienv->co->cls->n_regs;
	ir_node       **const clique             = ALLOCAN(ir_node*, n_colors);
	unsigned const *const allocatable_colors = ienv->co->cenv->allocatable_regs->data;

	/* for each maximal clique */
	cliques_iter_t iter;
	int            size;
	be_ifg_foreach_clique(ifg, &iter, clique, &size) {
		unsigned realsize = 0;
		for (int i = 0; i < size; ++i) {
			if (!sr_is_removed(ienv, clique[i]))
				++realsize;
		}

		if (realsize < 2)
			continue;

		/* for all colors */
		for (unsigned col = 0; col < n_colors; ++col) {
			if (!rbitset_is_set(allocatable_colors, col))
				continue;

			int const cst_idx = lpp_add_cst(lpp, NULL, lpp_less_equal, 1.0);

			/* for each member of this clique */
			for (int i = 0; i < size; ++i) {
				ir_node *const irn = clique[i];
				if (sr_is_removed(ienv, irn))
					continue;

				// Use the first part of the large registers for all
				// interference, since it is the only colorable one.
				unsigned aligment_offset = 0;
				if (check_alignment_constraints(irn)) {
					arch_register_req_t const *const req = arch_get_irn_register_req(irn);
					aligment_offset = col % req->width;
				}

				char buf[32];
				make_color_var_name(buf, sizeof(buf), irn, col - aligment_offset);
				int const var_idx = lpp_get_var_idx(lpp, buf);
				lpp_set_factor_fast(lpp, cst_idx, var_idx, 1);
			}
		}
	}
}

static void make_affinity_var_name(char *buf, size_t buf_size,
                                   const ir_node *node1, const ir_node *node2)
{
	unsigned n1 = get_irn_idx(node1);
	unsigned n2 = get_irn_idx(node2);
	if (n2 < n1) {
		unsigned const t = n1;
		n1 = n2;
		n2 = t;
	}
	snprintf(buf, buf_size, "y_%u_%u", n1, n2);
}


/**
 * TODO: Remove the dependency of the opt-units data structure
 *       by walking over all affinity edges. Graph structure
 *       does not provide this walker, yet.
 */
static void build_affinity_cstr(ilp_env_t const *const ienv)
{
	lpp_t   *const lp       = ienv->lp;
	unsigned const n_colors = ienv->co->cls->n_regs;

	/* for all optimization units */
	list_for_each_entry(unit_t, curr, &ienv->co->units, units) {
		ir_node               *const root     = curr->nodes[0];
		arch_register_t const *const root_reg = arch_get_irn_register(root);
		for (int i = 1; i < curr->node_count; ++i) {
			ir_node *const arg = curr->nodes[i];
			char           buf[32];

			/* add a new affinity variable */
			make_affinity_var_name(buf, sizeof(buf), arg, root);
			int    const y_idx = lpp_add_var(lp, buf, lpp_binary, curr->costs[i]);
			double const val   = root_reg == arch_get_irn_register(arg) ? 0.0 : 1.0;
			lpp_set_start_value(lp, y_idx, val);

			/* add constraints relating the affinity var to the color vars */
			for (unsigned col = 0; col < n_colors; ++col) {
				int const cst_idx = lpp_add_cst(lp, NULL, lpp_less_equal, 0.0);

				make_color_var_name(buf, sizeof(buf), root, col);
				int const root_idx = lpp_get_var_idx(lp, buf);
				make_color_var_name(buf, sizeof(buf), arg, col);
				int const arg_idx  = lpp_get_var_idx(lp, buf);

				lpp_set_factor_fast(lp, cst_idx, root_idx,  1.0);
				lpp_set_factor_fast(lp, cst_idx, arg_idx,  -1.0);
				lpp_set_factor_fast(lp, cst_idx, y_idx,    -1.0);
			}
		}
	}
}

/**
 * Helping stuff for build_clique_star_cstr
 */
typedef struct edge_t {
	ir_node *n1;
	ir_node *n2;
} edge_t;

static int compare_edge_t(const void *k1, const void *k2, size_t size)
{
	(void)size;

	edge_t const *const e1 = (edge_t const*)k1;
	edge_t const *const e2 = (edge_t const*)k2;
	return e1->n1 != e2->n1 || e1->n2 != e2->n2;
}

static void init_edge(edge_t *const edge, ir_node *const n1, ir_node *const n2)
{
	if (n1 < n2) {
		edge->n1 = n1;
		edge->n2 = n2;
	} else {
		edge->n1 = n2;
		edge->n2 = n1;
	}
}

#define HASH_EDGE(e) (hash_irn((e)->n1) ^ hash_irn((e)->n2))

static inline edge_t *add_edge(set *edges, ir_node *n1, ir_node *n2, size_t *counter)
{
	edge_t new_edge;
	init_edge(&new_edge, n1, n2);

	++*counter;
	return set_insert(edge_t, edges, &new_edge, sizeof(new_edge), HASH_EDGE(&new_edge));
}

static inline edge_t *find_edge(set *edges, ir_node *n1, ir_node *n2)
{
	edge_t new_edge;
	init_edge(&new_edge, n1, n2);

	return set_find(edge_t, edges, &new_edge, sizeof(new_edge), HASH_EDGE(&new_edge));
}

static inline void remove_edge(set *edges, ir_node *n1, ir_node *n2, size_t *counter)
{
	edge_t new_edge;
	init_edge(&new_edge, n1, n2);

	edge_t *const e = set_find(edge_t, edges, &new_edge, sizeof(new_edge), HASH_EDGE(&new_edge));
	if (e) {
		e->n1 = NULL;
		e->n2 = NULL;
		--*counter;
	}
}

#define pset_foreach(pset, irn) foreach_pset((pset), ir_node, (irn))

/**
 * Search for an interference clique and an external node
 * with affinity edges to all nodes of the clique.
 * At most 1 node of the clique can be colored equally with the external node.
 */
static void build_clique_star_cstr(ilp_env_t *ienv)
{
	/* for each node with affinity edges */
	co_gs_foreach_aff_node(ienv->co, aff) {
		ir_node const *const center = aff->irn;
		if (arch_irn_is_ignore(center))
			continue;

		struct obstack ob;
		obstack_init(&ob);
		set *const edges = new_set(compare_edge_t, 8);

		/* get all affinity neighbours */
		int n_nodes = 0;
		co_gs_foreach_neighb(aff, nbr) {
			if (!arch_irn_is_ignore(nbr->irn)) {
				obstack_ptr_grow(&ob, nbr->irn);
				++n_nodes;
			}
		}
		ir_node **const nodes = (ir_node**)obstack_finish(&ob);

		/* get all interference edges between these */
		size_t n_edges = 0;
		for (int i = 0; i < n_nodes; ++i) {
			for (int o = 0; o < i; ++o) {
				if (be_values_interfere(nodes[i], nodes[o]))
					add_edge(edges, nodes[i], nodes[o], &n_edges);
			}
		}

		/* cover all these interference edges with maximal cliques */
		while (n_edges) {
			/* get 2 starting nodes to form a clique */
			edge_t *e;
			for (e = set_first(edge_t, edges); !e->n1; e = set_next(edge_t, edges)) {}

			/* we could be stepped out of the loop before the set iterated to the end */
			set_break(edges);

			pset *const clique = pset_new_ptr(8);
			pset_insert_ptr(clique, e->n1);
			pset_insert_ptr(clique, e->n2);
			remove_edge(edges, e->n1, e->n2, &n_edges);

			/* while the clique is growing */
			bool grew;
			do {
				grew = false;

				/* search for a candidate to extend the clique */
				for (int i = 0; i < n_nodes; ++i) {
					ir_node *const cand = nodes[i];

					/* if its already in the clique try the next */
					if (pset_find_ptr(clique, cand))
						continue;

					/* are there all necessary interferences? */
					bool is_cand = true;
					pset_foreach(clique, member) {
						if (!find_edge(edges, cand, member)) {
							is_cand = false;
							pset_break(clique);
							break;
						}
					}

					/* now we know if we have a clique extender */
					if (is_cand) {
						/* first remove all covered edges */
						pset_foreach(clique, member) {
							remove_edge(edges, cand, member, &n_edges);
						}

						/* insert into clique */
						pset_insert_ptr(clique, cand);
						grew = true;
						break;
					}
				}
			} while (grew);

			/* now the clique is maximal. Finally add the constraint */
			{
				int const cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_greater_equal, pset_count(clique)-1);
				pset_foreach(clique, member) {
					char buf[32];
					make_affinity_var_name(buf, sizeof(buf), center, member);
					int const var_idx = lpp_get_var_idx(ienv->lp, buf);
					lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1.0);
				}
			}

			del_pset(clique);
		}

		del_set(edges);
		obstack_free(&ob, NULL);
	}
}

static bool path_contains(deq_t const *const path, ir_node const *const node)
{
	deq_foreach_pointer(path, ir_node, n) {
		if (n == node)
			return true;
	}
	return false;
}

static unsigned path_len(deq_t const *const path)
{
	unsigned res = 0;
	deq_foreach_pointer(path, ir_node, n) {
		(void)n;
		++res;
	}
	return res;
}

static void extend_path(ilp_env_t *ienv, deq_t *path, const ir_node *irn)
{
	/* do not walk backwards or in circles */
	if (path_contains(path, irn))
		return;

	if (arch_irn_is_ignore(irn))
		return;

	/* insert the new irn */
	deq_push_pointer_right(path, (ir_node*)irn);

	/* check for forbidden interferences */
	int       const len       = path_len(path);
	ir_node **const curr_path = ALLOCAN(ir_node*, len);
	unsigned i = 0;
	deq_foreach_pointer(path, ir_node, n) {
		curr_path[i++] = n;
	}

	for (int i = 1; i < len; ++i) {
		if (be_values_interfere(irn, curr_path[i]))
			goto end;
	}

	/* check for terminating interference */
	if (be_values_interfere(irn, curr_path[0])) {
		/* One node is not a path. */
		/* And a path of length 2 is covered by a clique star constraint. */
		if (len > 2) {
			/* finally build the constraint */
			int cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_greater_equal, 1.0);
			for (int i = 1; i < len; ++i) {
				char buf[32];
				int  var_idx;

				make_affinity_var_name(buf, sizeof(buf), curr_path[i-1], curr_path[i]);
				var_idx = lpp_get_var_idx(ienv->lp, buf);
				lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1.0);
			}
		}

		/* this path cannot be extended anymore */
		goto end;
	}

	/* recursively extend the path */
	affinity_node_t *const aff = get_affinity_info(ienv->co, irn);
	co_gs_foreach_neighb(aff, nbr) {
		extend_path(ienv, path, nbr->irn);
	}

end:
	/* remove the irn */
	(void)deq_pop_pointer_right(ir_node, path);
}

/**
 * Search a path of affinity edges, whose ends are connected
 * by an interference edge and there are no other interference
 * edges in between.
 * Then at least one of these affinity edges must break.
 */
static void build_path_cstr(ilp_env_t *ienv)
{
	/* for each node with affinity edges */
	co_gs_foreach_aff_node(ienv->co, aff_info) {
		deq_t path;
		deq_init(&path);
		extend_path(ienv, &path, aff_info->irn);
		deq_free(&path);
	}
}

static void ilp2_build(ilp_env_t *ienv)
{
	ienv->lp = lpp_new("copyilp", lpp_minimize);
	build_coloring_cstr(ienv);
	build_interference_cstr(ienv);
	build_affinity_cstr(ienv);
	build_clique_star_cstr(ienv);
	build_path_cstr(ienv);

	int const lower_bound = co_get_lower_bound(ienv->co) - co_get_inevit_copy_costs(ienv->co);
	lpp_set_bound(ienv->lp, lower_bound);
}

static void ilp2_apply(ilp_env_t *const ienv)
{
	local_env_t *const lenv = (local_env_t*)ienv->env;

	/* first check if there was sth. to optimize */
	if (lenv->first_x_var >= 0) {
		ir_graph        *const irg   = ienv->co->irg;
		int              const count = lenv->last_x_var - lenv->first_x_var + 1;
		double          *const sol   = XMALLOCN(double, count);
		lpp_sol_state_t  const state = lpp_get_solution(ienv->lp, sol, lenv->first_x_var, lenv->last_x_var);

		if (state != lpp_optimal) {
			ir_printf("WARNING: Solution state of %F register class %s is not 'optimal': %d\n", irg, ienv->co->cls->name, (int)state);
			if (state < lpp_feasible)
				panic("copy coalescing solution not feasible");
		}

		for (int i = 0; i < count; ++i) {
			if (sol[i] <= 1 - EPSILON)
				continue;
			/* split variable name into components */
			char var_name[32];
			lpp_get_var_name(ienv->lp, lenv->first_x_var + i, var_name, sizeof(var_name));

			unsigned nodenr;
			unsigned color;
			if (sscanf(var_name, "x_%u_%u", &nodenr, &color) == 2) {
				ir_node *const irn = get_idx_irn(irg, nodenr);
				arch_set_irn_register_idx(irn, color);
			} else {
				panic("this should be an x-var");
			}
		}

		free(sol);
	}
}

/**
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 * Uses the OU and the GRAPH data structure
 * Dependency of the OU structure can be removed
 */
static int co_solve_ilp2(copy_opt_t *const co)
{
	ASSERT_OU_AVAIL(co); //See build_clique_st
	ASSERT_GS_AVAIL(co);

	FIRM_DBG_REGISTER(dbg, "firm.be.coilp2");

	local_env_t my;
	my.first_x_var = -1;
	my.last_x_var  = -1;

	ilp_env_t      *const ienv      = new_ilp_env(co, ilp2_build, ilp2_apply, &my);
	lpp_sol_state_t const sol_state = ilp_go(ienv);
	free_ilp_env(ienv);

	return sol_state == lpp_optimal;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copyilp2)
void be_init_copyilp2(void)
{
	static co_algo_info copyheur = {
		co_solve_ilp2
	};

	be_register_copyopt("ilp", &copyheur);
}
