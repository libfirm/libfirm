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
#include "be_t.h"
#include "belive.h"
#include "beirg.h"
#include "panic.h"
#include "raw_bitset.h"
#include "pdeq.h"

#include "util.h"
#include "irgwalk.h"
#include "becopyilp_t.h"
#include "beifg.h"
#include "besched.h"
#include "bemodule.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct local_env_t {
	int             first_x_var;
	int             last_x_var;
	const unsigned *allocatable_colors;
} local_env_t;

static unsigned check_alignment_constraints(ir_node *node)
{
	const arch_register_req_t *req = arch_get_irn_register_req(node);
	// For larger than 1 variables, support only aligned constraints
	assert((arch_register_req_is(req, aligned) || req->width == 1) && "Unaligned large (width > 1) variables not supported");
	return arch_register_req_is(req, aligned) && req->width > 1;
}

static void make_color_var_name(char *buf, size_t buf_size,
                                const ir_node *node, unsigned color)
{
	unsigned node_idx = get_irn_idx(node);
	snprintf(buf, buf_size, "x_%u_%u", node_idx, color);
}

static void build_coloring_cstr(ilp_env_t *ienv)
{
	local_env_t    *lenv   = (local_env_t*)ienv->env;
	be_ifg_t       *ifg    = ienv->co->cenv->ifg;
	unsigned        n_regs = arch_register_class_n_regs(ienv->co->cls);
	const unsigned *allocatable_colors = lenv->allocatable_colors;
	char            buf[32];

	unsigned *const colors = rbitset_alloca(n_regs);
	be_ifg_foreach_node(ifg, irn) {
		const arch_register_req_t *req;
		unsigned                   col;
		int                        cst_idx;
		unsigned                   curr_node_color;
		unsigned                   has_alignment_cstr;

		if (sr_is_removed(ienv, irn))
			continue;

		has_alignment_cstr = check_alignment_constraints(irn);

		req = arch_get_irn_register_req(irn);

		/* get assignable colors */
		if (arch_register_req_is(req, limited)) {
			rbitset_copy(colors, req->limited, n_regs);
		} else {
			rbitset_copy(colors, allocatable_colors, n_regs);
		}

		/* add the coloring constraint */
		cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_equal, 1.0);

		curr_node_color = get_irn_col(irn);
		for (col = 0; col < n_regs; ++col) {
			int    var_idx;
			double val;
			if (!rbitset_is_set(colors, col)
				|| (has_alignment_cstr && ((col % req->width) != 0)))
				continue;

			make_color_var_name(buf, sizeof(buf), irn, col);
			var_idx = lpp_add_var(ienv->lp, buf, lpp_binary, 0.0);
			lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1);

			val = (col == curr_node_color) ? 1.0 : 0.0;
			lpp_set_start_value(ienv->lp, var_idx, val);

			lenv->last_x_var = var_idx;
			if (lenv->first_x_var == -1)
				lenv->first_x_var = var_idx;
		}

		/* add register constraint constraints */
		for (col = 0; col < n_regs; ++col) {
			int cst_idx;
			int var_idx;
			if (rbitset_is_set(colors, col)
				// for aligned variable, we set the unaligned part to 0
				&& (!has_alignment_cstr || ((col % req->width) == 0)))
				continue;

			make_color_var_name(buf, sizeof(buf), irn, col);
			cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_equal, 0.0);
			var_idx = lpp_add_var(ienv->lp, buf, lpp_binary, 0.0);
			lpp_set_start_value(ienv->lp, var_idx, 0.0);
			lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1);

			lenv->last_x_var = var_idx;
		}
	}
}

static void build_interference_cstr(ilp_env_t *ienv)
{
	lpp_t          *lpp      = ienv->lp;
	local_env_t    *lenv     = (local_env_t*)ienv->env;
	be_ifg_t       *ifg      = ienv->co->cenv->ifg;
	unsigned        n_colors = arch_register_class_n_regs(ienv->co->cls);
	ir_node       **clique   = ALLOCAN(ir_node*, n_colors);
	const unsigned *allocatable_colors = lenv->allocatable_colors;
	cliques_iter_t iter;
	int            size;
	unsigned       col;
	int            i;

	/* for each maximal clique */
	be_ifg_foreach_clique(ifg, &iter, clique, &size) {
		unsigned realsize = 0;

		for (i=0; i<size; ++i) {
			if (!sr_is_removed(ienv, clique[i]))
				++realsize;
		}

		if (realsize < 2)
			continue;

		/* for all colors */
		for (col=0; col<n_colors; ++col) {
			int cst_idx;
			if (!rbitset_is_set(allocatable_colors, col))
				continue;

			cst_idx = lpp_add_cst(lpp, NULL, lpp_less_equal, 1.0);

			/* for each member of this clique */
			for (i=0; i<size; ++i) {
				ir_node *irn = clique[i];
				char     buf[32];
				int      var_idx;
				unsigned aligment_offset = 0;

				if (sr_is_removed(ienv, irn))
					continue;

				// Use the first part of the large registers for all
				// interference, since it is the only colorable one.
				if (check_alignment_constraints(irn)) {
					const arch_register_req_t *req
						= arch_get_irn_register_req(irn);
					aligment_offset = col % req->width;
				}

				make_color_var_name(buf, sizeof(buf), irn,
									col - aligment_offset);
				var_idx = lpp_get_var_idx(lpp, buf);
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
	if (n1 < n2) {
		snprintf(buf, buf_size, "y_%u_%u", n1, n2);
	} else {
		snprintf(buf, buf_size, "y_%u_%u", n2, n1);
	}
}


/**
 * TODO: Remove the dependency of the opt-units data structure
 *       by walking over all affinity edges. Graph structure
 *       does not provide this walker, yet.
 */
static void build_affinity_cstr(ilp_env_t *ienv)
{
	unsigned  n_colors = arch_register_class_n_regs(ienv->co->cls);

	/* for all optimization units */
	list_for_each_entry(unit_t, curr, &ienv->co->units, units) {
		ir_node *root     = curr->nodes[0];
		unsigned root_col = get_irn_col(root);
		int      i;

		for (i = 1; i < curr->node_count; ++i) {
			ir_node *arg     = curr->nodes[i];
			unsigned arg_col = get_irn_col(arg);
			double   val;
			char     buf[32];
			unsigned col;
			int      y_idx;

			/* add a new affinity variable */
			make_affinity_var_name(buf, sizeof(buf), arg, root);
			y_idx = lpp_add_var(ienv->lp, buf, lpp_binary, curr->costs[i]);
			val   = (root_col == arg_col) ? 0.0 : 1.0;
			lpp_set_start_value(ienv->lp, y_idx, val);

			/* add constraints relating the affinity var to the color vars */
			for (col=0; col<n_colors; ++col) {
				int cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_less_equal, 0.0);
				int root_idx;
				int arg_idx;

				make_color_var_name(buf, sizeof(buf), root, col);
				root_idx = lpp_get_var_idx(ienv->lp, buf);
				make_color_var_name(buf, sizeof(buf), arg, col);
				arg_idx  = lpp_get_var_idx(ienv->lp, buf);

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
typedef struct edge_t {
	ir_node *n1;
	ir_node *n2;
} edge_t;

static int compare_edge_t(const void *k1, const void *k2, size_t size)
{
	const edge_t *e1 = (const edge_t*)k1;
	const edge_t *e2 = (const edge_t*)k2;
	(void) size;

	return ! (e1->n1 == e2->n1 && e1->n2 == e2->n2);
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

	(*counter)++;
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
		(*counter)--;
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
		struct obstack ob;
		const ir_node *center = aff->irn;
		ir_node **nodes;
		set *edges;
		int i, o, n_nodes;
		size_t n_edges;

		if (arch_irn_is_ignore(aff->irn))
			continue;

		obstack_init(&ob);
		edges = new_set(compare_edge_t, 8);

		/* get all affinity neighbours */
		n_nodes = 0;
		co_gs_foreach_neighb(aff, nbr) {
			if (!arch_irn_is_ignore(nbr->irn)) {
				obstack_ptr_grow(&ob, nbr->irn);
				++n_nodes;
			}
		}
		nodes = (ir_node**)obstack_finish(&ob);

		/* get all interference edges between these */
		n_edges = 0;
		for (i=0; i<n_nodes; ++i) {
			for (o=0; o<i; ++o) {
				if (be_values_interfere(nodes[i], nodes[o]))
					add_edge(edges, nodes[i], nodes[o], &n_edges);
			}
		}

		/* cover all these interference edges with maximal cliques */
		while (n_edges) {
			edge_t *e;
			pset   *clique = pset_new_ptr(8);
			bool    growed;

			/* get 2 starting nodes to form a clique */
			for (e = set_first(edge_t, edges); !e->n1; e = set_next(edge_t, edges)) {}

			/* we could be stepped out of the loop before the set iterated to the end */
			set_break(edges);

			pset_insert_ptr(clique, e->n1);
			pset_insert_ptr(clique, e->n2);
			remove_edge(edges, e->n1, e->n2, &n_edges);

			/* while the clique is growing */
			do {
				growed = false;

				/* search for a candidate to extend the clique */
				for (i=0; i<n_nodes; ++i) {
					ir_node *cand = nodes[i];
					bool     is_cand;

					/* if its already in the clique try the next */
					if (pset_find_ptr(clique, cand))
						continue;

					/* are there all necessary interferences? */
					is_cand = true;
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
						pset_foreach(clique, member)
							remove_edge(edges, cand, member, &n_edges);

						/* insert into clique */
						pset_insert_ptr(clique, cand);
						growed = true;
						break;
					}
				}
			} while (growed);

			/* now the clique is maximal. Finally add the constraint */
			{
				int  var_idx;
				int  cst_idx;
				char buf[32];

				cst_idx = lpp_add_cst(ienv->lp, NULL, lpp_greater_equal, pset_count(clique)-1);

				pset_foreach(clique, member) {
					make_affinity_var_name(buf, sizeof(buf), center, member);
					var_idx = lpp_get_var_idx(ienv->lp, buf);
					lpp_set_factor_fast(ienv->lp, cst_idx, var_idx, 1.0);
				}
			}

			del_pset(clique);
		}

		del_set(edges);
		obstack_free(&ob, NULL);
	}
}

static void extend_path(ilp_env_t *ienv, pdeq *path, const ir_node *irn)
{
	int i, len;
	ir_node **curr_path;
	affinity_node_t *aff;

	/* do not walk backwards or in circles */
	if (pdeq_contains(path, irn))
		return;

	if (arch_irn_is_ignore(irn))
		return;

	/* insert the new irn */
	pdeq_putr(path, irn);

	/* check for forbidden interferences */
	len       = pdeq_len(path);
	curr_path = ALLOCAN(ir_node*, len);
	pdeq_copyl(path, (const void **)curr_path);

	for (i=1; i<len; ++i) {
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
			for (i=1; i<len; ++i) {
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
	aff = get_affinity_info(ienv->co, irn);
	co_gs_foreach_neighb(aff, nbr) {
		extend_path(ienv, path, nbr->irn);
	}

end:
	/* remove the irn */
	pdeq_getr(path);
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
		pdeq *path = new_pdeq();

		extend_path(ienv, path, aff_info->irn);

		del_pdeq(path);
	}
}

static void ilp2_build(ilp_env_t *ienv)
{
	int lower_bound;

	ienv->lp = lpp_new("copyilp", lpp_minimize);
	build_coloring_cstr(ienv);
	build_interference_cstr(ienv);
	build_affinity_cstr(ienv);
	build_clique_star_cstr(ienv);
	build_path_cstr(ienv);

	lower_bound = co_get_lower_bound(ienv->co) - co_get_inevit_copy_costs(ienv->co);
	lpp_set_bound(ienv->lp, lower_bound);
}

static void ilp2_apply(ilp_env_t *ienv)
{
	local_env_t *lenv = (local_env_t*)ienv->env;
	ir_graph    *irg  = ienv->co->irg;

	/* first check if there was sth. to optimize */
	if (lenv->first_x_var >= 0) {
		int              count = lenv->last_x_var - lenv->first_x_var + 1;
		double          *sol   = XMALLOCN(double, count);
		lpp_sol_state_t  state = lpp_get_solution(ienv->lp, sol, lenv->first_x_var, lenv->last_x_var);
		int              i;

		if (state != lpp_optimal) {
			ir_printf("WARNING: Solution state of %F register class %s is not 'optimal': %d\n", ienv->co->irg, ienv->co->cls->name, (int)state);
			if (state < lpp_feasible) {
				panic("Copy coalescing solution not feasible!");
			}
		}

		for (i=0; i<count; ++i) {
			unsigned nodenr;
			unsigned color;
			char     var_name[32];
			if (sol[i] <= 1-EPSILON)
				continue;
			/* split variable name into components */
			lpp_get_var_name(ienv->lp, lenv->first_x_var+i, var_name, sizeof(var_name));

			if (sscanf(var_name, "x_%u_%u", &nodenr, &color) == 2) {
				ir_node *irn = get_idx_irn(irg, nodenr);
				set_irn_col(ienv->co->cls, irn, color);
			} else {
				panic("This should be a x-var");
			}
		}

		free(sol);
	}

#ifdef COPYOPT_STAT
	/* TODO adapt to multiple possible ILPs */
	copystat_add_ilp_time((int)(1000.0*lpp_get_sol_time(pi->curr_lp)));  //now we have ms
	copystat_add_ilp_vars(lpp_get_var_count(pi->curr_lp));
	copystat_add_ilp_csts(lpp_get_cst_count(pi->curr_lp));
	copystat_add_ilp_iter(lpp_get_iter_cnt(pi->curr_lp));
#endif
}

/**
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 * Uses the OU and the GRAPH data structure
 * Dependency of the OU structure can be removed
 */
static int co_solve_ilp2(copy_opt_t *co)
{
	ASSERT_OU_AVAIL(co); //See build_clique_st
	ASSERT_GS_AVAIL(co);

	FIRM_DBG_REGISTER(dbg, "firm.be.coilp2");

	local_env_t my;
	my.first_x_var        = -1;
	my.last_x_var         = -1;
	my.allocatable_colors = co->cenv->allocatable_regs->data;

	ilp_env_t      *const ienv      = new_ilp_env(co, ilp2_build, ilp2_apply, &my);
	lpp_sol_state_t const sol_state = ilp_go(ienv);
	free_ilp_env(ienv);

	return sol_state == lpp_optimal;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copyilp2)
void be_init_copyilp2(void)
{
	static co_algo_info copyheur = {
		co_solve_ilp2, 1
	};

	be_register_copyopt("ilp", &copyheur);
}
