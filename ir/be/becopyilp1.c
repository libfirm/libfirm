/**
 * Author:      Daniel Grund
 * Date:		17.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-ID:      $Id$
 *
 * ILP formalization using:
 *  - 2 classes of vars: Nodes- and optimality variables.
 *  - Clique constraints
 *  - Path constraints
 *  - Clique path constraints
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef WITH_ILP

#include "becopyilp_t.h"
#include "benumb_t.h"
#include "belive_t.h"
#include "irdom_t.h"
#include "irgwalk.h"
#include "xmalloc.h"
#include "pset.h"
#include "irprog.h"
#include "irdom_t.h"
#include "iredges_t.h"

#include "becopystat.h"
#include "besched_t.h"
#include "phiclass.h"

#if 0 //temporary

#define PATH_CONSTRAINTS_FOR_CLASSES

typedef struct _problem_instance_t {
	const copy_opt_t *co;			/**< the copy opt problem */
	size_red_t *sr;					/**< problem size reduction. removes simple nodes */
	lpp_t *lp;						/**< the linear programming problem */

	/* Helpers for maintaining indices and finding variables */
	int first_nnc_var_idx;			/**< the first index of a constraint belonging to no-null-colors stuff*/
	int cst_counter, first_x_var, last_x_var;
	char buf[32];
	pset *done;
} problem_instance_t;

#define is_color_possible(irn,color) arch_reg_is_allocatable(pi->co->aenv, irn, -1, arch_register_for_index(pi->co->cls, color))

/*
 * Some stuff for variable name handling.
 */
#define mangle_cst(buf, prefix, nr) \
			snprintf((buf), sizeof(buf), "%c%d", (prefix), (nr))

#define mangle_var1(buf, prefix, color) \
			snprintf((buf), sizeof(buf), "%c%d", (prefix), (color))

#define mangle_var2(buf, prefix, node_nr, color) \
			snprintf((buf), sizeof(buf), "%c%d_%d", (prefix), (node_nr), (color))

#define mangle_var3(buf, prefix, n1, n2, col) \
			snprintf((buf), sizeof(buf), "%c%d_%d_%d", (prefix), (n1), (n2), (col))

#define mangle_var_irn(buf, prefix, irn, color) \
			mangle_var2((buf), (prefix), get_irn_graph_nr(irn), (color))

#define split_var(var, nnr, col) \
			sscanf(var, "x%d_%d", (nnr), (col))


#ifndef PATH_CONSTRAINTS_FOR_CLASSES
/**
 * Matrix P: Path contraints.
 * If 2 nodes interfere and there is a path of equal-color-edges
 * connecting them, then at least one of those equal-color-edges
 * will break and cause some costs.
 */
static void pi_add_path_cstr(problem_instance_t *pi) {
	unit_t *curr;
	int cst_counter = 0;
	DBG((dbg, LEVEL_2, "Adding path constraints...\n"));

	/* for all optimization units (only phis) */
	list_for_each_entry(unit_t, curr, &pi->co->units, units) {
		int i, o, rootnr;

		if (curr->min_nodes_costs == 0)
			continue;

		rootnr = get_irn_graph_nr(curr->nodes[0]);
		/* check all argument pairs for interference */
		for (i=1; i<curr->node_count; ++i) {
			const ir_node *arg1 = curr->nodes[i];
			int arg1nr = get_irn_graph_nr(arg1);
			for (o=i+1; o<curr->node_count; ++o) {
				const ir_node *arg2 = curr->nodes[o];
				int arg2nr = get_irn_graph_nr(arg2);
				if (nodes_interfere(pi->co->cenv, arg1, arg2)) {
					int cst_idx, y_idx;
					char buf[32];

					mangle_cst(buf, 'P', cst_counter++);
					cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_greater, 1);

					mangle_var2(buf, 'y', rootnr, arg1nr);
					y_idx = lpp_get_var_idx(pi->curr_lp, buf);
					lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, 1);

					mangle_var2(buf, 'y', rootnr, arg2nr);
					y_idx = lpp_get_var_idx(pi->curr_lp, buf);
					lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, 1);
				}
			}
		}
	}
}
#endif

#ifdef PATH_CONSTRAINTS_FOR_CLASSES
static INLINE int get_y_var_idx(problem_instance_t *pi, int nnr1, int nnr2) {
	int res;
	char buf[30];

	mangle_var2(buf, 'y', nnr1, nnr2);
	if ((res = lpp_get_var_idx(pi->curr_lp, buf)) != -1)
		return res;

	mangle_var2(buf, 'y', nnr2, nnr1);
	if ((res = lpp_get_var_idx(pi->curr_lp, buf)) != -1)
		return res;

	assert(0 && "One of them must work");
  return -1;
}

static void check_ecc_and_add_cut(problem_instance_t *pi, ir_node **path, int length, pset *remain, ir_node *tgt) {
	if (path[length-1] == tgt) { /* we found a path */
		int cst_idx, var_idx, i, nnr1, nnr2;
		char buf[30];

		/* add cut to ilp */
		mangle_cst(buf, 'Q', pi->cst_counter++);
		cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_greater, 1);

		/* add all vars along the path */
		nnr2 = get_irn_graph_nr(path[0]);
		for (i=1; i<length; ++i) {
			nnr1 = nnr2;
			nnr2 = get_irn_graph_nr(path[i]);
			var_idx = get_y_var_idx(pi, nnr1, nnr2);
			lpp_set_factor_fast(pi->curr_lp, cst_idx, var_idx, 1);
		}
	} else { /* try to extend the path */
		be_chordal_env_t *cenv = pi->co->cenv;
		const ir_edge_t *edge;
		ir_node *end = path[length-1];
		ir_node **next = alloca(pset_count(remain) * sizeof(*next));
		int i, o, max, next_pos = 0;
		pset *done = pset_new_ptr_default();

		/* find all potential next nodes on path */
		/*  args of phis */
		if (is_Phi(end))
			for(i=0, max=get_irn_arity(end); i<max; ++i) {
				ir_node *arg = get_irn_n(end, i);
				if (!pset_find_ptr(done, arg) && pset_find_ptr(remain, arg)) {
					next[next_pos++] = arg;
					pset_insert_ptr(done, arg);
				}
			}
		/*  outs of phis and other nodes */
		foreach_out_edge(end, edge) {
			ir_node *user = edge->src;
			if (is_Phi(user) && !pset_find_ptr(done, user) && pset_find_ptr(remain, user)) {
				next[next_pos++] = user;
				pset_insert_ptr(done, user);
			}
		}
		del_pset(done);


		/* delete all potential nodes with interferences to other nodes in the path */
		for (i=0; i<next_pos; ++i) {
			ir_node *nn = next[i];

			/* if next is the tgt, it may interfere with path[0],
			 * so skip the first check */
			o = (nn == tgt && length > 1) ? 1 : 0;

			for(; o<length; ++o)
				if (nodes_interfere(cenv, nn, path[o])) {
					next[i] = NULL;
					break;
				}
		}
		/* now we have all possible nodes in next; impossibles are NULL */

		/* try to finish path with all possible nodes */
		for (i=0; i<next_pos; ++i) {
			if (!next[i]) /* this was an impossible node */
				continue;

			path[length] = next[i];
			pset_remove_ptr(remain, next[i]);
			check_ecc_and_add_cut(pi, path, length+1, remain, tgt);
			pset_insert_ptr(remain, next[i]);
		}
	}
}

static void path_cstr_for_classes_walker(ir_node *irn, void *env) {
	problem_instance_t *pi = env;
	be_chordal_env_t *cenv;
	int i, o, max;
	ir_node *m, **cls;
	pset *class = get_phi_class(irn);
	if (!class || pset_find_ptr(pi->done, class))
		return;

	pset_insert_ptr(pi->done, class);

	/* pset to array */
	max = pset_count(class);
	cls = alloca(max * sizeof(*cls));
	for(i=0, m = pset_first(class); m; i++, m = pset_next(class)) {
		DBG((dbg, LEVEL_1, " class member: %+F\n", m));
		cls[i] = m;
	}

	cenv = pi->co->cenv;
	for(i=0; i<max; ++i) {
		ir_node **path = alloca(max * sizeof(*path));
		pset *remain = pset_new_ptr(8);
		pset_insert_pset_ptr(remain, class);

		/* add cls[i] to path and remove it from remainder */
		path[0] = cls[i];
		pset_remove_ptr(remain, cls[i]);

		for(o=i+1; o<max; ++o)
			if (nodes_interfere(cenv, cls[i], cls[o]))
				check_ecc_and_add_cut(pi, path, 1, remain, cls[o]);

		/* insert back into remainder */
		pset_insert_ptr(remain, cls[i]);
	}
}


/**
 * Matrix P: Path contraints.
 * If 2 nodes interfere and there is a path of equal-color-edges
 * connecting them, then at least one of those equal-color-edges
 * will break and cause some costs.
 */
static void pi_add_path_cstr_for_classes(problem_instance_t *pi) {
	DBG((dbg, LEVEL_2, "Adding path constraints for phi classes...\n"));
	pi->cst_counter = 0;
	pi->done = pset_new_ptr_default();
	irg_walk_graph(pi->co->irg, path_cstr_for_classes_walker, NULL, pi);
	del_pset(pi->done);
}
#endif

static void pi_construct(problem_instance_t *pi) {
	pi_add_path_cstr_for_classes(pi);
	pi_add_path_cstr(pi);
	pi_add_clique_path_cstr(pi);
}
#endif

#include "becopyilp_t.h"

#define DEBUG_LVL 1

typedef struct _my_env_t {
	int foo;
} my_env_t;


static void ilp1_build(ilp_env_t *ienv) {
	ienv->lp = new_lpp(ienv->co->name, lpp_minimize);

}

static void ilp1_apply(ilp_env_t *ienv) {

}

int co_solve_ilp1(copy_opt_t *co, double time_limit) {
	return 1;
}


#else /* WITH_ILP */

static void only_that_you_can_compile_without_WITH_ILP_defined(void) {
}

#endif /* WITH_ILP */
