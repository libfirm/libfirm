/**
 * Author:      Daniel Grund
 * Date:		17.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-ID:      $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#define PATH_CONSTRAINTS_FOR_CLASSES
#undef PRECOLOR_MAX_CLIQUE
#undef NO_NULL_COLORS
#undef NO_NULL_COLORS_EXTRA_CSTS
#undef NO_NULL_COLORS_WITH_COSTS
#if (defined(NO_NULL_COLORS_EXTRA_CSTS) || defined(NO_NULL_COLORS_WITH_COSTS)) && !defined(NO_NULL_COLORS)
#error Chose your weapon!
#endif

#include "irprog.h"

#include <lpp/lpp.h>
#include <lpp/lpp_net.h>
#include <lpp/lpp_cplex.h>
#include <lpp/lpp_remote.h>
#include "xmalloc.h"
#include "pset.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "bechordal_t.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "besched_t.h"
#include "phiclass.h"

#define LPP_HOST "i44pc52"
#define LPP_SOLVER "cplex"

#undef DUMP_MPS
static firm_dbg_module_t *dbg = NULL;

#define MAX(a,b) ((a<b)?(b):(a))
#define MIN(a,b) ((a<b)?(a):(b))
#define EPSILON 0.00001
#define SLOTS_LIVING 32

typedef struct _simpl_t {
	struct list_head chain;
	ir_node *irn;
} simpl_t;

typedef struct _problem_instance_t {
	const copy_opt_t *co;			/** the copy_opt problem */
	/* problem size reduction removing simple nodes */
	struct list_head simplicials;	/**< holds all simpl_t's in right order to color*/
	pset *removed;					/**< holds all removed simplicial irns */
	/* lp problem */
	lpp_t *curr_lp;					/**< points to the problem currently used */
	lpp_t *dilp;					/**< problem formulation directly as milp */
#ifdef NO_NULL_COLORS_EXTRA_CSTS
	int first_nnc_cst_idx;			/**< the first index of a constraint belonging to no-null-colors stuff*/
#endif
	int first_nnc_var_idx;			/**< the first index of a constraint belonging to no-null-colors stuff*/

	int cst_counter, first_x_var, last_x_var;
	char buf[32];
	int all_simplicial;
	pset *done;
} problem_instance_t;

#define is_removed(irn) pset_find_ptr(pi->removed, irn)

#define is_color_possible(irn,color) arch_reg_is_allocatable(get_arch_env(pi->co), irn, -1, arch_register_for_index(pi->co->chordal_env->cls, color))

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


/**
 * Checks if a node is simplicial in the graph
 * heeding the already removed nodes.
 */
static INLINE int pi_is_simplicial(problem_instance_t *pi, const ir_node *ifn) {
	int i, o, size = 0;
	ir_node **all, *curr;
	be_ifg_t *ifg = pi->co->chordal_env->ifg;
	void *iter = be_ifg_iter_alloca(ifg);

	all = alloca(be_ifg_degree(ifg, ifn) * sizeof(*all));

	/* get all non-removed neighbors */
	be_ifg_foreach_neighbour(ifg, iter, ifn, curr)
		if (!is_removed(curr))
			all[size++] = curr;

	/* check if these form a clique */
	for (i=0; i<size; ++i)
		for (o=i+1; o<size; ++o)
			if (!be_ifg_connected(ifg, all[i], all[o]))
				return 0;

	/* all edges exist so this is a clique */
	return 1;
}

static int irn_cmp(const void *a, const void *b, size_t n)
{
	return a != b;
}

/**
 * Iterative finds and 'removes' from the graph all nodes which are
 * simplicial AND not member of a equal-color-wish
 */
static void pi_find_simplicials(problem_instance_t *pi) {
	ir_node *irn;
	int redo = 1;
	int n_nodes = 0;
	const be_ifg_t *ifg = pi->co->chordal_env->ifg;
	void *iter = be_ifg_iter_alloca(ifg);

	DBG((dbg, LEVEL_2, "Find simlicials...\n"));

	while (redo) {
		redo = 0;
		be_ifg_foreach_node(ifg, iter, irn) {
			if (!is_removed(irn) && !is_optimizable(get_arch_env(pi->co), irn) && !is_optimizable_arg(pi->co, irn)) {
          	 	if (pi_is_simplicial(pi, irn)) {
					simpl_t *s = xmalloc(sizeof(*s));
					s->irn = irn;
					list_add(&s->chain, &pi->simplicials);
					pset_insert_ptr(pi->removed, irn);
					redo = 1;
					DBG((dbg, LEVEL_2, " Removed %+F\n", irn));
          	 	}
			}
		}
	}

	/* TODO: Count inside the last look */
	be_ifg_foreach_node(ifg, iter, irn) {
		n_nodes++;
	}

	if (n_nodes == pset_count(pi->removed))
		pi->all_simplicial = 1;
}

#ifdef NO_NULL_COLORS
static void pi_add_constr_no_null_colors(problem_instance_t *pi) {
	int cst_counter=0, col, var_idx, cst_idx;
	int n_colors = pi->co->chordal_env->cls->n_regs;
	char buf[40];

	for (col = 0; col < n_colors; ++col) {
		mangle_var1(buf, 'u', col);
#ifdef NO_NULL_COLORS_WITH_COSTS
		var_idx = lpp_add_var(pi->curr_lp, buf, lpp_binary, 1.0 / (double) (1 << (col+1)) );
#else
		var_idx = lpp_add_var(pi->curr_lp, buf, lpp_binary, 1.0 / (2.0 * n_colors) );
#endif
		if (!pi->first_nnc_var_idx)
			pi->first_nnc_var_idx = var_idx;
	}

#ifdef NO_NULL_COLORS_EXTRA_CSTS
	for (col = 0; col < n_colors; ++col) {
		mangle_cst(buf, 'U', cst_counter++);
		cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_greater, 0);
		if (!pi->first_nnc_cst_idx)
			pi->first_nnc_cst_idx = cst_idx;
		lpp_set_factor_fast(pi->curr_lp, cst_idx, pi->first_nnc_var_idx+col, -1);
	}
#endif

#ifndef NO_NULL_COLORS_WITH_COSTS
	for (col = 0; col < n_colors - 1; ++col) {
		mangle_cst(buf, 'U', cst_counter++);
		cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_greater, 0);
		lpp_set_factor_fast(pi->curr_lp, cst_idx, pi->first_nnc_var_idx+col  ,  1);
		lpp_set_factor_fast(pi->curr_lp, cst_idx, pi->first_nnc_var_idx+col+1, -1);
	}
#endif

}
#endif

/**
 * Add coloring-force conditions
 * Matrix A: knapsack constraint for each node
 */
static void pi_add_constr_A(problem_instance_t *pi) {
	pmap_entry *pme;

	DBG((dbg, LEVEL_2, "Add A constraints...\n"));
	/* iterate over all blocks */
	pmap_foreach(pi->co->chordal_env->border_heads, pme) {
		struct list_head *head = pme->value;
		border_t *curr;
		bitset_t *pos_regs = bitset_alloca(pi->co->chordal_env->cls->n_regs);

		list_for_each_entry_reverse(border_t, curr, head, list)
			if (curr->is_def && curr->is_real && !is_removed(curr->irn)) {
				int cst_idx, nnr, col;

				nnr = get_irn_graph_nr(curr->irn);
				mangle_cst(pi->buf, 'A', nnr);
				cst_idx = lpp_add_cst(pi->curr_lp, pi->buf, lpp_equal, 1);

				/* iterate over all possible colors in order */
				bitset_clear_all(pos_regs);
				arch_get_allocatable_regs(get_arch_env(pi->co), curr->irn, -1, pi->co->chordal_env->cls, pos_regs);
				bitset_foreach(pos_regs, col) {
					int var_idx;
					mangle_var2(pi->buf, 'x', nnr, col);
					var_idx = lpp_add_var(pi->curr_lp, pi->buf, lpp_binary, 0);
					if (!pi->first_x_var)
						pi->first_x_var = var_idx;
					pi->last_x_var = var_idx;
					lpp_set_factor_fast(pi->curr_lp, cst_idx, var_idx, 1);
#ifdef NO_NULL_COLORS_EXTRA_CSTS
					lpp_set_factor_fast(pi->curr_lp, pi->first_nnc_cst_idx+col, var_idx, 1);
#endif
				}
			}
	}
}

/**
 * Checks if all nodes in @p living are live in in block @p block.
 * @return 1 if all are live in
 *         0 else
 */
static INLINE int all_live_in(ir_node *block, pset *living) {
	ir_node *n;
	for (n = pset_first(living); n; n = pset_next(living))
		if (!is_live_in(block, n)) {
			pset_break(living);
			return 0;
		}
	return 1;
}

/**
 * Finds cliques in the interference graph, considering only nodes
 * for which the color @p color is possible. Finds only 'maximal-cliques',
 * viz cliques which are not contained in another one.
 * Matrix B: interference constraints using cliques
 */
static void pi_add_constr_B(problem_instance_t *pi, int color) {
	enum phase_t {growing, shrinking} phase = growing;
	border_t *b;
	pmap_entry *pme;
	pset *living = pset_new_ptr(SLOTS_LIVING);

	DBG((dbg, LEVEL_2, "Add B constraints (col = %d)...\n", color));
	/* iterate over all blocks */
	pmap_foreach(pi->co->chordal_env->border_heads, pme) {
		ir_node *block = pme->key;
		struct list_head *head = pme->value;

		list_for_each_entry_reverse(border_t, b, head, list) {
			const ir_node *irn = b->irn;
			if (is_removed(irn) || !is_color_possible(irn, color))
				continue;

			if (b->is_def) {
				DBG((dbg, LEVEL_2, "Def %n\n", irn));
				pset_insert_ptr(living, irn);
				phase = growing;
			} else { /* is_use */
				DBG((dbg, LEVEL_2, "Use %n\n", irn));

				/* before shrinking the set, store the current 'maximum' clique;
				 * do NOT if clique is a single node
				 * do NOT if all values are live_in (in this case they were contained in a live-out clique elsewhere) */
				if (phase == growing && pset_count(living) >= 2 && !all_live_in(block, living)) {
					int cst_idx;
					ir_node *n;
					mangle_cst(pi->buf, 'B', pi->cst_counter);
#ifdef NO_NULL_COLORS
					cst_idx = lpp_add_cst(pi->curr_lp, pi->buf, lpp_less, 0);
#else
					cst_idx = lpp_add_cst(pi->curr_lp, pi->buf, lpp_less, 1);
#endif
					for (n = pset_first(living); n; n = pset_next(living)) {
						int var_idx;
						mangle_var_irn(pi->buf, 'x', n, color);
						var_idx = lpp_get_var_idx(pi->curr_lp, pi->buf);
						lpp_set_factor_fast(pi->curr_lp, cst_idx, var_idx, 1);
					}
#ifdef NO_NULL_COLORS
					lpp_set_factor_fast(pi->curr_lp, cst_idx, pi->first_nnc_var_idx+color, -1.0);
#endif
					pi->cst_counter++;
				}
				pset_remove_ptr(living, irn);
				phase = shrinking;
			}
		}
	}
	assert(0 == pset_count(living));
	del_pset(living);
}

/**
 * Generates constraints which interrelate x with y variables.
 * x1 and x2 have the different colors ==> y_12 = 1
 */
static void pi_add_constr_E(problem_instance_t *pi) {
	unit_t *curr;
	bitset_t *root_regs, *arg_regs, *work_regs;
	int cst_counter = 0;
	unsigned nregs = pi->co->chordal_env->cls->n_regs;
	root_regs = bitset_alloca(nregs);
	arg_regs = bitset_alloca(nregs);
	work_regs = bitset_alloca(nregs);

	DBG((dbg, LEVEL_2, "Add E constraints...\n"));
	/* for all roots of optimization units */
	list_for_each_entry(unit_t, curr, &pi->co->units, units) {
		ir_node *root, *arg;
		int rootnr, argnr, color;
		int y_idx, i;
		char buf[32];

		root = curr->nodes[0];
		rootnr = get_irn_graph_nr(root);
		bitset_clear_all(root_regs);
		arch_get_allocatable_regs(get_arch_env(pi->co), root, -1, pi->co->chordal_env->cls, root_regs);

		/* for all arguments of root */
		for (i = 1; i < curr->node_count; ++i) {
			arg = curr->nodes[i];
			argnr = get_irn_graph_nr(arg);
			bitset_clear_all(arg_regs);
			arch_get_allocatable_regs(get_arch_env(pi->co), arg, -1, pi->co->chordal_env->cls, arg_regs);

			/* Introduce new variable and set factor in objective function */
			mangle_var2(buf, 'y', rootnr, argnr);
			y_idx = lpp_add_var(pi->curr_lp, buf, lpp_binary, curr->costs[i]);

			/* set starting value */
			lpp_set_start_value(pi->curr_lp, y_idx, (get_irn_col(pi->co, root) != get_irn_col(pi->co, arg)));

			/* For all colors root and arg have in common, add 2 constraints to E */
			bitset_copy(work_regs, root_regs);
			bitset_and(work_regs, arg_regs);
			bitset_foreach(work_regs, color) {
				int root_idx, arg_idx, cst_idx;
				mangle_var2(buf, 'x', rootnr, color);
				root_idx = lpp_get_var_idx(pi->curr_lp, buf);
				mangle_var2(buf, 'x', argnr, color);
				arg_idx = lpp_get_var_idx(pi->curr_lp, buf);

				/* add root-arg-y <= 0 */
				mangle_cst(buf, 'E', cst_counter++);
				cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_less, 0);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, root_idx, 1);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, arg_idx, -1);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, -1);

				/* add arg-root-y <= 0 */
				mangle_cst(buf, 'E', cst_counter++);
				cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_less, 0);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, root_idx, -1);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, arg_idx, 1);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, -1);
			}
			/* For all colors root and arg have "disjunct", add 1 constraints to E.
			 * If root gets a color the arg is not possible to get then they will
			 * definetly get different colors. So y has to be 1.
			 * Vice versa for arg.
			 */
			bitset_copy(work_regs, root_regs);
			bitset_xor(work_regs, arg_regs);
			bitset_foreach(work_regs, color) {
				int root_idx, arg_idx, cst_idx;
				mangle_var2(buf, 'x', rootnr, color);
				root_idx = lpp_get_var_idx(pi->curr_lp, buf);
				mangle_var2(buf, 'x', argnr, color);
				arg_idx = lpp_get_var_idx(pi->curr_lp, buf);

				mangle_cst(buf, 'E', cst_counter++);
				cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_less, 0);
				if (bitset_is_set(root_regs, color)) {
					/* add root-y <= 0 */
					lpp_set_factor_fast(pi->curr_lp, cst_idx, root_idx, 1);
					lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, -1);
				} else {
					assert(bitset_is_set(arg_regs, color) && "bitset_xor is buggy");
					/* add arg-y <= 0 */
					lpp_set_factor_fast(pi->curr_lp, cst_idx, arg_idx, 1);
					lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, -1);
				}
			}
		}
	}
}

static INLINE int get_costs(problem_instance_t *pi, ir_node *phi, ir_node *irn) {
	int i;
	unit_t *curr;
	/* search optimization unit for phi */
	list_for_each_entry(unit_t, curr, &pi->co->units, units)
		if (curr->nodes[0] == phi) {
			for (i=1; i<curr->node_count; ++i)
				if (curr->nodes[i] == irn)
					return curr->costs[i];
			assert(0 && "irn must occur in this ou");
		}
	assert(0 && "phi must be found in a ou");
	return 0;
}

static void clique_path_walker(ir_node *block, void *env) {
	problem_instance_t *pi = env;
	int count, arity, row, col, other_row, *costs;
	ir_node **phis, *phi, *irn, **phi_matrix;
	pset *done;
	bitset_t *candidates;

	/* Count all phi nodes of this block */
	for (count=0, irn = sched_first(block); is_Phi(irn); irn = sched_next(irn))
		count++;

	/* We at least 2 phi nodes for this class of inequalities */
	if (count < 2)
		return;

	/* Build the \Phi-Matrix */
	arity = get_irn_arity(sched_first(block));
	phis = alloca(count * sizeof(*phis));
	costs = alloca(count * sizeof(costs));
	phi_matrix = alloca(count*arity * sizeof(*phi_matrix));
	candidates = bitset_alloca(count);

	phi = sched_first(block);
	for (row=0; row<count; ++row) {
		phis[row] = phi;
		for (col=0; col<arity; ++col) {
			ir_node *arg = get_irn_n(phi, col);
			/* Sort out all arguments interfering with its phi */
			if (nodes_interfere(pi->co->chordal_env, phi, arg)) {
				phi_matrix[row*arity + col] =  NULL;
			} else
				phi_matrix[row*arity + col] =  arg;
		}
		phi = sched_next(phi);
	}

	/* Now find the interesting patterns in the matrix:
	 * All nodes which are used at least twice in a column. */
	/* columnwise ... */
	for (col=0; col<arity; ++col) {
		done = pset_new_ptr_default();
		for (row=0; row<count; ++row) {
			irn = phi_matrix[row*arity + col];
			/*
			 * is this an interfering arg (NULL)
			 * or has the irn already been processed in this col?
			 */
			if (!irn || pset_find_ptr(done, irn))
				continue;
			else
				pset_insert_ptr(done, irn);

			/* insert irn in candidates */
			bitset_clear_all(candidates);
			bitset_set(candidates, row);
			/* search the irn in the rows below */
			for (other_row = row+1; other_row<count; ++other_row)
				if (irn == phi_matrix[other_row*arity + col]) {
					/* found the irn in the same col in another row */
					bitset_set(candidates, other_row);
				}

			/* now we know all occurences of irn in this col */
			if (bitset_popcnt(candidates) < 2)
				continue;

			/* generate an unequation finally.
			 * phis are indexed in the bitset,
			 * shared argument is irn
			 * rhs is phi_count - 1 */
			{
				char buf[32];
				ir_node *root;
				int pos, irnnr, rootnr, cst_idx, y_idx, cst_counter = 0;
				int minimal_unequal_count = bitset_popcnt(candidates)-1;

				irnnr = get_irn_graph_nr(irn);
				mangle_cst(buf, 'M', cst_counter++);
				cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_greater, minimal_unequal_count);

				/* for all phis */
				bitset_foreach(candidates, pos) {
					root = phis[pos];
					rootnr = get_irn_graph_nr(root);
					mangle_var2(buf, 'y', rootnr, irnnr);
					y_idx = lpp_get_var_idx(pi->curr_lp, buf);
					lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, 1);
				}
			}
		}
		del_pset(done); /* clear set for next row */
	} /*next col*/
}

/**
 * Matrix M: Multi-Arg-Use. Interrelates different \phi-functions
 * in the same block, iff they use the same arg at the same pos.
 * Only one of the phis can get the arg.
 */
static void pi_add_clique_path_cstr(problem_instance_t *pi) {
	DBG((dbg, LEVEL_2, "Adding clique path constraints...\n"));
	dom_tree_walk_irg(get_irg(pi->co), clique_path_walker, NULL, pi);
}

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
				if (nodes_interfere(pi->co->chordal_env, arg1, arg2)) {
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
		be_chordal_env_t *cenv = pi->co->chordal_env;
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

	cenv = pi->co->chordal_env;
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
	irg_walk_graph(get_irg(pi->co), path_cstr_for_classes_walker, NULL, pi);
	del_pset(pi->done);
}
#endif

#ifdef PRECOLOR_MAX_CLIQUE
struct pre_col {
	problem_instance_t *pi;
	pset **clique;
};

#define has_reg_class(pi,irn) \
  (arch_get_irn_reg_class(pi->co->chordal_env->session_env->main_env->arch_env, \
                          irn, -1) == pi->co->chordal_env->cls)

static void preColoringWalker(ir_node *bl, void *env) {
	struct pre_col *e = env;
	pset **clique = e->clique;
	pset *max_clique = clique ? *clique : NULL;
	int max = max_clique ? pset_count(max_clique) : 0;
	problem_instance_t *pi = e->pi;

	int i, n;
	pset *live       = pset_new_ptr_default();
	ir_node *irn;
	irn_live_t *li;

	/* as always, bring the live end nodes to life here */
	live_foreach(bl, li) {
	  if(live_is_end(li) && has_reg_class(pi, li->irn)) {
	    pset_insert_ptr(live, irn);
	  }
	}

	sched_foreach_reverse(bl, irn) {
		int pres = pset_count(live);

		if(pres > max) {
			max = pres;
			if(max_clique)
				del_pset(max_clique);

			max_clique = pset_new_ptr_default();
			pset_insert_pset_ptr(max_clique, live);
		}



		if(has_reg_class(pi, irn))
			pset_remove_ptr(live, irn);

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op = get_irn_n(irn, i);
			if(has_reg_class(pi, op) && !is_Phi(irn))
				pset_insert_ptr(live, op);
		}
	}

  del_pset(live);
  *clique = max_clique;
}

static void pi_add_constr_preColoring(problem_instance_t *pi) {
	ir_node *irn;
	int cst_counter, color;
	struct pre_col pre_col;

	pre_col.clique = NULL;
	pre_col.pi = pi;

	dom_tree_walk_irg(get_irg(pi->co), preColoringWalker, NULL, &pre_col);

	color = 0;
	for (irn = pset_first(*pre_col.clique); irn; irn = pset_next(*pre_col.clique)) {
		int cst_idx, var_idx, nnr = get_irn_graph_nr(irn);
		char buf[100];

		mangle_cst(buf, 'K', cst_counter++);
		cst_idx = lpp_add_cst(pi->curr_lp, buf, lpp_equal, 1);

		mangle_var2(buf, 'x', nnr, color++);
		var_idx = lpp_get_var_idx(pi->curr_lp, buf);
		lpp_set_factor_fast(pi->curr_lp, cst_idx, var_idx, 1);
	}
}
#endif

/**
 * Generate the initial problem matrices and vectors.
 */
static problem_instance_t *new_pi(const copy_opt_t *co) {
	problem_instance_t *pi;
	int col;

	DBG((dbg, LEVEL_2, "Generating new instance...\n"));
	pi = xcalloc(1, sizeof(*pi));
	pi->co = co;
	pi->removed = pset_new_ptr_default();
	INIT_LIST_HEAD(&pi->simplicials);
	pi->dilp     = new_lpp(co->name, lpp_minimize);

	/* problem size reduction */
	pi_find_simplicials(pi);
	if (pi->all_simplicial)
		return pi;

	/* built objective and constraints */
	pi->curr_lp = pi->dilp;
#ifdef NO_NULL_COLORS
	pi_add_constr_no_null_colors(pi);
#endif
	pi_add_constr_A(pi);
	for (col = 0; col < pi->co->chordal_env->cls->n_regs; ++col)
		pi_add_constr_B(pi, col);
	pi_add_constr_E(pi);

#ifdef PATH_CONSTRAINTS_FOR_CLASSES
	pi_add_path_cstr_for_classes(pi);
#else
	pi_add_path_cstr(pi);
#endif
	pi_add_clique_path_cstr(pi);
#ifdef PRECOLOR_MAX_CLIQUE
	pi_add_constr_preColoring(pi);
#endif

	return pi;
}

/**
 * Clean the problem instance
 */
static void free_pi(problem_instance_t *pi) {
	simpl_t *simpl, *tmp;

	DBG((dbg, LEVEL_2, "Free instance...\n"));
	free_lpp(pi->dilp);
	list_for_each_entry_safe(simpl_t, simpl, tmp, &pi->simplicials, chain)
		free(simpl);
	del_pset(pi->removed);
	free(pi);
}

/**
 * Set starting values for the mip problem according
 * to the current coloring of the graph.
 */
static void pi_set_start_sol(problem_instance_t *pi) {
	int i;
	char var_name[64];
	DBG((dbg, LEVEL_2, "Set start solution...\n"));
	for (i=pi->first_x_var; i<=pi->last_x_var; ++i) {
		int nnr, col;
		double val;
		/* get variable name */
		lpp_get_var_name(pi->curr_lp, i, var_name, sizeof(var_name));
		/* split into components */
		if (split_var(var_name, &nnr, &col) == 2) {
			assert(get_irn_col(pi->co, get_irn_for_graph_nr(get_irg(pi->co), nnr)) != -1);
			val = (get_irn_col(pi->co, get_irn_for_graph_nr(get_irg(pi->co), nnr)) == col) ? 1 : 0;
			lpp_set_start_value(pi->curr_lp, i, val);
		} else {
			fprintf(stderr, "Variable name is: %s\n", var_name);
			assert(0 && "x vars always look like this 'x123_45'");
		}
	}
}

/**
 * Invoke a solver
 */
static void pi_solve_ilp(problem_instance_t *pi) {
  double lower_bound;

	pi_set_start_sol(pi);
	lower_bound = co_get_lower_bound(pi->co) - co_get_inevit_copy_costs(pi->co);
	lpp_set_bound(pi->curr_lp, lower_bound);
	lpp_solve_net(pi->curr_lp, LPP_HOST, LPP_SOLVER);
//	lpp_solve_cplex(pi->curr_lp);
	DBG((dbg, LEVEL_1, "Solution time: %.2f\n", pi->curr_lp->sol_time));
}

/**
 * Set the color of all simplicial nodes removed form
 * the graph before transforming it to an ilp.
 */
static void pi_set_simplicials(problem_instance_t *pi) {
	simpl_t *simpl, *tmp;
	be_ifg_t *ifg        = pi->co->chordal_env->ifg;
	bitset_t *used_cols  = bitset_alloca(arch_register_class_n_regs(pi->co->chordal_env->cls));
	void *iter           = be_ifg_iter_alloca(ifg);

	DBG((dbg, LEVEL_2, "Set simplicials...\n"));
	/* color the simplicial nodes in right order */
	list_for_each_entry_safe(simpl_t, simpl, tmp, &pi->simplicials, chain) {
		int free_col;
		ir_node *other, *irn;

		/* get free color by inspecting all neighbors */
		irn = simpl->irn;
		bitset_clear_all(used_cols);

		be_ifg_foreach_neighbour(ifg, iter, irn, other) {
			if (!is_removed(other)) /* only inspect nodes which are in graph right now */
				bitset_set(used_cols, get_irn_col(pi->co, other));
		}

		/* now all bits not set are possible colors */
		free_col = bitset_next_clear(used_cols, 0);
		assert(free_col != -1 && "No free color found. This can not be.");
		set_irn_col(pi->co, irn, free_col);
		pset_remove_ptr(pi->removed, irn); /* irn is back in graph again */
	}
}

/**
 * Sets the colors of irns according to the values of variables
 * provided by the solution of the solver.
 */
static int pi_apply_solution(problem_instance_t *pi) {
	int res = 1, i;
	double *sol;
	lpp_sol_state_t state;
	DBG((dbg, LEVEL_2, "Applying solution...\n"));

#ifdef DO_STAT
	copystat_add_ilp_time((int)(1000.0*lpp_get_sol_time(pi->curr_lp)));  //now we have ms
	copystat_add_ilp_vars(lpp_get_var_count(pi->curr_lp));
	copystat_add_ilp_csts(lpp_get_cst_count(pi->curr_lp));
	copystat_add_ilp_iter(lpp_get_iter_cnt(pi->curr_lp));
#endif

	sol = xmalloc((pi->last_x_var - pi->first_x_var + 1) * sizeof(*sol));
	state = lpp_get_solution(pi->curr_lp, sol, pi->first_x_var, pi->last_x_var);
	if (state != lpp_optimal) {
		printf("WARNING %s: Solution state is not 'optimal': %d\n", pi->co->name, state);
		assert(state >= lpp_feasible && "The solution should at least be feasible!");
		res = 0;
	}
	for (i=0; i<pi->last_x_var - pi->first_x_var + 1; ++i) {
		int nnr, col;
		char var_name[64];

		if (sol[i] > 1-EPSILON) { /* split varibale name into components */
			lpp_get_var_name(pi->curr_lp, pi->first_x_var+i, var_name, sizeof(var_name));
			if (split_var(var_name, &nnr, &col) == 2) {
				DBG((dbg, LEVEL_2, "Irn %n  Idx %d  Var %s  Val %f\n", get_irn_for_graph_nr(get_irg(pi->co), nnr), i, var_name, sol[i]));
				DBG((dbg, LEVEL_2, "x%d = %d\n", nnr, col));
				set_irn_col(pi->co, get_irn_for_graph_nr(get_irg(pi->co), nnr), col);
			} else
				assert(0 && "This should be a x-var");
		}
	}
	return res;
}

int co_ilp_opt(copy_opt_t *co, double time_limit) {
	int res = 1;
	problem_instance_t *pi;

	dbg = firm_dbg_register("ir.be.copyoptilp");
	if (!strcmp(co->name, DEBUG_IRG))
		firm_dbg_set_mask(dbg, DEBUG_IRG_LVL_ILP);
	else
		firm_dbg_set_mask(dbg, DEBUG_LVL_ILP);

	pi = new_pi(co);
	if (!pi->all_simplicial) {
#ifdef DUMP_MPS
		char buf[512];
		snprintf(buf, sizeof(buf), "%s.mps", co->name);
		lpp_dump(pi->curr_lp, buf);
#endif
		lpp_set_time_limit(pi->curr_lp, time_limit);
		pi_solve_ilp(pi);
		res = pi_apply_solution(pi);
		pi_set_simplicials(pi);
	}
	free_pi(pi);
	return res;
}
