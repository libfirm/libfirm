/**
 * Author:      Daniel Grund
 * Date:		17.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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

#include "irprog.h"

#include "lpp.h"
#include "lpp_local.h"
#include "lpp_remote.h"
#include "xmalloc.h"
#include "becopyopt.h"
#include "becopystat.h"

#define DUMP_MPS
#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

#define SLOTS_LIVING 32

/**
 * Represents the _costs_ if node n and m have different colors.
 * Must be >=0.
 **/
#define get_weight(n,m) 1

typedef struct _simpl_t {
	struct list_head chain;
	if_node_t *ifn;
} simpl_t;

typedef struct _problem_instance_t {
	const copy_opt_t *co;			/** the copy_opt problem */
	/* problem size reduction removing simple nodes */
	struct list_head simplicials;	/**< holds all simpl_t's in right order to color*/
	pset *removed;					/**< holds all removed simplicial irns */
	/* lp problem */
	lpp_t *dilp;					/**< problem formulation directly as milp */
	/* overhead stuff */
	lpp_t *curr_lp;					/**< points to the problem currently used */
	int curr_color, cst_counter, last_x_var;
	char buf[32];
	int all_simplicial;
} problem_instance_t;

#define is_removed(irn) pset_find_ptr(pi->removed, irn)

#define is_color_possible(irn,color) arch_reg_is_allocatable(pi->co->chordal_env->arch_env, irn, arch_pos_make_out(0), arch_register_for_index(pi->co->chordal_env->cls, color))

/*
 * Some stuff for variable name handling.
 */
#define mangle_cst(buf, prefix, nr) \
			snprintf((buf), sizeof(buf), "%c%d", (prefix), (nr))

#define mangle_var(buf, prefix, node_nr, color) \
			snprintf((buf), sizeof(buf), "%c%d_%d", (prefix), (node_nr), (color))

#define mangle_var_irn(buf, prefix, irn, color) \
			mangle_var((buf), (prefix), get_irn_graph_nr(irn), (color))

#define split_var(var, nnr, col) \
			sscanf(var, "x%d_%d", (nnr), (col))


/**
 * Checks if a node is simplicial in the graph
 * heeding the already removed nodes.
 */
static INLINE int pi_is_simplicial(problem_instance_t *pi, const if_node_t *ifn) {
	int i, o, size = 0;
	if_node_t **all, *curr;
	all = alloca(ifn_get_degree(ifn) * sizeof(*all));

	/* get all non-removed neighbors */
	foreach_neighb(ifn, curr)
		if (!is_removed(curr))
			all[size++] = curr;

	/* check if these form a clique */
	for (i=0; i<size; ++i)
		for (o=i+1; o<size; ++o)
			if (!ifg_has_edge(pi->co->chordal_env, all[i], all[o]))
				return 0;

	/* all edges exist so this is a clique */
	return 1;
}

/**
 * Iterative finds and 'removes' from the graph all nodes which are
 * simplicial AND not member of a equal-color-wish
 */
static void pi_find_simplicials(problem_instance_t *pi) {
	set *if_nodes;
	if_node_t *ifn;
	int redo = 1;

	DBG((dbg, LEVEL_2, "Find simlicials...\n"));

	if_nodes = be_ra_get_ifg_nodes(pi->co->chordal_env);
	while (redo) {
		redo = 0;
		for (ifn = set_first(if_nodes); ifn; ifn = set_next(if_nodes)) {
			ir_node *irn = get_irn_for_graph_nr(pi->co->chordal_env->irg, ifn->nnr);
			if (!is_removed(irn) && !is_optimizable(irn) &&
          !is_optimizable_arg(pi->co, irn) && pi_is_simplicial(pi, ifn)) {
				simpl_t *s = xmalloc(sizeof(*s));
				s->ifn = ifn;
				list_add(&s->chain, &pi->simplicials);
				pset_insert_ptr(pi->removed, irn);
				redo = 1;
				DBG((dbg, LEVEL_2, " Removed %n\n", irn));
			}
		}
	}
}

/**
 * Add coloring-force conditions
 */
static void pi_add_constr_A(ir_node *block, void *env) {
	problem_instance_t *pi = env;
	struct list_head *head = get_block_border_head(pi->co->chordal_env, block);
	border_t *curr;
	bitset_t *pos_regs = bitset_alloca(pi->co->chordal_env->cls->n_regs);

	list_for_each_entry_reverse(border_t, curr, head, list)
		if (curr->is_def && curr->is_real && !is_removed(curr->irn)) {
			int cst_idx, nnr, col;

			nnr = get_irn_graph_nr(curr->irn);
			mangle_cst(pi->buf, 'A', nnr);
			cst_idx = lpp_add_cst(pi->curr_lp, pi->buf, equal, 1);

			// iterate over all possible colors in order
			bitset_clear_all(pos_regs);
			arch_get_allocatable_regs(pi->co->chordal_env->arch_env, curr->irn, arch_pos_make_out(0), pi->co->chordal_env->cls, pos_regs);
			bitset_foreach(pos_regs, col) {
				int var_idx;
				mangle_var(pi->buf, 'x', nnr, col);
				var_idx = lpp_add_var(pi->curr_lp, pi->buf, binary, 0);
				pi->last_x_var = var_idx;
				lpp_set_factor_fast(pi->curr_lp, cst_idx, var_idx, 1);
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
 * for which the color pi->curr_color is possible. Finds only 'maximal-cliques',
 * viz cliques which are not contained in another one.
 * This is used for the matrix B.
 */
static void pi_add_constr_B(ir_node *block, void *env) {
	problem_instance_t *pi = env;
	enum phase_t {growing, shrinking} phase = growing;
	struct list_head *head = get_block_border_head(pi->co->chordal_env, block);
	border_t *b;
	pset *living = pset_new_ptr(SLOTS_LIVING);

	list_for_each_entry_reverse(border_t, b, head, list) {
		const ir_node *irn = b->irn;
		if (is_removed(irn) || !is_color_possible(irn, pi->curr_color))
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
				cst_idx = lpp_add_cst(pi->curr_lp, pi->buf, less, 1);
				for (n = pset_first(living); n; n = pset_next(living)) {
					int var_idx;
					mangle_var_irn(pi->buf, 'x', n, pi->curr_color);
					var_idx = lpp_get_var_idx(pi->curr_lp, pi->buf);
					lpp_set_factor_fast(pi->curr_lp, cst_idx, var_idx, 1);
				}
				pi->cst_counter++;
			}
			pset_remove_ptr(living, irn);
			phase = shrinking;
		}
	}

	del_pset(living);
}

static void pi_add_constr_E(problem_instance_t *pi) {
	unit_t *curr;
	bitset_t *root_regs, *arg_regs;
	int cst_counter = 0;
	unsigned nregs = pi->co->chordal_env->cls->n_regs;
	root_regs = bitset_alloca(nregs);
	arg_regs = bitset_alloca(nregs);

	/* for all roots of optimization units */
	list_for_each_entry(unit_t, curr, &pi->co->units, units) {
		const ir_node *root, *arg;
		int rootnr, argnr, color;
		int y_idx, i;
		char buf[32];

		root = curr->nodes[0];
		rootnr = get_irn_graph_nr(root);
		bitset_clear_all(root_regs);
		arch_get_allocatable_regs(pi->co->chordal_env->arch_env, root, arch_pos_make_out(0), pi->co->chordal_env->cls, root_regs);

		/* for all arguments of root */
		for (i = 1; i < curr->node_count; ++i) {
			arg = curr->nodes[i];
			argnr = get_irn_graph_nr(arg);
			bitset_clear_all(arg_regs);
			arch_get_allocatable_regs(pi->co->chordal_env->arch_env, arg, arch_pos_make_out(0), pi->co->chordal_env->cls, arg_regs);

			/* Introduce new variable and set factor in objective function */
			mangle_var(buf, 'y', rootnr, argnr);
			y_idx = lpp_add_var(pi->curr_lp, buf, continous, get_weight(root, arg));
			/* set starting value */
			//lpp_set_start_value(pi->curr_lp, y_idx, (get_irn_col(pi->co, root) != get_irn_col(pi->co, arg)));

			/* For all colors root and arg have in common, add 2 constraints to E */
			bitset_and(arg_regs, root_regs);
			bitset_foreach(arg_regs, color) {
				int root_idx, arg_idx, cst_idx;
				mangle_var(buf, 'x', rootnr, color);
				root_idx = lpp_get_var_idx(pi->curr_lp, buf);
				mangle_var(buf, 'x', argnr, color);
				arg_idx = lpp_get_var_idx(pi->curr_lp, buf);

				/* add root-arg-y <= 0 */
				mangle_cst(buf, 'E', cst_counter++);
				cst_idx = lpp_add_cst(pi->curr_lp, buf, less, 0);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, root_idx, 1);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, arg_idx, -1);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, -1);

				/* add arg-root-y <= 0 */
				mangle_cst(buf, 'E', cst_counter++);
				cst_idx = lpp_add_cst(pi->curr_lp, buf, less, 0);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, root_idx, -1);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, arg_idx, 1);
				lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, -1);
			}
		}
	}
}

/**
 * Sum(y_root_arg, arg \in Args) <= max_indep_set_size - 1
 */
static void pi_add_constr_M(problem_instance_t *pi) {
	unit_t *curr;
	int cst_counter = 0;

	/* for all optimization units */
	list_for_each_entry(unit_t, curr, &pi->co->units, units) {
		const ir_node *root, *arg;
		int rootnr, argnr;
		int cst_idx, y_idx, i;
		char buf[32];

		if (curr->ifg_mis_size == curr->node_count)
			continue;

		root = curr->nodes[0];
		rootnr = get_irn_graph_nr(root);
		mangle_cst(buf, 'M', cst_counter++);
		cst_idx = lpp_add_cst(pi->curr_lp, buf, greater, curr->node_count - curr->ifg_mis_size);

		/* for all arguments */
		for (i = 1; i < curr->node_count; ++i) {
			arg = curr->nodes[i];
			argnr = get_irn_graph_nr(arg);
			mangle_var(buf, 'y', rootnr, argnr);
			y_idx = lpp_get_var_idx(pi->curr_lp, buf);
			lpp_set_factor_fast(pi->curr_lp, cst_idx, y_idx, 1);
		}
	}
}

/**
 * Generate the initial problem matrices and vectors.
 */
static problem_instance_t *new_pi(const copy_opt_t *co) {
	problem_instance_t *pi;

	DBG((dbg, LEVEL_2, "Generating new instance...\n"));
	pi = xcalloc(1, sizeof(*pi));
	pi->co = co;
	pi->removed = pset_new_ptr_default();
	INIT_LIST_HEAD(&pi->simplicials);
	pi->dilp = new_lpp(co->name, minimize);
	pi->last_x_var = -1;

	/* problem size reduction */
	pi_find_simplicials(pi);
	//TODO dump_ifg_w/o_removed
	if (set_count(be_ra_get_ifg_nodes(pi->co->chordal_env)) == pset_count(pi->removed))
		pi->all_simplicial = 1;

	pi->curr_lp = pi->dilp;

	/* Matrix A: knapsack constraint for each node */
	DBG((dbg, LEVEL_2, "Add A constraints...\n"));
	dom_tree_walk_irg(co->chordal_env->irg, pi_add_constr_A, NULL, pi);
	/* Matrix B: interference constraints using cliques */
	DBG((dbg, LEVEL_2, "Add B constraints...\n"));
	for (pi->curr_color = 0; pi->curr_color < pi->co->chordal_env->cls->n_regs; ++pi->curr_color)
		dom_tree_walk_irg(co->chordal_env->irg, pi_add_constr_B, NULL, pi);
	/* Matrix E: interrelate x with y variables */
	DBG((dbg, LEVEL_2, "Add E constraints...\n"));
	pi_add_constr_E(pi);
	/* Matrix M: maximum independent set constraints */
	DBG((dbg, LEVEL_2, "Add M constraints...\n"));
	//pi_add_constr_M(pi);

	return pi;
}

/**
 * Clean the problem instance
 */
static void free_pi(problem_instance_t *pi) {
	DBG((dbg, LEVEL_2, "Free instance...\n"));
	/* pi->simplicials get freed during apply_solution */
	free_lpp(pi->dilp);
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
	for (i=1; i<=pi->last_x_var; ++i) {
		int nnr, col;
		double val;
		/* get variable name */
		lpp_get_var_name(pi->curr_lp, i, var_name, sizeof(var_name));
		/* split into components */
		if (split_var(var_name, &nnr, &col) == 2) {
			assert(get_irn_col(pi->co, get_irn_for_graph_nr(pi->co->chordal_env->irg, nnr)) != -1);
			val = (get_irn_col(pi->co, get_irn_for_graph_nr(pi->co->chordal_env->irg, nnr)) == col) ? 1 : 0;
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
static void pi_solve_ilp(problem_instance_t *pi, void (*lpp_solve)(lpp_t *)) {
	pi_set_start_sol(pi);
	lpp_solve(pi->curr_lp);
	DBG((dbg, LEVEL_1, "Solution time: %f\n", lpp_get_sol_time(pi->curr_lp)));
}

/**
 * Set the color of all simplicial nodes removed form
 * the graph before transforming it to an ilp.
 */
static void pi_set_simplicials(problem_instance_t *pi) {
	simpl_t *simpl, *tmp;
	bitset_t *used_cols = bitset_alloca(arch_register_class_n_regs(pi->co->chordal_env->cls));

	DBG((dbg, LEVEL_2, "Set simplicials...\n"));
	/* color the simplicial nodes in right order */
	list_for_each_entry_safe(simpl_t, simpl, tmp, &pi->simplicials, chain) {
		int free_col;
		ir_node *other_irn, *irn;
		if_node_t *other, *ifn;

		/* get free color by inspecting all neighbors */
		ifn = simpl->ifn;
		irn = get_irn_for_graph_nr(pi->co->chordal_env->irg, ifn->nnr);
		bitset_clear_all(used_cols);
		foreach_neighb(ifn, other) {
			other_irn = get_irn_for_graph_nr(pi->co->chordal_env->irg, other->nnr);
			if (!is_removed(other_irn)) /* only inspect nodes which are in graph right now */
				bitset_set(used_cols, get_irn_col(pi->co, other_irn));
		}

		/* now all bits not set are possible colors */
		free_col = bitset_next_clear(used_cols, 0);
		assert(free_col != -1 && "No free color found. This can not be.");
		set_irn_col(pi->co, irn, free_col);
		pset_remove_ptr(pi->removed, irn); /* irn is back in graph again */
		free(simpl);
	}
}

/**
 * Sets the colors of irns according to the values of variables
 * provided by the solution of the solver.
 */
static void pi_apply_solution(problem_instance_t *pi) {
	int i;
	double *sol;
	sol_state_t state;
	DBG((dbg, LEVEL_2, "Applying solution...\n"));

#ifdef DO_STAT
	//TODO stat
#endif

	sol = xmalloc((pi->last_x_var+1) * sizeof(*sol));
	state = lpp_get_solution(pi->curr_lp, sol, 1, pi->last_x_var);
	if (state != optimal) {
		printf("Solution state is not 'optimal': %d\n", state);
		if (state < feasible)
			assert(0);
	}
	for (i=0; i<pi->last_x_var; ++i)
		if (sol[i] == 1.0) { /* split varibale name into components */
			int nnr, col;
			char var_name[64];
			lpp_get_var_name(pi->curr_lp, 1+i, var_name, sizeof(var_name));
			if (split_var(var_name, &nnr, &col) == 2) {
				DBG((dbg, LEVEL_2, " x%d = %d\n", nnr, col));
				set_irn_col(pi->co, get_irn_for_graph_nr(pi->co->chordal_env->irg, nnr), col);
			} else
				assert(0 && "This should be a x-var");
		}
}

void co_ilp_opt(copy_opt_t *co) {
	problem_instance_t *pi;

	dbg = firm_dbg_register("ir.be.copyoptilp");
	if (!strcmp(co->name, DEBUG_IRG))
		firm_dbg_set_mask(dbg, -1);
	else
		firm_dbg_set_mask(dbg, DEBUG_LVL);

	pi = new_pi(co);
	if (!pi->all_simplicial) {
#ifdef DUMP_MPS
		char buf[512];
		snprintf(buf, sizeof(buf), "%s.mps", co->name);
		lpp_dump(pi->curr_lp, buf);
#endif
		pi_solve_ilp(pi, lpp_solve_local);
		pi_apply_solution(pi);
	}
	pi_set_simplicials(pi);
	free_pi(pi);
}
