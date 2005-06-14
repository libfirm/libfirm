/**
 * Author:      Daniel Grund
 * Date:		12.04.2005
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

#include "xmalloc.h"
#include "bechordal_t.h"
#include "becopyopt.h"
#include "becopystat.h"

#define DEBUG_LVL 0 //SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

#define is_curr_reg_class(irn) (arch_get_irn_reg_class(co->chordal_env->arch_env, irn, arch_pos_make_out(0)) == co->chordal_env->cls)

#define MIN(a,b) ((a<b)?(a):(b))

/**
 * Computes a 'max independent set' wrt. ifg-edges only (no coloring conflicts, no register constraints)
 * @return The size of such a mis
 * NOTE: Code adapted from becopyheur
 * BETTER: Here we can be sure having a chordal graph to work on, so for 'larger' opt-units we
 *         could use a special algorithm.
 */
static int get_ifg_mis_size(unit_t *ou) {
	int all_size, curr_size, i, o;
	int *which;
	ir_node **curr, **all = alloca(ou->node_count * sizeof(*all));

	/* all contains all nodes */
	all_size = 0;
	for (i=0; i<ou->node_count; ++i)
		all[all_size++] = ou->nodes[i];

	/* which[i] says which element to take out of all[] and put into curr[i] */
	which = alloca(all_size*sizeof(*which));
	for (curr_size=0; curr_size<all_size; ++curr_size)
		which[curr_size] = curr_size;

	/* stores the currently examined set */
	curr = alloca(all_size*sizeof(*curr));

	while (1) { /* this loop will terminate because at least a single node will be a max indep. set */
		/* build current set */
		for (i=0; i<curr_size; ++i)
			curr[i] = all[which[all_size-curr_size+i]];

		/* check current set */
		for (i=0; i<curr_size; ++i)
			for (o=i+1; o<curr_size; ++o)
				if (nodes_interfere(ou->co->chordal_env, curr[i], curr[o]))
					goto conflict_found;

		/* We had no conflict. This is the (one) max indep. set */
		return curr_size;

conflict_found:
		/* We had a conflict. Generate next set */
		if (which[all_size-curr_size+1] == all_size-curr_size+1) {
			curr_size--;
			for (i=0; i<curr_size; ++i)
				which[all_size-curr_size+i] = i;
		} else {
			int redo = 1;
			while (redo) {
				int pos = all_size;
				do {
					pos--;
				} while (!(which[pos] = (which[pos]+1) % all_size));

				for (i=pos+1; i<all_size; ++i)
					which[i] = MIN(which[i-1]+1, all_size-1);

				redo = 0;
				for (i=all_size-curr_size; i<all_size-1; ++i)
					if (which[i]>=which[i+1]) {
						redo = 1;
						break;
					}
			}
		}
	}
	assert(0 && "How did you get here?");
}

/**
 * Builds an optimization unit for a given optimizable irn (root).
 * This opt-unit is inserted in the main structure co.
 * If an arg of root itself is optimizable process this arg before with a
 * recursive call. For handling this situation and loops co->root is used
 * to remember all roots.
 */
static void co_append_unit(copy_opt_t *co, ir_node *root) {
	int i, arity;
	unit_t *unit;
	DBG((dbg, LEVEL_1, "\t  Root: %n %N\n", root, root));
	/* check if we encountered this root earlier */
	if (pset_find_ptr(co->roots, root))
		return;
	pset_insert_ptr(co->roots, root);

	assert(is_curr_reg_class(root) && "node is in wrong register class!");

	/* init unit */
	arity = get_irn_arity(root);
	unit = xcalloc(1, sizeof(*unit));
	unit->co = co;
	unit->interf = 0;
	unit->node_count = 1;
	unit->nodes = xmalloc((arity+1) * sizeof(*unit->nodes));
	unit->nodes[0] = root;
	INIT_LIST_HEAD(&unit->queue);

	/* check all args */
	if (is_Phi(root)) {
		for (i=0; i<arity; ++i) {
			ir_node *arg = get_irn_n(root, i);
			assert(is_curr_reg_class(arg) && "Argument not in same register class.");
			if (arg != root) {
				if (!nodes_interfere(co->chordal_env, root, arg)) {
					DBG((dbg, LEVEL_1, "\t   Member: %n %N\n", arg, arg));
					if (is_optimizable(arg))
						co_append_unit(co, arg);
					unit->nodes[unit->node_count++] = arg;
				} else
					unit->interf++;
			}
		}
		unit->nodes = xrealloc(unit->nodes, unit->node_count * sizeof(*unit->nodes));
	} else if (is_Copy(root)) {
		assert(!nodes_interfere(co->chordal_env, root, get_Copy_src(root)));
		unit->nodes[unit->node_count++] = get_Copy_src(root);
		unit->nodes = xrealloc(unit->nodes, 2 * sizeof(*unit->nodes));
	} else
		assert(0 && "This is not an optimizable node!");

	list_add_tail(&unit->units, &co->units);
	/* Init ifg_mis_size to node_count. So get_lower_bound returns correct results. */
	unit->ifg_mis_size = get_ifg_mis_size(unit);
}

static void co_collect_in_block(ir_node *block, void *env) {
	copy_opt_t *co = env;
	struct list_head *head = get_block_border_head(co->chordal_env, block);
	border_t *curr;

	list_for_each_entry_reverse(border_t, curr, head, list)
		if (curr->is_def && curr->is_real && is_optimizable(curr->irn))
			co_append_unit(co, curr->irn);
}

static void co_collect_units(copy_opt_t *co) {
	DBG((dbg, LEVEL_1, "\tCollecting optimization units\n"));
	co->roots = pset_new_ptr(64);
	dom_tree_walk_irg(co->chordal_env->irg, co_collect_in_block, NULL, co);
	del_pset(co->roots);
}

copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env) {
	const char *s1, *s2, *s3;
	int len;
        copy_opt_t *co;

	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	co = xcalloc(1, sizeof(*co));
	co->chordal_env = chordal_env;

	s1 = get_irp_prog_name();
	s2 = get_entity_name(get_irg_entity(co->chordal_env->irg));
	s3 = chordal_env->cls->name;
	len = strlen(s1) + strlen(s2) + strlen(s3) + 5;
	co->name = xmalloc(len);
	snprintf(co->name, len, "%s__%s__%s", s1, s2, s3);
	if (!strcmp(co->name, DEBUG_IRG))
		firm_dbg_set_mask(dbg, DEBUG_LVL_CO);
	else
		firm_dbg_set_mask(dbg, DEBUG_LVL);

	INIT_LIST_HEAD(&co->units);
	co_collect_units(co);
	return co;
}

void free_copy_opt(copy_opt_t *co) {
	unit_t *curr, *tmp;
	xfree(co->name);
	list_for_each_entry_safe(unit_t, curr, tmp, &co->units, units) {
		xfree(curr->nodes);
		xfree(curr);
	}
}

int is_optimizable_arg(const copy_opt_t *co, ir_node *irn) {
	int i, max;
	for(i=0, max=get_irn_n_outs(irn); i<max; ++i) {
		ir_node *n = get_irn_out(irn, i);
		if ((is_Phi(n) || is_Perm(n)) && (irn == n || !nodes_interfere(co->chordal_env, irn, n)))
			return 1;
	}
	return 0;
}

int co_get_copy_count(const copy_opt_t *co) {
	int i, res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units) {
		int root_col = get_irn_col(co, curr->nodes[0]);
		res += curr->interf;
		DBG((dbg, LEVEL_1, "%n %N has %d intf\n", curr->nodes[0], curr->nodes[0], curr->interf));
		for (i=1; i<curr->node_count; ++i)
			if (root_col != get_irn_col(co, curr->nodes[i])) {
				DBG((dbg, LEVEL_1, "  %n %N\n", curr->nodes[i], curr->nodes[i]));
				res++;
			}
	}
	return res;
}

int co_get_lower_bound(const copy_opt_t *co) {
	int res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units)
		res += curr->interf + curr->node_count - curr->ifg_mis_size;
	return res;
}

int co_get_interferer_count(const copy_opt_t *co) {
	int res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units)
		res += curr->interf;
	return res;
}

/**
 * Needed for result checking
 */
static void co_collect_for_checker(ir_node *block, void *env) {
	copy_opt_t *co = env;
	struct list_head *head = get_block_border_head(co->chordal_env, block);
	border_t *curr;

	list_for_each_entry_reverse(border_t, curr, head, list)
		if (curr->is_def && curr->is_real && is_curr_reg_class(curr->irn))
			obstack_ptr_grow(&co->ob, curr->irn);
}

/**
 * This O(n^2) checker checks if
 * 	two ifg-connected nodes have the same color
 * 	register constraint are satisfied
 */
void co_check_allocation(copy_opt_t *co) {
	ir_node **nodes, *n1, *n2;
	int i, o;

	obstack_init(&co->ob);
	dom_tree_walk_irg(co->chordal_env->irg, co_collect_for_checker, NULL, co);
	obstack_ptr_grow(&co->ob, NULL);

	nodes = (ir_node **) obstack_finish(&co->ob);
	for (i = 0, n1 = nodes[i]; n1; n1 = nodes[++i]) {
		assert(arch_reg_is_allocatable(co->chordal_env->arch_env, n1, arch_pos_make_out(0),
			arch_get_irn_register(co->chordal_env->arch_env, n1, 0)) && "Constraint does not hold");
		for (o = i+1, n2 = nodes[o]; n2; n2 = nodes[++o])
			if (nodes_interfere(co->chordal_env, n1, n2)
          && get_irn_col(co, n1) == get_irn_col(co, n2)) {
				DBG((dbg, 0, "Error in graph %s: %n %d  and  %n %d have the same color %d.\n", co->name, n1, get_irn_graph_nr(n1), n2, get_irn_graph_nr(n2), get_irn_col(co, n1)));
				assert(0 && "Interfering values have the same color!");
			}
	}
	obstack_free(&co->ob, NULL);
	DBG((dbg, 2, "The checker seems to be happy!\n"));
}
