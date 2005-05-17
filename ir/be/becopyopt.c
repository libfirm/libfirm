/**
 * Author:      Daniel Grund
 * Date:		12.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "xmalloc.h"
#include "bechordal_t.h"
#include "becopyopt.h"
#include "becopystat.h"

#define DEBUG_LVL 0 //SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

#define is_curr_reg_class(irn) (arch_get_irn_reg_class(co->env, irn, arch_pos_make_out(0)) == co->cls)

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
	DBG((dbg, LEVEL_1, "\t  Root: %n\n", root));
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
	for (i=0; i<arity; ++i) {
		ir_node *arg = get_irn_n(root, i);
		assert(is_curr_reg_class(arg) && "Argument not in same register class.");
		if (arg != root) {
			if (!nodes_interfere(co->chordal_env, root, arg)) {
				DBG((dbg, LEVEL_1, "\t  Member: %n\n", arg));
				if (is_optimizable(arg))
					co_append_unit(co, arg);
				unit->nodes[unit->node_count++] = arg;
			} else
				unit->interf++;
		}
	}
	unit->nodes = xrealloc(unit->nodes, unit->node_count * sizeof(*unit->nodes));
	list_add_tail(&unit->units, &co->units);
	/* Init mis_size to node_count. So get_lower_bound returns correct results.
	 * - Now it can be called even before the heuristic has run.
	 * - And it will return correct results for units with nodecount 1 which are
	 * not optimized during the heuristic and have therefor delivered wrong results for get_lower_bound
	 */
	unit->mis_size = unit->node_count;

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
	dom_tree_walk_irg(co->irg, co_collect_in_block, NULL, co);
	del_pset(co->roots);
}

copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env,
    const arch_env_t *env, const arch_register_class_t *cls) {
	const char *s1, *s2, *s3;
	int len;
        copy_opt_t *co;

	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	co = xcalloc(1, sizeof(*co));
  co->chordal_env = chordal_env;
	co->irg = chordal_env->irg;
	co->env = env;
//	co->isa = env->isa;
	co->cls = cls;

	s1 = get_irp_prog_name();
	s2 = get_entity_name(get_irg_entity(co->irg));
	s3 = cls->name;
	len = strlen(s1) + strlen(s2) + strlen(s3) + 5;
	co->name = xmalloc(len);
	if (!strcmp(co->name, DEBUG_IRG))
		firm_dbg_set_mask(dbg, -1);
	snprintf(co->name, len, "%s__%s__%s", s1, s2, s3);

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
		if (is_optimizable(n) && (irn == n || !nodes_interfere(co->chordal_env, irn, n)))
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
		for (i=1; i<curr->node_count; ++i)
			if (root_col != get_irn_col(co, curr->nodes[i]))
				res++;
	}
	return res;
}

int co_get_lower_bound(const copy_opt_t *co) {
	int res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units)
		res += curr->interf + curr->node_count - curr->mis_size;
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
 * This O(n^2) checker checks, if two ifg-connected nodes have the same color.
 */
void co_check_allocation(copy_opt_t *co) {
	ir_node **nodes, *n1, *n2;
	int i, o;

	obstack_init(&co->ob);
	dom_tree_walk_irg(co->irg, co_collect_for_checker, NULL, co);
	obstack_ptr_grow(&co->ob, NULL);

	nodes = (ir_node **) obstack_finish(&co->ob);
	for (i = 0, n1 = nodes[i]; n1; n1 = nodes[++i]) {
		for (o = i+1, n2 = nodes[o]; n2; n2 = nodes[++o])
			if (nodes_interfere(co->chordal_env, n1, n2)
          && get_irn_col(co, n1) == get_irn_col(co, n2)) {

				DBG((dbg, 0, "Error: %n in %n  and  %n in %n have the same color.\n", n1, get_nodes_block(n1), n2, get_nodes_block(n2)));
				assert(0 && "Interfering values have the same color!");
			}
	}
	obstack_free(&co->ob, NULL);
	DBG((dbg, 2, "The checker seems to be happy!\n"));
}
