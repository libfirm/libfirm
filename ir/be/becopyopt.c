/**
 * @author Daniel Grund
 * @date 12.04.2005
 */

#include "becopyopt.h"

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

//TODO
#define is_optimizable(irn) (is_Phi(irn) || 0)

/**
 * Builds an optimization unit for a given optimizable irn (root).
 * This opt-unit is inserted in the main structure co.
 * If an arg of root itself is optimizable process this arg before with a
 * recursive call. For handling this situation and loops co->root is used
 * to remember all roots.
 */
static void co_append_unit(copy_opt_t *co, const ir_node *root) {
	int i, arity;
	unit_t *unit;
	DBG((dbg, LEVEL_1, "Root: %n\n", root));
	/* check if we encountered this root earlier */
	if (pset_find_ptr(co->roots, root))
		return;
	pset_insert_ptr(co->roots, root);

	/* init unit */
	arity = get_irn_arity(root);
	unit = malloc(sizeof(*unit));
	unit->interf = 0;
	unit->node_count = 1;
	unit->nodes = malloc((arity+1) * sizeof(*unit->nodes));
	unit->nodes[0] = root;
	INIT_LIST_HEAD(&unit->queue);

	/* check all args */
	for (i=0; i<arity; ++i) {
		ir_node *arg = get_irn_n(root, i);
		if (!values_interfere(root, arg)) {
			if (arg != root) {
				DBG((dbg, LEVEL_1, "Member: %n\n", arg));
				if (is_optimizable(arg))
					co_append_unit(co, arg);
				unit->nodes[unit->node_count++] = arg;
			}
		} else
			unit->interf++;
	}

	if (unit->node_count > 1) {
		unit->nodes = realloc(unit->nodes, unit->node_count * sizeof(*unit->nodes));
		list_add_tail(&unit->units, &co->units);
	} else {
		free(unit->nodes);
		free(unit);
	}
}

static void co_collect_in_block(ir_node *block, void *env) {
	copy_opt_t *co = env;
	struct list_head *head = &get_ra_block_info(block)->border_head;
	border_t *curr;

	list_for_each_entry_reverse(border_t, curr, head, list)
		if (curr->is_def && curr->is_real && is_optimizable(curr->irn))
			co_append_unit(co, curr->irn);
}

static void co_collect_units(copy_opt_t *co) {
	co->roots = pset_new_ptr(64);
	dom_tree_walk_irg(co->irg, co_collect_in_block, NULL, co);
	del_pset(co->roots);
}

copy_opt_t *new_copy_opt(ir_graph *irg) {
	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	copy_opt_t *co = calloc(1, sizeof(*co));
	co->irg = irg;
	INIT_LIST_HEAD(&co->units);
	co_collect_units(co);
	return co;
}

void free_copy_opt(copy_opt_t *co) {
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units) {
		free(curr->nodes);
		free(curr);
	}
}

int co_get_lower_bound(copy_opt_t *co) {
	//TODO Think if copy counting is this easy!
	int res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units)
		res += curr->interf + curr->node_count - curr->mis_size;
	return res;
}

int co_get_interferer_count(copy_opt_t *co) {
	int res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units)
		res += curr->interf;
	return res;
}
