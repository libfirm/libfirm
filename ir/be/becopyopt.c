/**
 * @author Daniel Grund
 * @date 12.04.2005
 */

#include "becopyopt.h"
#include "becopystat.h"

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

//TODO external things
#define is_curr_reg_class(irn) (co->isa->get_irn_reg_class(irn)==co->cls)
#define is_optimizable(irn) (is_Phi(irn) || is_Copy(irn))

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
	DBG((dbg, LEVEL_1, "\t  Root: %n\n", root));
	/* check if we encountered this root earlier */
	if (pset_find_ptr(co->roots, root))
		return;
	pset_insert_ptr(co->roots, root);

	assert(is_curr_reg_class(root) && "node is in wrong register class!");

	/* init unit */
	arity = get_irn_arity(root);
	unit = calloc(1, sizeof(*unit));
	unit->interf = 0;
	unit->node_count = 1;
	unit->nodes = malloc((arity+1) * sizeof(*unit->nodes));
	unit->nodes[0] = root;
	INIT_LIST_HEAD(&unit->queue);

	/* check all args */
	for (i=0; i<arity; ++i) {
		ir_node *arg = get_irn_n(root, i);
		assert(is_curr_reg_class(arg) && "Argument not in same register class.");
		if (arg != root) {
			if (!values_interfere(root, arg)) {
				DBG((dbg, LEVEL_1, "\t  Member: %n\n", arg));
				if (is_optimizable(arg))
					co_append_unit(co, arg);
				unit->nodes[unit->node_count++] = arg;
			} else
				unit->interf++;
		}
	}

	unit->nodes = realloc(unit->nodes, unit->node_count * sizeof(*unit->nodes));
	list_add_tail(&unit->units, &co->units);
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
	DBG((dbg, LEVEL_1, "\tCollecting optimization units\n"));
	co->roots = pset_new_ptr(64);
	dom_tree_walk_irg(co->irg, co_collect_in_block, NULL, co);
	del_pset(co->roots);
}

copy_opt_t *new_copy_opt(ir_graph *irg, const arch_isa_if_t *isa, const arch_register_class_t *cls) {
	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	copy_opt_t *co = calloc(1, sizeof(*co));
	co->irg = irg;
	co->isa = isa;
	co->cls = cls;
	INIT_LIST_HEAD(&co->units);
	co_collect_units(co);
	return co;
}

void free_copy_opt(copy_opt_t *co) {
	unit_t *curr, *tmp;
	list_for_each_entry_safe(unit_t, curr, tmp, &co->units, units) {
		free(curr->nodes);
		free(curr);
	}
}

int co_get_copy_count(copy_opt_t *co) {
	int i, res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units) {
		int root_col = get_irn_color(curr->nodes[0]);
		res += curr->interf;
		for (i=1; i<curr->node_count; ++i)
			if (root_col != get_irn_color(curr->nodes[i]))
				res++;
	}
	return res;
}

int co_get_lower_bound(copy_opt_t *co) {
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

/**
 * Needed for result checking
 */
static void co_collect_for_checker(ir_node *block, void *env) {
	copy_opt_t *co = env;
	struct list_head *head = &get_ra_block_info(block)->border_head;
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
		assert(! (is_allocatable_irn(n1) && get_irn_color(n1) == NO_COLOR));
		for (o = i+1, n2 = nodes[o]; n2; n2 = nodes[++o])
			if (phi_ops_interfere(n1, n2) && get_irn_color(n1) == get_irn_color(n2)) {
				DBG((dbg, 0, "Error: %n in %n  and  %n in %n have the same color.\n", n1, get_nodes_block(n1), n2, get_nodes_block(n2)));
				assert(0 && "Interfering values have the same color!");
			}
	}
	obstack_free(&co->ob, NULL);
	DBG((dbg, 2, "The checker seems to be happy!\n"));
}
