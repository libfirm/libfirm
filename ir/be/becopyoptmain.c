/**
 * Main file for the optimization reducing the copies needed for:
 * - phi coalescing
 * - register-constrained nodes
 *
 * Contains a simple checker for this optimization.
 * By request some statistics are collected too.
 *
 * @author Daniel Grund
 * @date 11.04.2005
 */

#include "debug.h"
#include "list.h"
#include "obst.h"
#include "irgraph.h"
#include "irnode.h"
#include "irgwalk.h"

#include "bera_t.h"
#include "becopyoptmain.h"
#include "becopyopt.h"
#include "becopystat.h"

#define DO_HEUR
#define DO_ILP
#undef DO_STAT

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

/**
 * Needed for result checking
 */
static void allocatable_collector(ir_node *node, void *env) {
	struct obstack *obst = env;
	if (!is_Block(node) && is_allocatable_irn(node))
		obstack_ptr_grow(obst, node);
}

/**
 * This O(n^2) checker checks, if two ifg-connected nodes have the same color.
 */
static void check_results(ir_graph *irg) {
	struct obstack ob;
	ir_node **nodes, *n1, *n2;
	int i, o;

	obstack_init(&ob);
	irg_walk_graph(irg, allocatable_collector, NULL, &ob);
	obstack_ptr_grow(&ob, NULL);
	nodes = (ir_node **) obstack_finish(&ob);
	for (i = 0, n1 = nodes[i]; n1; n1 = nodes[++i]) {
		assert(! (is_allocatable_irn(n1) && get_irn_color(n1) == NO_COLOR));
		for (o = i+1, n2 = nodes[o]; n2; n2 = nodes[++o])
			if (phi_ops_interfere(n1, n2) && get_irn_color(n1) == get_irn_color(n2)) {
				DBG((dbg, 0, "Error: %n in %n  and  %n in %n\n", n1, get_nodes_block(n1), n2, get_nodes_block(n2)));
				assert(0 && "Interfering values have the same color!");
			}
	}
	obstack_free(&ob, NULL);
}

void be_copy_opt_init(void) {
	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, DEBUG_LVL);
}

void be_copy_opt(ir_graph* irg) {
	copy_opt_t *co;

	check_results(irg);
	co = new_copy_opt(irg);

#ifdef DO_STAT
	irg_stat_t *stats = new_irg_stat(co);
	irg_stat_count(stats, co, 0);
#endif

#ifdef DO_HEUR
	co_heur_opt(co);
	check_results(irg);
#ifdef DO_STAT
	irg_stat_count(stats, co, 1);
#endif
#endif

#ifdef DO_ILP
	co_ilp_opt(co);
	check_results(irg);
#ifdef DO_STAT
	irg_stat_count(stats, co, 2);
#endif
#endif

#ifdef DO_STAT
	irg_stat_print(stats);
	all_stat_dump();
#endif

	free_copy_opt(co);
}

void be_copy_opt_done(ir_graph* irg) {
}
