/**
 * @author Daniel Grund
 * @date 09.08.2005
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "debug.h"
#include "irgwalk.h"
#include "irop_t.h"
#include "iredges_t.h"
#include "phiclass_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

size_t phi_irn_data_offset = 0;

static void phi_class_build(ir_node *irn, pset *pc) {
	int i, max;
	const ir_edge_t *edge;

	/* If irn has a phi class assigned already
	 * return immediately to stop recursion */
	if (_get_phi_class(irn)) {
		DBG((dbg, LEVEL_2, "  already done for %+F\n", irn));
		return;
	}

	/* The initial call to phi_class_build doesn't
	 * provide a pset, so alloc it */
	if (!pc) {
		DBG((dbg, LEVEL_1, "Computing phi class for %+F\n", irn));
		assert(is_Phi(irn));
		pc = pset_new_ptr(4);
	}

	/* Add the irn to the phi class */
	DBG((dbg, LEVEL_1, "  adding %+F\n", irn));
	pset_insert_ptr(pc, irn);
	_set_phi_class(irn, pc);

	/* Check the 'neighbour' irns */
	if (is_Phi(irn) && mode_is_datab(get_irn_mode(irn))) {
		/* Add all args of the phi to the phi-class. */
		 for (i=0, max=get_irn_arity(irn); i<max; ++i) {
			DBG((dbg, LEVEL_2, "  checking arg %+F\n", get_irn_n(irn, i)));
		 	phi_class_build(get_irn_n(irn, i), pc);
		 }
	}

	/* Add a user of the irn to the class,
	 * iff it is a phi node  */
	foreach_out_edge(irn, edge) {
		ir_node *user = edge->src;
		DBG((dbg, LEVEL_2, "  checking user %+F\n", user));
		if (is_Phi(user) && mode_is_datab(get_irn_mode(user)))
			phi_class_build(user, pc);
	}
}

static void phi_class_construction_walker(ir_node *node, void *env) {
	if (is_Phi(node) && mode_is_datab(get_irn_mode(node)))
		phi_class_build(node, NULL);
}

static void phi_class_destruction_walker(ir_node *node, void *env) {
	pset *clss = _get_phi_class(node);
	if (clss) {
		ir_node *n;
		for(n = pset_first(clss); n; n = pset_next(clss))
			_set_phi_class(n, NULL);
		del_pset(clss);
	}
}

void phi_class_compute(ir_graph *irg) {
	irg_walk_graph(irg, phi_class_destruction_walker, NULL, NULL);
	irg_walk_graph(irg, phi_class_construction_walker, NULL, NULL);
}

pset *phi_class_compute_by_phis(pset *all_phi_nodes) {
	int i;
	ir_node *phi;
	pset *all_phi_classes = pset_new_ptr_default();

	if (pset_count(all_phi_nodes)) {
		ir_graph *irg = get_irn_irg(pset_first(all_phi_nodes));
		pset_break(all_phi_nodes);
		irg_walk_graph(irg, phi_class_destruction_walker, NULL, NULL);

		for (i = 0, phi=pset_first(all_phi_nodes); phi; phi=pset_next(all_phi_nodes)) {
			assert(is_Phi(phi) && mode_is_datab(get_irn_mode(phi)));
			phi_class_build(phi, NULL);
			pset_insert_ptr(all_phi_classes, _get_phi_class(phi));
		}
	}

	return all_phi_classes;
}

void phi_class_free(ir_graph *irg) {
	irg_walk_graph(irg, phi_class_destruction_walker, NULL, NULL);
}

pset *get_phi_class(const ir_node *irn) {
	return get_irn_phi_info(irn)->phi_class;
}

void phi_class_init(void) {
	FIRM_DBG_REGISTER(dbg, "ir.ana.phiclass");
	phi_irn_data_offset = register_additional_node_data(sizeof(phi_info_t));
}
