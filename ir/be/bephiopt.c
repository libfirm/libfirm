/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#include "pset.h"
#include "irnode.h"
#include "irgwalk.h"

#include "bephiopt.h"
#include "bephicongr_t.h"
#include "bephicoal_t.h"

pset *all_phi_nodes = NULL;


static void phi_node_walker(ir_node *node, void *env) {
	if (is_Phi(node) && mode_is_datab(get_irn_mode(node)))
		pset_insert_ptr(all_phi_nodes, node);
}


void be_phi_opt(ir_graph* irg) {
	all_phi_nodes = pset_new_ptr(64);
	irg_walk_graph(irg, phi_node_walker, NULL, NULL);

	be_phi_congr_classes(all_phi_nodes);
	be_phi_coalesce_locals(all_phi_classes);
}


void be_phi_opt_init(void) {
	be_phi_congr_class_init();
	be_phi_coal_init();
}
