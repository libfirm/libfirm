/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#include <stdio.h>

#include "pset.h"
#include "irgraph.h"
#include "irnode.h"
#include "irgwalk.h"

#include "domtree.h"
#include "bephiopt.h"
#include "bephicongr_t.h"
#include "bephicoal_t.h"
#include "phistat.h"

#define CUMULATIVE_FILE "~/all.phistat"

static firm_dbg_module_t *dbgphi = NULL;

static void phi_node_walker(ir_node *node, void *env) {
	if (is_Phi(node) && mode_is_datab(get_irn_mode(node)))
		pset_insert_ptr((pset *)env, node);
}


void be_phi_opt(ir_graph* irg) {
	dominfo_t *dominfo;
	pset *all_phi_nodes, *all_phi_classes;
	char buf[1024];



	/* get all phi nodes */
	printf("-----------------------> Collecting phi nodes <-----------------------\n");
	all_phi_nodes = pset_new_ptr(64);
	irg_walk_graph(irg, phi_node_walker, NULL, all_phi_nodes);

	/* get all phi congruence classes */
	printf("-----------------------> Collecting phi classes <-----------------------\n");
	all_phi_classes = be_phi_congr_classes(all_phi_nodes);

	/* do some statistics */
	printf("-----------------------> Collecting phi stats <-----------------------\n");
	phi_stat_reset();
	phi_stat_collect(irg, all_phi_nodes, all_phi_classes);
	snprintf(buf, sizeof(buf), "%s.phistat", get_entity_name(get_irg_entity(irg)));
	//phi_stat_dump_pretty(buf);
	phi_stat_dump(buf, CUMULATIVE_FILE);

	/* try to coalesce the colors of each phi class */
	printf("-----------------------> Building domtree <-----------------------\n");
	dominfo = domtree_create(irg);
	printf("-----------------------> Coalescing <-----------------------\n");
	/* be_phi_coalesce_locals(all_phi_classes, dominfo); */
	printf("-----------------------> Destroying domtree <-----------------------\n");
	domtree_free(dominfo);
}


void be_phi_opt_init(void) {
	dbgphi = firm_dbg_register("Phi optimizer");
	firm_dbg_set_mask(dbgphi, 1);

	be_phi_congr_class_init();
	be_phi_coal_init();
}
