/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#include <stdlib.h>
#include <stdio.h>

#include "pset.h"
#include "irgraph.h"
#include "irnode.h"
#include "irgwalk.h"
#include "irdom.h"

#include "bephiopt.h"
#include "bephicongr_t.h"
#include "bephicoal_t.h"
#include "phistat.h"

#define DEBUG_LVL SET_LEVEL_1

#define DO_PHI_STATISTICS
#undef DUMP_IRG_PHI_STAT

#define DUMP_CUMULATIVE
#define CUMULATIVE_FILE "all.phistat"

#define ENV_PHI_STAT "PHI_STAT"

static firm_dbg_module_t *dbgphi = NULL;

static void phi_node_walker(ir_node *node, void *env) {
	if (is_Phi(node) && mode_is_datab(get_irn_mode(node)))
		pset_insert_ptr((pset *)env, node);
}


void be_phi_opt(ir_graph* irg) {
	pset *all_phi_nodes, *all_phi_classes;

	DBG((dbgphi, 1, "\n\n=======================> IRG: %s\n\n", get_entity_name(get_irg_entity(irg))));


	/* get all phi nodes */
	DBG((dbgphi, 1, "-----------------------> Collecting phi nodes <-----------------------\n\n"));
	all_phi_nodes = pset_new_ptr(64);
	irg_walk_graph(irg, phi_node_walker, NULL, all_phi_nodes);


	/* get all phi congruence classes */
	DBG((dbgphi, 1, "-----------------------> Collecting phi classes <---------------------\n\n"));
	all_phi_classes = be_phi_congr_classes(all_phi_nodes);


	/* do some statistics */
#ifdef DO_PHI_STATISTICS
	DBG((dbgphi, 1, "-----------------------> Collecting phi stats <-----------------------\n\n"));
	phi_stat_reset();
	phi_stat_collect(irg, all_phi_nodes, all_phi_classes);
#ifdef DUMP_IRG_PHI_STAT
	{
		char buf[1024];
		snprintf(buf, sizeof(buf), "%s.phistat", get_entity_name(get_irg_entity(irg)));
		//phi_stat_dump(buf);
		phi_stat_dump_pretty(buf);
	}
#endif
#ifdef DUMP_CUMULATIVE
	phi_stat_update(CUMULATIVE_FILE);
#endif
	phi_stat_update(getenv(ENV_PHI_STAT));
#endif


	/* try to coalesce the colors of each phi class */
	DBG((dbgphi, 1, "-----------------------> Coalescing <---------------------------------\n\n"));
	compute_doms(irg);
	be_phi_coalesce(all_phi_classes);
	free_dom_and_peace(irg);
}


void be_phi_opt_init(void) {
	dbgphi = firm_dbg_register("Phi optimizer");
	firm_dbg_set_mask(dbgphi, DEBUG_LVL);

	be_phi_congr_class_init();
	be_phi_coal_init();
}
