/**
 * @author Daniel Grund
 * @date 04.01.2005
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>

#include "pset.h"
#include "obst.h"
#include "irgraph.h"
#include "irnode.h"
#include "irgwalk.h"
#include "irouts.h"
#include "irdom.h"

#include "beutil.h"
#include "benumb_t.h"
#include "bera_t.h"
#include "bephiopt.h"
#include "phiclass_t.h"
#include "bephicoal_t.h"
#include "bephicoalilp_t.h"
#include "phistat.h"

#define DO_HEURISTIC
#undef DO_OPTIMAL

#define DEBUG_LVL SET_LEVEL_1

/** checks if two interfering nodes have the same color */
#define CHECK_RESULTS
/** counts the copies needed before and after optimization */
#define COUNT_COPY_SAVINGS
/** dumps 2 allocated graphs; before and after opt. */
#undef DUMP_OPT_DIFF

#undef DO_PHI_STATISTICS
#define DUMP_IRG_PHI_STAT
#define DUMP_DIR_PHI_STAT
#define DUMP_ALL_PHI_STAT

#define PHI_STAT_FILE "dir.phistat"
#define ENV_PHI_STAT "PHI_STAT"

static firm_dbg_module_t *dbgphi = NULL;

typedef struct _copies_t {
	int lb, ub;
	int count[3]; /* before, after heuristics, after optimality */
} copies_t;


static void phi_node_walker(ir_node *node, void *env) {
	if (is_Phi(node) && mode_is_datab(get_irn_mode(node)))
		pset_insert_ptr((pset *)env, node);
}


static void node_collector(ir_node *node, void *env) {
	struct obstack *obst = env;
	if (!is_Block(node) && is_allocatable_irn(node))
		obstack_ptr_grow(obst, node);
}


static void check_result(ir_graph *irg) {
	struct obstack ob;
	ir_node **nodes, *n1, *n2;
	int i, o;

	obstack_init(&ob);
	irg_walk_graph(irg, node_collector, NULL, &ob);
	obstack_ptr_grow(&ob, NULL);
	nodes = (ir_node **) obstack_finish(&ob);
	for (i = 0, n1 = nodes[i]; n1; n1 = nodes[++i]) {
		assert(! (is_allocatable_irn(n1) && get_irn_color(n1) == NO_COLOR));
		for (o = i+1, n2 = nodes[o]; n2; n2 = nodes[++o])
			if (phi_ops_interfere(n1, n2) && get_irn_color(n1) == get_irn_color(n2)) {
				DBG((dbgphi, 1, "Ouch!\n   %n[%d]in %n\n   %n[%d] in %n\n", n1, get_irn_graph_nr(n1), get_nodes_block(n1), n2, get_irn_graph_nr(n2), get_nodes_block(n2)));
				assert(0 && "Interfering values have the same color!");
			}
	}

	obstack_free(&ob, NULL);
}


/**
 * Init the copies struct lower and upper bound
 */
static void init_copies(copies_t *c, pset *all_phi_nodes) {
	ir_node *phi;

	c->count[0] = c->count[1] = c->count[2] = -1;
	c->ub = c->lb = 0;

	for (phi = pset_first(all_phi_nodes); phi; phi = pset_next(all_phi_nodes)) {
		/* upper bound: each argument of a phi node is a potential copy */
		int i, max = get_irn_arity(phi);
		c->ub += max;
		/* lower bound: at least all interfering pairs */
		for (i = 0; i < max; ++i) {
			ir_node *arg = get_irn_n(phi, i);
			if (phi_ops_interfere(phi, arg))
				c->lb++;
		}
	}
}


/**
 * Count the copies needed with current coloring
 */
static void count_copies(copies_t *c, pset *all_phi_nodes, int stage) {
	int i, max, phi_color;
	ir_node *phi;

	c->count[stage] = 0;

	for (phi = pset_first(all_phi_nodes); phi; phi = pset_next(all_phi_nodes)) {
		phi_color = get_irn_color(phi);
		for (i = 0, max = get_irn_arity(phi); i < max; ++i) {
			ir_node *arg = get_irn_n(phi, i);
			if (phi_color != get_irn_color(arg)) {
				c->count[stage]++;
				DBG((dbgphi, LEVEL_2, "Copy: %n %n\n", phi, arg));
			}
		}
	}
}


void be_phi_opt(ir_graph* irg) {
	pset *all_phi_nodes, *all_phi_classes;
	copies_t *copies;

	DBG((dbgphi, 1, "\n\n=======================> IRG: %s\n\n", get_entity_name(get_irg_entity(irg))));


	/* get all phi nodes */
	DBG((dbgphi, 1, "-----------------------> Collecting phi nodes <-----------------------\n"));
	all_phi_nodes = pset_new_ptr(64);
	irg_walk_graph(irg, phi_node_walker, NULL, all_phi_nodes);


	/* get all phi congruence classes */
	DBG((dbgphi, 1, "-----------------------> Collecting phi classes <---------------------\n"));
	all_phi_classes = phi_class_compute_by_phis(all_phi_nodes);


	/* do some statistics */
#ifdef DO_PHI_STATISTICS
	DBG((dbgphi, 1, "-----------------------> Collecting phi stats <-----------------------\n"));
	phi_stat_reset();
	phi_stat_collect(irg, all_phi_nodes, all_phi_classes);
#ifdef DUMP_IRG_PHI_STAT
		char buf[1024];
		snprintf(buf, sizeof(buf), "%s.phistat", get_entity_name(get_irg_entity(irg)));
		/*phi_stat_dump(buf);*/
		phi_stat_dump_pretty(buf);
#endif
#ifdef DUMP_DIR_PHI_STAT
		phi_stat_update(PHI_STAT_FILE);
#endif
#ifdef DUMP_ALL_PHI_STAT
		phi_stat_update(getenv(ENV_PHI_STAT));
#endif
#endif


	/* try to coalesce the colors of each phi class */
	DBG((dbgphi, 1, "-----------------------> Coalescing <---------------------------------\n"));
	compute_outs(irg);
	compute_doms(irg);


#ifdef DUMP_OPT_DIFF
	dump_allocated_irg(irg, "-before");
#endif
#ifdef CHECK_RESULTS
	check_result(irg);
#endif
#ifdef COUNT_COPY_SAVINGS
	copies = malloc(sizeof(*copies));
	init_copies(copies, all_phi_nodes);
	DBG((dbgphi, 0, "Copy bounds  : %3d < . < %3d\n", copies->lb, copies->ub));
	count_copies(copies, all_phi_nodes, 0);
#endif



#ifdef DO_HEURISTIC
	be_phi_coalesce(all_phi_classes);
#ifdef DUMP_OPT_DIFF
	dump_allocated_irg(irg, "-heur");
#endif
#ifdef CHECK_RESULTS
	check_result(irg);
#endif
#ifdef COUNT_COPY_SAVINGS
	count_copies(copies, all_phi_nodes, 1);
#endif
#endif /* DO_HEURISTIC */


	if (copies->count[1] == -1 || copies->count[1] > copies->lb) {
#ifdef DO_OPTIMAL
		be_phi_coalesce_ilp(irg, all_phi_nodes);
#ifdef DUMP_OPT_DIFF
		dump_allocated_irg(irg, "-opt");
#endif
#ifdef CHECK_RESULTS
		check_result(irg);
#endif
#ifdef COUNT_COPY_SAVINGS
		count_copies(copies, all_phi_nodes, 2);
#endif
#endif /* DO_OPTIMAL */
	}
#ifdef COUNT_COPY_SAVINGS
	DBG((dbgphi, 0, "Copies before/heur/opt: %3d / %3d / %3d\n", copies->count[0], copies->count[1], copies->count[2]));
#endif
	free_dom_and_peace(irg);
}


void be_phi_opt_init(void) {
	dbgphi = firm_dbg_register("ir.be.phiopt");
	firm_dbg_set_mask(dbgphi, DEBUG_LVL);

	phi_class_init();
#ifdef DO_HEURISTIC
	be_phi_coal_init();
#endif
#ifdef DO_OPTIMAL
	be_phi_coal_ilp_init();
#endif
}
