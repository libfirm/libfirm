/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "irgraph.h"
#include "irnode.h"
#include "irgwalk.h"
#include "irop.h"
#include "irprog.h"
#include "irprintf_t.h"
#include "debug.h"
#include "pset.h"

#include "bephicongr_t.h"
#include "beutil.h"
#include "bechordal.h"


size_t phi_irn_data_offset = 0;
firm_dbg_module_t *dbgmod = NULL;

void be_phi_congr_class_init(void) {
	dbgmod = firm_dbg_register("Phi congr. classes");
	firm_dbg_set_mask(dbgmod, 1);
	phi_irn_data_offset = register_additional_node_data(sizeof(phi_info_t));
}

#define get_phi(irn)            get_irn_phi_info(irn)->phi
#define set_phi(irn, p)         get_irn_phi_info(irn)->phi = p
#define set_phi_class(irn, cls) get_irn_phi_info(irn)->phi_class = cls

#define is_Const(n)       (get_irn_opcode(n) == iro_Const)

/**
 * Insert a node to the phi class of a phi node
 * @param class The phi congruence class
 * @param phi The phi node holding the class
 * @param arg Node which gets assigned to the class
 */
static void inline phi_class_insert(pset *class, ir_node *phi, ir_node *arg) {
	/*DBG((dbgmod, 1, "\tinsert %n in %n\n", arg, phi));*/
	ir_debugf("\tinsert %n in %n\n", arg, phi);
	if (!(is_Const(arg) && CONSTS_SPLIT_PHI_CLASSES))
		set_phi(arg, phi);
	pset_insert_ptr(class, arg);
}


/**
 * Assigns all operands of a phi node
 * to a (new) phi congruence class
 * @param n Phi node, of which all args get reassigned
 * @param new_tgt Phi node representing new phi congruence class
 */
static void phi_class_correct(ir_node *n, ir_node *new_tgt) {
	ir_node *p;
	pset *src, *tgt;

    assert(is_Phi(n) && is_Phi(new_tgt) && "These must be phi nodes.");
	ir_debugf("\tcorrect %n\n", n);

	/* copy all class members from n to new_tgt. Duplicates eliminated by pset */
	src = get_phi_class(n);
	tgt = get_phi_class(new_tgt);
	for (p = (ir_node *)pset_first(src); p; p = (ir_node *)pset_next(src))
		phi_class_insert(tgt, new_tgt, p);

	/* phi class of n is no longer needed */
	set_phi_class(n, NULL);
	del_pset(src);
}


/**
 * Determines the phi congruence class of a phi node.
 * This will assign a phi class to all operands of this
 * phi node. Exceptions may be const nodes: See CONSTS_SPLIT_PHI_CLASSES
 */
static void det_phi_congr_class(ir_node *curr_phi) {
	pset *pc;
    int i, n;
    assert(is_Phi(curr_phi) && "This must be a phi node.");
    ir_debugf("Det. phi class of %n.\n", curr_phi);

	pc = get_phi_class(curr_phi);
	if (!pc) {
		pc = pset_new_ptr(2);
		set_phi_class(curr_phi, pc);\
		phi_class_insert(pc, curr_phi, curr_phi);
	}

	for (i = 0, n = get_irn_arity(curr_phi); i < n; i++) {
		ir_node *arg, *phi;

		arg = get_irn_n(curr_phi, i);
		ir_debugf("    Arg %n\n", arg);

		phi = get_phi(arg);
		if (phi == NULL) { /* Argument is not assigned to another phi class. */
			phi_class_insert(pc, curr_phi, arg);
		} else if (phi != curr_phi) {
			assert(!is_Const(arg) && "Const nodes must not have a phi class assigned");
			phi_class_correct(phi, curr_phi);
		}
	}
	ir_debugf("Size now: %d\n", get_phi_class_size(curr_phi));
}

/**
 * Determines the liveness interference information
 * of a phi congruence class.
 */
static void det_interference(ir_node *n) {
	pset *pc;
	ir_node **members, *p;
	int count, i, o;
	int doit;

	pc = get_phi_class(n);
	if (!pc)
		return;

	count = pset_count(pc);
	members = (ir_node **) malloc(count * sizeof(ir_node*));

	ir_debugf("\nChecking phi class of node %n:\n", n);
	for (i=0, p = (ir_node *)pset_first(pc); p; p = (ir_node *)pset_next(pc))
		members[i++] = p;
	assert(i == count);

	//determine interference of phi args
	for (i = 0; i < count-1; ++i) {
		doit = 1;
		for (o = i+1; o < count; ++o) {
			ir_debugf("Checking %n\t%n", members[i], members[o]);
			if (phi_ops_interfere(members[i], members[o])) {
				ir_debugf("\tinterfere\n");
				get_irn_phi_info(n)->interf_pairs++;
				if (doit) {
					get_irn_phi_info(n)->interf_vals++;
					doit = 0;
				}

			} else {
				ir_debugf("\tclean\n");
			}
		}
	}

	free(members);
}


static void phi_class_walker(ir_node *node, void *env) {
	if (!(is_Phi(node) && mode_is_datab(get_irn_mode(node)))) return;
	det_phi_congr_class(node);
}


static void phi_class_inteference_walker(ir_node *node, void *env) {
	if (!(is_Phi(node) && mode_is_datab(get_irn_mode(node)))) return;
	det_interference(node);
}


void be_phi_congr_classes(ir_graph *irg) {
	irg_walk_graph(irg, phi_class_walker, NULL, NULL);
	irg_walk_graph(irg, phi_class_inteference_walker, NULL, NULL);
}
