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
#include "irop.h"
#include "irprog.h"
#include "debug.h"
#include "pset.h"

#include "bephicongr_t.h"
#include "beutil.h"


pset *all_phi_classes = NULL;

size_t phi_irn_data_offset = 0;
static firm_dbg_module_t *dbgmod = NULL;

void be_phi_congr_class_init(void) {
	dbgmod = firm_dbg_register("Phi congruence classes");
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
static INLINE void phi_class_insert(pset *class, ir_node *phi, ir_node *member) {
	DBG((dbgmod, 1, "\tinsert %n in %n\n", member, phi));
	if (!(is_Const(member) && CONSTS_SPLIT_PHI_CLASSES))
		set_phi(member, phi);
	pset_insert_ptr(class, member);
}


/**
 * Unites two phi classes repesented by two phi nodes.
 * @param n Phi node representing phi class to reassign
 * @param new_tgt Phi node, which will hold the new bigger phi class
 */
static void phi_class_union(ir_node *n, ir_node *new_tgt) {
	ir_node *p;
	pset *src, *tgt;

    assert(is_Phi(n) && is_Phi(new_tgt) && "These must be phi nodes.");
	DBG((dbgmod, 1, "\tcorrect %n\n", n));

	/* copy all class members from n to new_tgt. Duplicates eliminated by pset */
	src = get_phi_class(n);
	tgt = get_phi_class(new_tgt);
	for (p = (ir_node *)pset_first(src); p; p = (ir_node *)pset_next(src))
		phi_class_insert(tgt, new_tgt, p);

	/* phi class of n is no longer needed */
	del_pset(src);
	set_phi_class(n, NULL);
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
    DBG((dbgmod, 1, "Det. phi class of %n.\n", curr_phi));

	pc = get_phi_class(curr_phi);
	if (!pc) {
		pc = pset_new_ptr(2);
		set_phi_class(curr_phi, pc);\
		phi_class_insert(pc, curr_phi, curr_phi);
	}

	for (i = 0, n = get_irn_arity(curr_phi); i < n; i++) {
		ir_node *arg, *phi;

		arg = get_irn_n(curr_phi, i);
		DBG((dbgmod, 1, "    Arg %n\n", arg));

		phi = get_phi(arg);
		if (phi == NULL) { /* Argument is not assigned to another phi class. */
			phi_class_insert(pc, curr_phi, arg);
		} else if (phi != curr_phi) {
			assert(!(is_Const(arg) && CONSTS_SPLIT_PHI_CLASSES) && "Const nodes must not have a phi class assigned. See CONSTS_SPLIT_PHI_CLASSES");
			phi_class_union(phi, curr_phi);
		}
	}
	DBG((dbgmod, 1, "Size now: %d\n", pset_count(get_phi_class(curr_phi))));
}


/**
 * Determines the phi congruence classes of
 * all phi nodes in a given pset
 */
void be_phi_congr_classes(pset *phis) {
	int i;
	ir_node *phi;
	pset *phi_class;

	/* determine all phi classes */
	for (i = 0, phi = (ir_node *)pset_first(phis); phi; phi = (ir_node *)pset_next(phis))
		det_phi_congr_class(phi);

	/* store them in a pset for fast retrieval */
	all_phi_classes = pset_new_ptr(64);
	for (i = 0, phi = (ir_node *)pset_first(phis); phi; phi = (ir_node *)pset_next(phis)) {
		phi_class = get_phi_class(phi);
		if (phi_class)
			pset_insert_ptr(all_phi_classes, phi_class);
	}
}
