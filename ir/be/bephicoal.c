/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#include <stdlib.h>

#include "debug.h"
#include "bechordal.h"

#include "bera_t.h"
#include "bephicongr_t.h"
#include "bephicoal_t.h"


firm_dbg_module_t *dbgmod = NULL;


void be_phi_coal_init(void) {
	dbgmod = firm_dbg_register("Phi coalescing");
	firm_dbg_register("Testsdflkjsdf");
	firm_dbg_set_mask(dbgmod, 1);
	DBG((dbgmod, 1, "Phi coalescing dbg enabled"));
}


static INLINE ir_node **pset_to_array(pset *theset) {
	ir_node **res, *p;
	int i, count;

	count = pset_count(theset);
	res = (ir_node **) malloc(count * sizeof(ir_node*));
	for (i = 0, p = (ir_node *)pset_first(theset); p; p = (ir_node *)pset_next(theset))
		res[i++] = p;
	assert(i == count);

	return res;
}


static INLINE pset *clone(pset *theset) {
	void *p;
	pset *res;

	res = pset_new_ptr(8);
	for (p = pset_first(theset); p; p = pset_next(theset))
		pset_insert_ptr(res, p);

	return res;
}


static void coalesce_locals(pset *phi_class) {
	int i, count, phi_count, arity, intf_det, phi_col;
	pset *pc, *intffree;
	ir_node *phi, *n, *m;
//	ir_node **members;

	count = pset_count(phi_class);
	pc = clone(phi_class);
//	members = pset_to_array(phi_class);

	/* how many phi nodes are in this class? */
	DBG((dbgmod, 1, "Checking phi count\n"));
	phi_count = 0;
	for (n = (ir_node *)pset_first(pc); n; n = (ir_node *)pset_next(pc)) {
		if (is_Phi(n)) {
			phi = n;
			phi_count++;
		}
	}

	if (phi_count > 1) {
		DBG((dbgmod, 1, "Dropped: Too many phis\n"));
		goto exit;
	}
	assert(phi_count == 1 && phi);


	/* where is the definition of the arguments? */
	DBG((dbgmod, 1, "Checking arg def\n"));
	arity = get_irn_arity(phi);
	for (i = 0; i < arity; ++i) {
        ir_node *block_of_arg, *block_ith_pred;
		ir_node *arg = get_irn_n(phi, i);

		/* TODO: check next few lines after const node copy placement */
//		if (iro_Const == get_irn_opcode(arg) && CONSTS_SPLIT_PHI_CLASSES)
//			continue;

		block_of_arg = get_nodes_block(arg);
		block_ith_pred = get_nodes_block(get_irn_n(get_nodes_block(phi), i));

		if (block_of_arg != block_ith_pred) {
			DBG((dbgmod, 1, "Dropped: Arg-def not in pred-block\n"));
			goto exit;
		}
	}


	/* determine a greedy set of non-interfering members
	 * crucial: starting with the phi node
	 */
	DBG((dbgmod, 1, "Building greedy non-intfering set\n"));
	intffree = pset_new_ptr(4);

	pset_remove_ptr(pc, phi);
	pset_insert_ptr(intffree, phi);

	while (m = (ir_node *)pset_first(pc), m) {
		DBG((dbgmod, 1, "Checking %n\n", m));
		pset_break(pc);
		pset_remove_ptr(pc, m);

		intf_det = 0;
		for (n = (ir_node *)pset_first(intffree); n; n = (ir_node *)pset_next(intffree)) {
			DBG((dbgmod, 1, "\t%n", n));
			if (phi_ops_interfere(m, n)) {
				DBG((dbgmod, 1, "\tinterf\n"));
				intf_det = 1;
			} else {
				DBG((dbgmod, 1, "\tclean\n"));
			}
		}

		if (!intf_det) {
			DBG((dbgmod, 1, "Added to set\n"));
			pset_insert_ptr(intffree, m);
		}
	}

	/*
	 * color the intffree set
	 */
	DBG((dbgmod, 1, "Coloring...\n"));
	phi_col = get_irn_color(phi);
	DBG((dbgmod, 1, "phi-color: %d\n", tgt_col));



exit:
	del_pset(pc);
//	free(members);
}


void be_phi_coalesce_locals(pset *all_phi_classes) {
	pset *phi_class;

	DBG((dbgmod, 1, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"));

	/* determine all phi classes */
	for (phi_class = (pset *)pset_first(all_phi_classes); phi_class; phi_class = (pset *)pset_next(all_phi_classes))
		coalesce_locals(phi_class);
}
