/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#include <stdlib.h>

#include "bitset.h"
#include "debug.h"
#include "bechordal.h"

#include "bera_t.h"
#include "bephicongr_t.h"
#include "bephicoal_t.h"

static firm_dbg_module_t *dbgphi = NULL;

void be_phi_coal_init(void) {
	dbgphi = firm_dbg_register("Phi coalescing");
	firm_dbg_set_mask(dbgphi, 1);
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


static void coalesce_locals(pset *phi_class, dominfo_t *dominfo) {
	int i, count, phi_count, arity, intf_det, phi_col, allfree;
	pset *pc, *intffree;
	ir_node *phi = NULL, *n, *m;
//	ir_node **members;

	count = pset_count(phi_class);
	pc = clone(phi_class);
//	members = pset_to_array(phi_class);

	/* how many phi nodes are in this class? */
	DBG((dbgphi, 1, "Checking phi count\n"));
	phi_count = 0;
	for (n = (ir_node *)pset_first(pc); n; n = (ir_node *)pset_next(pc)) {
		if (is_Phi(n)) {
			phi = n;
			phi_count++;
		}
	}

	if (phi_count > 1) {
		DBG((dbgphi, 1, "Dropped: Too many phis\n"));
		goto exit;
	}
	assert(phi_count == 1 && phi);


	/* where is the definition of the arguments? */
	DBG((dbgphi, 1, "Checking arg def\n"));
	arity = get_irn_arity(phi);
	for (i = 0; i < arity; ++i) {
        ir_node *block_of_arg, *block_ith_pred;
		ir_node *arg = get_irn_n(phi, i);

		block_of_arg = get_nodes_block(arg);
		block_ith_pred = get_nodes_block(get_irn_n(get_nodes_block(phi), i));

		if (block_of_arg != block_ith_pred) {
			DBG((dbgphi, 1, "Dropped: Arg-def not in pred-block\n"));
			goto exit;
		}
	}


	/* determine a greedy set of non-interfering members
	 * crucial: starting with the phi node
	 */
	DBG((dbgphi, 1, "Building greedy non-interfering set\n"));
	intffree = pset_new_ptr(4);

	pset_remove_ptr(pc, phi);
	pset_insert_ptr(intffree, phi);

	while (m = (ir_node *)pset_first(pc), m) {
		DBG((dbgphi, 1, "Checking %n\n", m));
		pset_break(pc);
		pset_remove_ptr(pc, m);

		intf_det = 0;
		for (n = (ir_node *)pset_first(intffree); n; n = (ir_node *)pset_next(intffree)) {
			DBG((dbgphi, 1, "\t%n", n));
			if (phi_ops_interfere(m, n)) {
				DBG((dbgphi, 1, "\tinterf\n"));
				intf_det = 1;
			} else {
				DBG((dbgphi, 1, "\tclean\n"));
			}
		}

		if (!intf_det) {
			DBG((dbgphi, 1, "Added to set\n"));
			pset_insert_ptr(intffree, m);
		}
	}

	/*
	 * color the non interfering set
	 */
	DBG((dbgphi, 1, "Coloring...\n"));
	phi_col = get_irn_color(phi);
	DBG((dbgphi, 1, "phi-color: %d\n", phi_col));

	/* check if phi color is free in blocks of all members */
	allfree = 1;
	for (n = (ir_node *)pset_first(intffree); n; n = (ir_node *)pset_next(intffree)) {
		ir_node *block;
		bitset_t *used_colors;

		if (n == phi)
			continue;

		block = get_nodes_block(n);

		used_colors = get_ra_block_info(block)->used_colors;

		if (bitset_is_set(used_colors, phi_col)) {
			allfree = 0;
			break;
		}
	}

	if (allfree) {
		for (n = (ir_node *)pset_first(intffree); n; n = (ir_node *)pset_next(intffree))
			set_irn_color(n, phi_col);
		printf("KKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK\n");
	} else {

	}

exit:
	del_pset(pc);
//	free(members);
}


void be_phi_coalesce(pset *all_phi_classes, dominfo_t *dominfo) {
	pset *phi_class;

	for (phi_class = (pset *)pset_first(all_phi_classes); phi_class; phi_class = (pset *)pset_next(all_phi_classes))
		coalesce_locals(phi_class, dominfo);
}
