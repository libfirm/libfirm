/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#ifndef _BEPHICONGR_T_H
#define _BEPHICONGR_T_H

#include "irnode.h"
#include "pset.h"

/**
 * Setting this to 0 will treat const nodes like
 * all other nodes when computing phi congruence classes.
 * A non zero value results in splitting phi congruence
 * classes at all const nodes (except they do share
 * some non-const nodes too)
 */
#define CONSTS_SPLIT_PHI_CLASSES 1


typedef struct _phi_info_t {
	ir_node *phi;      			   /* only set in args of phi nodes (which could be a phi itslef). Points to a phi node or NULL */
	pset *phi_class;               /* only set in phi nodes. A set containing the members of the phi congruence class this phi node belongs to */
	int interf_pairs;
	int interf_vals;
} phi_info_t;

extern size_t phi_irn_data_offset;

#define get_irn_phi_info(irn)   get_irn_data(irn, phi_info_t, phi_irn_data_offset)
/* Only for phi nodes */
#define get_phi_class(irn)      get_irn_phi_info(irn)->phi_class
#define get_phi_class_size(irn) (get_phi_class(irn)==NULL ? 0 : pset_count(get_phi_class(irn)))

void be_phi_congr_class_init(void);

/**
 * Determines the phi congruence classes of
 * all phi nodes in a graph
 */
void be_phi_congr_classes(ir_graph *irg);

#endif
