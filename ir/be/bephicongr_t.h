/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#ifndef _BEPHICONGR_T_H
#define _BEPHICONGR_T_H

#include "pset.h"
#include "irnode.h"

typedef struct _phi_info_t {
	ir_node *phi;		/**< For all nodes of a phi class points to a phi node
							 representing the phi class and holding the ptr to
							 the pset phi_class      or NULL */
	pset *phi_class;	/**< Only set in phi nodes. A set containing the members
							 of the phi congruence class this phi node represents */
} phi_info_t;

extern size_t phi_irn_data_offset;

/**
 * Setting this to 0 will treat const nodes like
 * all other nodes when computing phi congruence classes.
 * A non zero value results in splitting phi congruence
 * classes at all const nodes (except they do share
 * some non-const nodes too)
 */
#define CONSTS_SPLIT_PHI_CLASSES 0
#define get_irn_phi_info(irn)   get_irn_data(irn, phi_info_t, phi_irn_data_offset)
#define get_phi_class(irn)      get_irn_phi_info(irn)->phi_class /* Only for phi nodes */

void be_phi_congr_class_init(void);
pset *be_phi_congr_classes(pset *all_phi_nodes);


#endif
