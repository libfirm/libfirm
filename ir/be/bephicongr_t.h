/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#ifndef _BEPHICONGR_T_H
#define _BEPHICONGR_T_H

#include "irnode.h"
#include "pset.h"


typedef struct _phi_info_t {
	ir_node *phi;      			   /* only set in args of phi nodes (which could be a phi itslef). Points to a phi node or NULL */
	pset *phi_class;               /* only set in phi nodes. A set containing the members of the phi congruence class this phi node belongs to */
} phi_info_t;

extern size_t phi_irn_data_offset;

/**
 * Setting this to 0 will treat const nodes like
 * all other nodes when computing phi congruence classes.
 * A non zero value results in splitting phi congruence
 * classes at all const nodes (except they do share
 * some non-const nodes too)
 *
 * A non zero value can only be set if copies of const
 * nodes are placed correctly.
 */
#define CONSTS_SPLIT_PHI_CLASSES 1
#define get_irn_phi_info(irn)   get_irn_data(irn, phi_info_t, phi_irn_data_offset)
#define get_phi_class(irn)      get_irn_phi_info(irn)->phi_class /* Only for phi nodes */

void be_phi_congr_class_init(void);
pset *be_phi_congr_classes(pset *all_phi_nodes);


#endif
