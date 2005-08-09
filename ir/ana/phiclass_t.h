/**
 * Analysis to conmpute phi congruence classes.
 * @author Daniel Grund
 * @date 09.08.2005
 */

#ifndef _BEPHICONGR_T_H
#define _BEPHICONGR_T_H

#include "phiclass.h"

typedef struct _phi_info_t {
	pset *phi_class;	/**< A set containing all members of the
							phi congruence class of this irn */
} phi_info_t;

extern size_t phi_irn_data_offset;

#define get_irn_phi_info(irn)   get_irn_data(irn, phi_info_t, phi_irn_data_offset)
#define _get_phi_class(irn)     get_irn_phi_info(irn)->phi_class
#define _set_phi_class(irn,cls) get_irn_phi_info(irn)->phi_class = cls

/**
 * Computes all phi classes of an irg. All phi nodes of this irg must be
 * contained in @p all_phi_nodes. Otherwise the results may be wrong.
 * @param all_phi_nodes All phi nodes of an irg.
 * @return A set containing all phi classes as psets
 */
pset *phi_class_compute_by_phis(pset *all_phi_nodes);


#endif
