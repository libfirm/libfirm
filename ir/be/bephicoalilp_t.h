/**
 * @author Daniel Grund
 * @date 11.03.2005
 */

#ifndef _BEPHICOALILP_T_H
#define _BEPHICOALILP_T_H

#include "irgraph.h"
#include "pset.h"

void be_phi_coal_ilp_init(void);
void be_phi_coalesce_ilp(ir_graph *irg, pset *all_phi_nodes);

#endif
