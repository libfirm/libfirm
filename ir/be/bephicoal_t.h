/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#ifndef _BEPHICOAL_T_H
#define _BEPHICOAL_T_H

#include "pset.h"
#include "domtree.h"

void be_phi_coal_init(void);
void be_phi_coalesce_locals(pset *all_phi_classes, dominfo_t *dominfo);

#endif
