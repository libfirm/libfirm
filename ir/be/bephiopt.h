/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#ifndef _BEPHIOPT_H
#define _BEPHIOPT_H

#include "debug.h"
#include "irgraph.h"

void be_phi_opt_init(void);
void be_phi_opt(ir_graph* irg);

#endif
