/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#ifndef _PHISTAT_H
#define _PHISTAT_H

#include "pset.h"
#include "irgraph.h"

void phi_stat_reset(void);
void phi_stat_collect(ir_graph *irg, pset *all_phi_nodes, pset *all_phi_classes);
void phi_stat_dump(char *filename);
void phi_stat_update(char *filename);
void phi_stat_dump_pretty(char *filename);

#endif
