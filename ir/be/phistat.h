/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#ifndef _PHISTAT_H
#define _PHISTAT_H

#include "pset.h"
#include "irgraph.h"

/**
 * Resets the array holding the data
 */
void phi_stat_reset(void);

/**
 * Collect all stat data
 */
void phi_stat_collect(ir_graph *irg, pset *all_phi_nodes, pset *all_phi_classes);

/**
 * Dumps the current contents of the internal values to a file.
 */
void phi_stat_dump(char *filename);

/**
 * Updates a cumulative file with the internal values.
 */
void phi_stat_update(char *filename);

/**
 * Dumps the current contents of the values array
 * and annotations to a file.
 */
void phi_stat_dump_pretty(char *filename);

#endif
