/**
 * Analysis to compute phi congruence classes.
 * @author Daniel Grund
 * @date 15.01.2005
 */

#ifndef _BEPHICONGR_H
#define _BEPHICONGR_H

#include "pset.h"
#include "irgraph.h"
#include "irnode.h"

/**
 * Initialize data structures
 */
void phi_class_init(void);

/**
 * Computes all phi classes of an irg.
 * @param irg The ir-graph to compute the classes for.
 * @return Sets the internal data structures.
 */
void phi_class_compute(ir_graph *irg);

/**
 * Throws away all allocated memory for phi classes of an irg.
 * @param irg The ir-graph to free recources for.
 * @return Frees the internal data structures.
 */
void phi_class_free(ir_graph *irg);

/**
 * @param irn A node to get the phi class for
 * @return A pset containing all members of the phi class @p irn belongs to.
 *         If @p irn is not member of a phi class NULL is returned.
 */
pset *get_phi_class(const ir_node *irn);

#endif
