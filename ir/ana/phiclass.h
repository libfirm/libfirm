/**
 * Analysis to compute phi congruence classes.
 * @author Daniel Grund
 * @cvsid  $Id$
 * @date   15.01.2005
 */

#ifndef _PHICLASS_H_
#define _PHICLASS_H_

#include "pset.h"
#include "irgraph.h"
#include "irnode.h"
#include "irnodeset.h"

typedef struct _phi_classes_t phi_classes_t;

/**
 * Return the array containing all nodes assigned to the same Phi class as @p irn.
 */
ir_node **get_phi_class(phi_classes_t *pc, ir_node *irn);

/**
 * Assigns a new array of nodes representing the new Phi class to @p irn.
 */
void set_phi_class(phi_classes_t *pc, ir_node *irn, ir_node **cls);

/**
 * Returns a set containing all computed Phi classes.
 */
pset *get_all_phi_classes(phi_classes_t *pc);

/**
 * Builds the Phi classes for all Phis in @p irg.
 * @param irg               The irg the classes should be build for
 * @param pure_phi_classes  Set to one if Phi classes should only contain Phi nodes.
 *                          Beware: This might result in different (more) Phi classes as if not set
 * @return The Phi class object for the @p irg.
 */
phi_classes_t *phi_class_new_from_irg(ir_graph *irg, int pure_phi_classes);

/**
 * Builds all Phi classes for the given set of Phis.
 * @param irg               The irg, the Phis are from
 * @param all_phis          An ir_nodeset containing all Phis nodes to build the classes for
 * @param pure_phi_classes  Set to one if Phi classes should only contain Phi nodes.
 *                          Beware: This might result in different (more) Phi classes as if not set
 * @return The Phis class object for @p all_phis.
 */
phi_classes_t *phi_class_new_from_set(ir_graph *irg, ir_nodeset_t *all_phis, int pure_phi_classes);

/**
 * Free all allocated data.
 */
void phi_class_free(phi_classes_t *pc);

#endif /* _PHICLASS_H_ */
