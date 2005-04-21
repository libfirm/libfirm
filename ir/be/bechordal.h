
/**
 * Chordal register allocation.
 * @author Sebastian Hack
 * @date 14.12.2004
 */

#ifndef __BECHORDAL_H
#define __BECHORDAL_H

#include "irgraph.h"
#include "irnode.h"

#include "bearch.h"

/**
 * Allocate registers for an ir graph.
 * @param irg The graph.
 * @return Some internal data to be freed with be_ra_chordal_done().
 */
void be_ra_chordal(ir_graph *irg,
    const arch_isa_if_t *isa,
    const arch_register_class_t *cls);

/**
 * Free data from the chordal register allocation.
 * @param irg The graph.
 */
void be_ra_chordal_done(ir_graph *irg);

/**
 * Init some things for the chordal register allocator.
 * This must be called before Firm is inited.
 */
void be_ra_chordal_init(void);

#endif
