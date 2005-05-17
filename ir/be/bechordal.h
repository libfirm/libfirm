
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

typedef struct _be_chordal_env_t be_chordal_env_t;

/**
 * Allocate registers for an ir graph.
 * @param irg The graph.
 * @return Some internal data to be freed with be_ra_chordal_done().
 */
be_chordal_env_t *be_ra_chordal(ir_graph *irg,
    const arch_env_t *arch_env,
    const arch_register_class_t *cls);

/**
 * Free data from the chordal register allocation.
 * @param irg The graph.
 */
void be_ra_chordal_done(be_chordal_env_t *info);

/**
 * Init some things for the chordal register allocator.
 * This must be called before Firm is inited.
 */
void be_ra_chordal_init(void);

#endif
