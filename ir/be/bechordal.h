
/**
 * Chordal register allocation.
 * @author Sebastian Hack
 * @date 14.12.2004
 */

#ifndef __BECHORDAL_H
#define __BECHORDAL_H

/**
 * Allocate registers for an ir graph.
 * @param irg The graph.
 * @return Some internal data to be freed with be_ra_chordal_free().
 */
void be_ra_chordal(ir_graph *irg);

void be_ra_chordal_free(ir_graph *irg);

#endif
