/**
 * Chordal register allocation.
 * @author Christian Wuerdig
 * @date 2005/12/14
 * @cvsid $Id$
 */
#ifndef _BELOWER_H_
#define _BELOWER_H_

#include "beirg.h"

void assure_constraints(be_irg_t *birg);
void lower_nodes_after_ra(be_irg_t *birg, int do_copy);

#endif /* _BELOWER_H_ */
