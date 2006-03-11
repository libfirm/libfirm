/**
 * Chordal register allocation.
 * @author Christian Wuerdig
 * @date 2005/12/14
 * @cvsid $Id$
 */

#ifndef _BELOWER_H_
#define _BELOWER_H_

#include "bechordal.h"

void lower_nodes_before_ra(be_chordal_env_t *chord_env);
void lower_nodes_after_ra(be_chordal_env_t *chord_env, int do_copy);

#endif /* _BELOWER_H_ */
