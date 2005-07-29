/**
 * Author:      Daniel Grund
 * Date:		25.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Performs SSA-Destruction.
 */

#include "bechordal_t.h"

/**
 * Performs SSA-Destruction. Arguments get adjusted, phi nodes just stay.
 */
void be_ssa_destruction(be_chordal_env_t *chordal_env);
void be_ssa_destruction_check(be_chordal_env_t *chordal_env);
