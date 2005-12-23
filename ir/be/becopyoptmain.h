/**
 * Author:      Daniel Grund
 * Date:		11.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.

 * Main file for the optimization reducing the copies needed for:
 * - Phi coalescing
 * - Register-constrained nodes
 * - Two-address code instructions
 */

#ifndef _BECOPYOPTMAIN_H
#define _BECOPYOPTMAIN_H

#include "irgraph.h"
#include "bearch.h"

#include "bechordal.h"

void be_copy_opt_init(void);
void be_copy_opt(be_chordal_env_t *chordal_env);

#endif
