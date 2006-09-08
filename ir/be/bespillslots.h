/*
 * Author:      Matthias Braun
 * Date:		27.7.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BESPILLSLOTS_H_
#define BESPILLSLOTS_H_

#include "bechordal.h"

/**
 * Computes the spill offsets for all spill nodes in the irg
 */
void be_coalesce_spillslots(const be_chordal_env_t *chordal_env, int coalesce_spillslots);

#endif
