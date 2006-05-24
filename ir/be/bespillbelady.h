/**
 * Author:      Daniel Grund
 * Date:		20.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef BESPILLBELADY_H_
#define BESPILLBELADY_H_

#include "be_t.h"
#include "bechordal.h"

#include "bearch.h"
#include "bespill.h"

void be_spill_belady(const be_chordal_env_t *env);
/// Same as be_spill_belady but reuses an existing spill enviornment
void be_spill_belady_spill_env(const be_chordal_env_t *env, spill_env_t *spill_env);

#endif /*BESPILLBELADY_H_*/
