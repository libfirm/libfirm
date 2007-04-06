/**
 * Author:      Daniel Grund, Matthias Braun
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

/**
 * Same as be_spill_belady but reuses an existing spill environment.
 * This is usefull for "pre-spillers" that create some spills+reloads
 * but can't ensure that regpressure never exceeds the number of registers
 */
void be_spill_belady_spill_env(be_irg_t *birg, const arch_register_class_t *cls,
                               spill_env_t *spill_env);

#endif /*BESPILLBELADY_H_*/
