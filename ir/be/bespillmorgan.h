/*
 * Author:      Matthias Braun
 * Date:		05.05.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BESPILLMORGAN_H_
#define BESPILLMORGAN_H_

#include "be_t.h"
#include "bechordal.h"

void be_spill_morgan(be_irg_t *birg, const arch_register_class_t *cls);

#endif
