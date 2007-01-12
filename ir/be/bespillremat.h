/**
 * @file   bespillremat.h
 * @date   2006-04-06
 * @author Adam M. Szalkowski
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef BESPILLREMAT_H_
#define BESPILLREMAT_H_
#include "bechordal.h"

void be_spill_remat(be_irg_t *birg, const arch_register_class_t *cls);

#endif /*BESPILLREMAT_H_*/
