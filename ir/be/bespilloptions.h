/*
 * Author:      Matthias Braun
 * Date:		12.10.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BESPILL_OPTIONS_H_
#define BESPILL_OPTIONS_H_

#include "bechordal.h"

extern int be_coalesce_spill_slots;
extern int be_do_remats;

typedef struct be_spiller_t {
	void (*spill) (be_irg_t *birg, const arch_register_class_t* cls);
} be_spiller_t;
void be_register_spiller(const char *name, be_spiller_t *spiller);

void be_do_spill(be_irg_t *birg, const arch_register_class_t* cls);

#endif
