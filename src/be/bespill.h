/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Option handling for spiller.
 * @author      Matthias Braun
 * @date        12.10.2006
 */
#ifndef FIRM_BE_BESPILL_H
#define FIRM_BE_BESPILL_H

#include "bera.h"
#include <stdbool.h>

extern bool be_coalesce_spill_slots;
extern bool be_do_remats;

typedef void (*be_spill_func)(ir_graph *irg, const arch_register_class_t *cls,
							  const regalloc_if_t *regif);

/**
 * Register a new spill algorithm.
 *
 * @param name     the name of the spill algorithm,
 *                 used to select it
 * @param spiller  a spill entry
 */
void be_register_spiller(const char *name, be_spill_func spiller);

/**
 * Execute the selected spill algorithm
 *
 * @param irg   the graph to spill on
 * @param cls   the register class to spill
 */
void be_do_spill(ir_graph *irg, const arch_register_class_t *cls,
				 const regalloc_if_t *regif);

#endif
