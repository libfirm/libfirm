/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Base routines for register allocation.
 * @author      Sebastian Hack
 * @date        13.01.2005
 */
#ifndef FIRM_BE_BERA_H
#define FIRM_BE_BERA_H

#include "firm_types.h"

typedef struct be_ra_t {
	void (*allocate)(ir_graph *irg);   /**< allocate registers on a graph */
} be_ra_t;

void be_register_allocator(const char *name, be_ra_t *allocator);

/**
 * Do register allocation with currently selected register allocator
 */
void be_allocate_registers(ir_graph *irg);

#endif
