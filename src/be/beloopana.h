/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Compute register pressure in loops.
 * @author      Christian Wuerdig
 * @date        20.02.2007
 */
#ifndef FIRM_BE_BELOOPANA_H
#define FIRM_BE_BELOOPANA_H

#include "be_types.h"
#include "irloop.h"

typedef struct be_loopana_t be_loopana_t;

/**
 * Compute the register pressure for a class of all loops in the irg.
 * @param irg   The graph
 * @param cls   The register class to compute the pressure for
 * @return The loop analysis object.
 */
be_loopana_t *be_new_loop_pressure(ir_graph *irg, arch_register_class_t const *cls);

/**
 * Returns the computed register pressure for the given class and loop.
 * @return The pressure or INT_MAX if not found
 */
unsigned be_get_loop_pressure(be_loopana_t *loop_ana,
                              const arch_register_class_t *cls, ir_loop *loop);

/**
 * Frees loop analysis object.
 */
void be_free_loop_pressure(be_loopana_t *loop_ana);

#endif
