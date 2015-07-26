/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the x87 support and virtual to stack
 *              register translation for the ia32 backend.
 * @author      Michael Beck
 */
#ifndef FIRM_BE_IA32_X86_X87_H
#define FIRM_BE_IA32_X86_X87_H

#include "firm_types.h"

/**
 * Run a simulation and fix all virtual instructions for a graph.
 * Replaces all virtual floating point instructions and registers
 * by real ones.
 *
 * @param irg      the graph to simulate and patch
 *
 * Registers must be allocated.
 */
void x86_x87_simulate_graph(ir_graph *irg);

/**
 * Initializes the x87 simulator.
 */
void x86_init_x87(void);

#endif
