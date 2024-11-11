/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements several optimizations for IA32.
 * @author      Christian Wuerdig
 */
#ifndef FIRM_BE_IA32_IA32_OPTIMIZE_H
#define FIRM_BE_IA32_IA32_OPTIMIZE_H

#include "firm_types.h"

/**
 * Performs conv and address mode optimizations.
 * @param cg  The ia32 codegenerator object
 */
void ia32_optimize_graph(ir_graph *irg);

/**
 * Performs Peephole Optimizations an a graph.
 *
 * @param irg   the graph
 * @param cg    the code generator object
 */
void ia32_peephole_optimization(ir_graph *irg);

/** Initialize the ia32 address mode optimizer. */
void ia32_init_optimize(void);

#endif
