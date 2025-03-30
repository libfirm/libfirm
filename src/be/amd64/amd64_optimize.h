/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Peephole optimizations.
 */
#ifndef FIRM_BE_AMD64_AMD64_OPTIMIZE_H
#define FIRM_BE_AMD64_AMD64_OPTIMIZE_H

#include "firm_types.h"

/**
 * Perform peephole optimizations an a graph.
 *
 * @param irg  the graph
 */
void amd64_peephole_optimization(ir_graph *irg);

#endif
