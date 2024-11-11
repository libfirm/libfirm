/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       lower mode_b operations to something the backend can handle
 * @author      Matthias Braun, Christoph Mallon
 *
 * Most machines can't really manipulate mode_b values (which are usually
 * modeled as cpu flags). So you often have to convert them into machine words
 * with the values 0/1 and operate on them instead.
 *
 * After this pass the following holds:
 *   - The only inputs with mode_b are for the Cond node and the Sel input of
 *     a Mux node.
 *   - The only nodes producing mode_b are: Cmp
 */
#ifndef FIRM_LOWER_MODE_B_H
#define FIRM_LOWER_MODE_B_H

#include "firm_types.h"

/**
 * Lowers mode_b operations to integer arithmetic. After the lowering the only
 * operations with mode_b are the Projs of Cmps; the only nodes with mode_b
 * inputs are Cond and Mux nodes.
 *
 * Example: Mux(a < 0, 0, 1) => a >> 31
 *
 * @param irg           the firm graph to lower
 * @param lowered_mode  mode that is used to transport 0/1 values
 */
void ir_lower_mode_b(ir_graph *irg, ir_mode *lowered_mode);

#endif
