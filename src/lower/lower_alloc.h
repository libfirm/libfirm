/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower (stack-) Alloc nodes to allocate an aligned number of bytes
 * @author  Matthias Braun
 */
#ifndef FIRM_LOWER_ALLOC_H
#define FIRM_LOWER_ALLOC_H

#include <stdbool.h>
#include "firm_types.h"

/**
 * Lower Alloc nodes: Ensure that alloc sizes are a multiple of a specified
 * alignment.
 * @param irg                  graph to process
 * @param po2_stack_alignment  stack should be aligned to 2**po2_stack_alignment
 */
void lower_alloc(ir_graph *irg, unsigned po2_stack_alignment);

#endif
