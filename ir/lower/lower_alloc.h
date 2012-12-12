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
 * Lower Alloc/Free nodes: This changes them to allocate bytes instead of
 * objects of a certain type. It can also make sure that the resulting
 * size is aligned.
 */
void lower_alloc(ir_graph *irg, unsigned stack_alignment,
                 bool align_constant_sizes,
                 long addr_delta);

#endif
