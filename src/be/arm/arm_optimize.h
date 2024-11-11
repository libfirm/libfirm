/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements several optimizations for ARM.
 * @author      Michael Beck
 */
#ifndef FIRM_BE_ARM_ARM_OPTIMIZE_H
#define FIRM_BE_ARM_ARM_OPTIMIZE_H

#include "firm_types.h"
#include <stdbool.h>
#include <stdint.h>

/**
 * Performs Peephole Optimizations an a graph.
 *
 * @param irg   the graph
 */
void arm_peephole_optimization(ir_graph *irg);

typedef struct arm_vals {
	unsigned ops;
	uint8_t values[4];
	uint8_t rors[4];
} arm_vals;

/**
 * construct 8bit values and rot amounts for a 32bit value.
 */
void arm_gen_vals_from_word(uint32_t value, arm_vals *result);

bool arm_is_valid_offset(int32_t v, ir_mode *mode, bool is_store);

#endif
