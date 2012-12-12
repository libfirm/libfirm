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

#include "irgraph.h"
#include "bearch_arm_t.h"

typedef struct arm_vals {
	int ops;
	unsigned char values[4];
	unsigned char rors[4];
} arm_vals;

/**
 * Encodes an immediate with shifter operand
 */
unsigned int arm_encode_imm_w_shift(unsigned int shift, unsigned int immediate);

/**
 * Decode an immediate with shifter operand
 */
unsigned int arm_decode_imm_w_shift(long imm_value);

/**
 * construct 8bit values and rot amounts for a 32bit value.
 */
void arm_gen_vals_from_word(unsigned int value, arm_vals *result);

/**
 * Performs Peephole Optimizations an a graph.
 *
 * @param irg   the graph
 */
void arm_peephole_optimization(ir_graph *irg);

#endif
