/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Implements several optimizations for ARM.
 * @author      Michael Beck
 * @version     $Id: $
 */
#ifndef FIRM_BE_ARM_ARM_OPTIMIZE_H
#define FIRM_BE_ARM_ARM_OPTIMIZE_H

#include "irgraph.h"
#include "bearch_arm_t.h"

typedef struct arm_vals {
	int ops;
	unsigned char values[4];
	unsigned char shifts[4];
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
 * @param cg    the code generator object
 */
void arm_peephole_optimization(arm_code_gen_t *cg);

#endif /* FIRM_BE_ARM_ARM_OPTIMIZE_H */
