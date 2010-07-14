/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @brief   support functions for calling conventions
 * @author  Matthias Braun
 * @version $Id$
 */
#ifndef FIRM_BE_ARM_ARM_CCONV_H
#define FIRM_BE_ARM_ARM_CCONV_H

#include "firm_types.h"
#include "../be_types.h"
#include "gen_arm_regalloc_if.h"

static const arch_register_t *const callee_saves[] = {
	&arm_gp_regs[REG_R4],
	&arm_gp_regs[REG_R5],
	&arm_gp_regs[REG_R6],
	&arm_gp_regs[REG_R7],
	&arm_gp_regs[REG_R8],
	&arm_gp_regs[REG_R9],
	&arm_gp_regs[REG_R10],
	&arm_gp_regs[REG_R11],
	&arm_gp_regs[REG_LR],
};

static const arch_register_t *const caller_saves[] = {
	&arm_gp_regs[REG_R0],
	&arm_gp_regs[REG_R1],
	&arm_gp_regs[REG_R2],
	&arm_gp_regs[REG_R3],
	&arm_gp_regs[REG_LR],

	&arm_fpa_regs[REG_F0],
	&arm_fpa_regs[REG_F1],
	&arm_fpa_regs[REG_F2],
	&arm_fpa_regs[REG_F3],
	&arm_fpa_regs[REG_F4],
	&arm_fpa_regs[REG_F5],
	&arm_fpa_regs[REG_F6],
	&arm_fpa_regs[REG_F7],
};

static const arch_register_t* const param_regs[] = {
	&arm_gp_regs[REG_R0],
	&arm_gp_regs[REG_R1],
	&arm_gp_regs[REG_R2],
	&arm_gp_regs[REG_R3]
};

static const arch_register_t* const result_regs[] = {
	&arm_gp_regs[REG_R0],
	&arm_gp_regs[REG_R1],
	&arm_gp_regs[REG_R2],
	&arm_gp_regs[REG_R3]
};

static const arch_register_t* const float_result_regs[] = {
	&arm_fpa_regs[REG_F0],
	&arm_fpa_regs[REG_F1]
};

/** information about a single parameter or result */
typedef struct reg_or_stackslot_t
{
	const arch_register_t *reg0;   /**< if != NULL, the first register used for this parameter. */
	const arch_register_t *reg1;   /**< if != NULL, the second register used. */
	ir_type               *type;   /**< indicates that an entity of the specific
									    type is needed */
	int                    offset; /**< if transmitted via stack, the offset for this parameter. */
	ir_entity             *entity; /**< entity in frame type */
} reg_or_stackslot_t;

/** The calling convention info for one call site. */
typedef struct calling_convention_t
{
	reg_or_stackslot_t *parameters;        /**< parameter info. */
	int                 param_stack_size;  /**< needed stack size for parameters */
	reg_or_stackslot_t *results;           /**< result info. */
} calling_convention_t;

/**
 * determine how function parameters and return values are passed.
 * Decides what goes to register or to stack and what stack offsets/
 * datatypes are used.
 */
calling_convention_t *decide_calling_convention(ir_type *function_type);

/**
 * free memory used by a calling_convention_t
 */
void free_calling_convention(calling_convention_t *cconv);

#endif
