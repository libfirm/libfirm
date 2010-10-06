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
#ifndef FIRM_BE_SPARC_SPARC_CCONV_H
#define FIRM_BE_SPARC_SPARC_CCONV_H

#include "firm_types.h"
#include "../be_types.h"
#include "gen_sparc_regalloc_if.h"

static const arch_register_t *const caller_saves[] = {
	&sparc_registers[REG_G1],
	&sparc_registers[REG_G2],
	&sparc_registers[REG_G3],
	&sparc_registers[REG_G4],
	&sparc_registers[REG_O0],
	&sparc_registers[REG_O1],
	&sparc_registers[REG_O2],
	&sparc_registers[REG_O3],
	&sparc_registers[REG_O4],
	&sparc_registers[REG_O5],

	&sparc_registers[REG_F0],
	&sparc_registers[REG_F1],
	&sparc_registers[REG_F2],
	&sparc_registers[REG_F3],
	&sparc_registers[REG_F4],
	&sparc_registers[REG_F5],
	&sparc_registers[REG_F6],
	&sparc_registers[REG_F7],
	&sparc_registers[REG_F8],
	&sparc_registers[REG_F9],
	&sparc_registers[REG_F10],
	&sparc_registers[REG_F11],
	&sparc_registers[REG_F12],
	&sparc_registers[REG_F13],
	&sparc_registers[REG_F14],
	&sparc_registers[REG_F15],
	&sparc_registers[REG_F16],
	&sparc_registers[REG_F17],
	&sparc_registers[REG_F18],
	&sparc_registers[REG_F19],
	&sparc_registers[REG_F20],
	&sparc_registers[REG_F21],
	&sparc_registers[REG_F22],
	&sparc_registers[REG_F23],
	&sparc_registers[REG_F24],
	&sparc_registers[REG_F25],
	&sparc_registers[REG_F26],
	&sparc_registers[REG_F27],
	&sparc_registers[REG_F28],
	&sparc_registers[REG_F29],
	&sparc_registers[REG_F30],
	&sparc_registers[REG_F31],
};

static const arch_register_t* const param_regs[] = {
	&sparc_registers[REG_I0],
	&sparc_registers[REG_I1],
	&sparc_registers[REG_I2],
	&sparc_registers[REG_I3],
	&sparc_registers[REG_I4],
	&sparc_registers[REG_I5],
};

static const arch_register_t* const float_result_regs[] = {
	&sparc_registers[REG_F0],
	&sparc_registers[REG_F1],
	&sparc_registers[REG_F2],
	&sparc_registers[REG_F3],
};

/** information about a single parameter or result */
typedef struct reg_or_stackslot_t
{
	const arch_register_t *reg0;   /**< if != NULL, the first register used for
	                                    this parameter. */
	const arch_register_t *reg1;   /**< if != NULL, the second register used. */
	ir_type               *type;   /**< indicates that an entity of the specific
									    type is needed */
	int                    offset; /**< if transmitted via stack, the offset for
	                                    this parameter. */
	ir_entity             *entity; /**< entity in frame type */
} reg_or_stackslot_t;

/** The calling convention info for one call site. */
typedef struct calling_convention_t
{
	reg_or_stackslot_t *parameters;       /**< parameter info. */
	int                 param_stack_size; /**< stack size for parameters */
	reg_or_stackslot_t *results;          /**< result info. */
} calling_convention_t;

/**
 * Determine how function parameters and return values are passed.
 * Decides what goes to register or to stack and what stack offsets/
 * datatypes are used.
 *
 * @param function_type  the type of the caller/callee function
 * @param caller         true for convention for the caller, false for callee
 */
calling_convention_t *sparc_decide_calling_convention(ir_type *function_type,
                                                      bool caller);

/**
 * free memory used by a calling_convention_t
 */
void sparc_free_calling_convention(calling_convention_t *cconv);

#endif
