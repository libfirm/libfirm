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

/** information about a single parameter or result */
typedef struct reg_or_stackslot_t
{
	const arch_register_t *reg0;   /**< if != NULL, the first register used for
	                                    this parameter. */
	const arch_register_t *reg1;   /**< if != NULL, the second register used. */
	size_t                 reg_offset;
	ir_type               *type;   /**< indicates that an entity of the specific
									    type is needed */
	unsigned               offset; /**< if transmitted via stack, the offset for
	                                    this parameter. */
	ir_entity             *entity; /**< entity in frame type */
} reg_or_stackslot_t;

/** The calling convention info for one call site. */
typedef struct calling_convention_t
{
	bool                omit_fp;          /**< do not use frame pointer (and no
	                                           save/restore) */
	reg_or_stackslot_t *parameters;       /**< parameter info. */
	unsigned            param_stack_size; /**< stack size for parameters */
	unsigned            n_param_regs;     /**< number of values passed in a
	                                           register */
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
                                                      ir_graph *irg);

/**
 * free memory used by a calling_convention_t
 */
void sparc_free_calling_convention(calling_convention_t *cconv);

#endif
