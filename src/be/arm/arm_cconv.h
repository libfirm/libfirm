/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   support functions for calling conventions
 * @author  Matthias Braun
 */
#ifndef FIRM_BE_ARM_ARM_CCONV_H
#define FIRM_BE_ARM_ARM_CCONV_H

#include "firm_types.h"
#include "be_types.h"
#include "gen_arm_regalloc_if.h"

/** information about a single parameter or result */
typedef struct reg_or_stackslot_t
{
	const arch_register_t *reg0;   /**< if != NULL, the first register used for this parameter. */
	const arch_register_t *reg1;   /**< if != NULL, the second register used. */
	ir_type               *type;   /**< indicates that an entity of the specific type is needed */
	unsigned               offset; /**< if transmitted via stack, the offset for this parameter. */
	ir_entity             *entity; /**< entity in frame type */
} reg_or_stackslot_t;

/** The calling convention info for one call site. */
typedef struct calling_convention_t
{
	reg_or_stackslot_t *parameters;        /**< parameter info. */
	unsigned            n_parameters;      /**< number of parameters */
	unsigned            param_stack_size;  /**< needed stack size for parameters */
	unsigned            n_param_regs;
	reg_or_stackslot_t *results;           /**< result info. */
} calling_convention_t;

/**
 * determine how function parameters and return values are passed.
 * Decides what goes to register or to stack and what stack offsets/
 * datatypes are used.
 */
calling_convention_t *arm_decide_calling_convention(const ir_graph *irg,
                                                    ir_type *function_type);

/**
 * free memory used by a calling_convention_t
 */
void arm_free_calling_convention(calling_convention_t *cconv);

#endif
