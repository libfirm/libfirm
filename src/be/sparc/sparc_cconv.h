/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   support functions for calling conventions
 * @author  Matthias Braun
 */
#ifndef FIRM_BE_SPARC_SPARC_CCONV_H
#define FIRM_BE_SPARC_SPARC_CCONV_H

#include "firm_types.h"
#include "be_types.h"
#include "benode.h"
#include "gen_sparc_regalloc_if.h"
#include "sparc_bearch_t.h"

/** information about a single parameter or result */
typedef struct reg_or_stackslot_t
{
	const arch_register_req_t *req0; /**< if != NULL, register requirements
	                                      for this parameter (or the first
	                                      part of it). */
	const arch_register_req_t *req1; /**< if != NULL, register requirements
	                                      for the 2nd part of the parameter */
	const arch_register_t *reg0;
	const arch_register_t *reg1;
	/** indicates that an entity of the specific type is needed */
	ir_type               *type;
	/** If transmitted via stack, the offset for this parameter.
	 * The offset is relative to the end of the space that contains all
	 * parameters (which is relative to SP + SPARC_MIN_STACK_SIZE immediately
	 * before the call). */
	int                    offset;
	bool                   already_stored;
	ir_entity             *entity; /**< entity in frame type */
} reg_or_stackslot_t;

/** The calling convention info for one call site. */
struct calling_convention_t
{
	bool                omit_fp;          /**< do not use frame pointer (and no
	                                           save/restore) */
	unsigned            n_parameters;     /**< number of parameters */
	reg_or_stackslot_t *parameters;       /**< parameter info. */
	unsigned            param_stack_size; /**< stack size for parameters */
	unsigned            n_param_regs;     /**< number of values passed in a
	                                           register */
	reg_or_stackslot_t *results;          /**< result info. */
	unsigned            n_reg_results;
	unsigned           *caller_saves;     /**< bitset of caller save registers */
	ir_entity          *va_start_addr;
};

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

void sparc_cconv_init(void);

#endif
