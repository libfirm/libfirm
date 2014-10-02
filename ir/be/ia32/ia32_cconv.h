/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   support functions for calling conventions
 * @author  Matthias Braun
 */
#ifndef FIRM_BE_IA32_IA32_CCONV_H
#define FIRM_BE_IA32_IA32_CCONV_H

#include "firm_types.h"
#include "bearch_ia32_t.h"
#include "be_types.h"
#include "benode.h"
#include "gen_ia32_regalloc_if.h"

/** information about a single parameter or result */
typedef struct reg_or_stackslot_t
{
	const arch_register_req_t *req; /**< if != NULL, register requirements
	                                     for this parameter (or the first
	                                     part of it). */
	const arch_register_t *reg;
	unsigned               reg_offset;
	ir_type               *type;   /**< indicates that an entity of the specific
									    type is needed */
	unsigned               offset; /**< if transmitted via stack, the offset for
	                                    this parameter. */
	ir_entity             *entity; /**< entity in frame type */
} reg_or_stackslot_t;

/** The calling convention info for one call site. */
typedef struct ia32_cconv_t
{
	bool                omit_fp;          /**< do not use frame pointer (and no
	                                           save/restore) */
	unsigned            sp_delta;
	reg_or_stackslot_t *parameters;       /**< parameter info. */
	unsigned            param_stack_size; /**< stack size for parameters */
	unsigned            n_param_regs;     /**< number of values passed in a
	                                           register (gp + xmm) */
	unsigned            n_xmm_regs;       /**< number of xmm registers used */
	reg_or_stackslot_t *results;          /**< result info. */
	unsigned            n_reg_results;
	unsigned           *caller_saves;     /**< bitset of caller saved registers */
	unsigned           *callee_saves;     /**< bitset of callee saved registers */
} ia32_cconv_t;

/**
 * Determine how function parameters and return values are passed.
 * Decides what goes to register or to stack and what stack offsets/
 * datatypes are used.
 *
 * @param function_type  the type of the caller/callee function
 * @param caller         true for convention for the caller, false for callee
 */
ia32_cconv_t *ia32_decide_calling_convention(ir_type *function_type,
                                             ir_graph *irg);

/**
 * free memory used by a ia32_cconv_t
 */
void ia32_free_calling_convention(ia32_cconv_t *cconv);

void ia32_cconv_init(void);

#endif
