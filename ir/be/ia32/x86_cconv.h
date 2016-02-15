/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   support functions for calling conventions
 * @author  Matthias Braun
 */
#ifndef FIRM_BE_IA32_X86_CCONV_H
#define FIRM_BE_IA32_X86_CCONV_H

#include "firm_types.h"
#include "be_types.h"
#include "benode.h"

/** Information about a single function parameter or result */
typedef struct reg_or_stackslot_t
{
	const arch_register_t *reg;
	/** Type of parameter if it is transfered on the stack, otherwise NULL. */
	ir_type               *type;
	/** Offset of parameter relative to the stackpointer immediately before the
	 * call. Undefined if a register is used. */
	unsigned               offset;
	ir_entity             *entity; /**< entity in frame type */
} reg_or_stackslot_t;

/** The calling convention info for one call site. */
typedef struct x86_cconv_t
{
	bool                omit_fp;          /**< do not use frame pointer (and no
	                                           save/restore) */
	unsigned            sp_delta;
	reg_or_stackslot_t *parameters;       /**< parameter info. */
	unsigned            n_parameters;     /**< number of parameters */
	unsigned            param_stacksize;  /**< stack size for parameters */
	unsigned            n_param_regs;     /**< number of values passed in a
	                                           register (gp + xmm) */
	unsigned            n_xmm_regs;       /**< number of xmm registers used */
	reg_or_stackslot_t *results;          /**< result info. */
	unsigned            n_reg_results;
	unsigned           *caller_saves;     /**< bitset: caller saved registers */
	unsigned           *callee_saves;     /**< bitset: callee saved registers */
	ir_entity          *va_start_addr;
} x86_cconv_t;

/**
 * free memory used by a x86_cconv_t
 */
void x86_free_calling_convention(x86_cconv_t *cconv);

void x86_create_parameter_loads(ir_graph *irg, const x86_cconv_t *cconv);

void x86_layout_param_entities(ir_graph *irg, x86_cconv_t *cconv,
                               int params_offset);

#endif
