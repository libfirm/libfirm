/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the IR transformation from firm into ia32-Firm.
 * @author      Christian Wuerdig, Matthias Braun
 */
#ifndef FIRM_BE_IA32_IA32_TRANSFORM_H
#define FIRM_BE_IA32_IA32_TRANSFORM_H

#include "firm_types.h"
#include "x86_asm.h"
#include "x86_node.h"

extern const x86_asm_constraint_list_t ia32_asm_constraints;

/**
 * Transform firm nodes to x86 assembler nodes, ie
 * do instruction selection.
 */
void ia32_transform_graph(ir_graph *irg);

/**
 * Some constants needed for code generation.
 * Generated on demand.
 */
typedef enum {
	ia32_SSIGN,          /**< SSE2 single precision sign */
	ia32_DSIGN,          /**< SSE2 double precision sign */
	ia32_SABS,           /**< SSE2 single precision ABS mask */
	ia32_DABS,           /**< SSE2 double precision ABS mask */
	ia32_ULLBIAS,        /**< x87 ULL->float bias */
	ia32_known_const_max /**< last constant */
} ia32_known_const_t;

/**
 * Generate a known floating point constant
 */
ir_entity *ia32_gen_fp_known_const(ia32_known_const_t kct);

/** Initialize the ia32 instruction selector. */
void ia32_init_transform(void);

ir_node *ia32_new_IncSP(ir_node *block, ir_node *old_sp, int offset,
                        bool no_align);

const arch_register_t *ia32_get_clobber_register(const char *clobber);

ir_node *ia32_create_Immediate_full(ir_graph *irg, const x86_imm32_t *imm);

static inline ir_node *ia32_create_Immediate(ir_graph *const irg,
                                             int32_t const val)
{
	x86_imm32_t imm = { .offset = val };
	return ia32_create_Immediate_full(irg, &imm);
}

#endif
