/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for AMD64 backend -- private header
 */
#ifndef FIRM_BE_AMD64_AMD64_BEARCH_T_H
#define FIRM_BE_AMD64_AMD64_BEARCH_T_H

#include "beirg.h"
#include "../ia32/x86_cconv.h"
#include "../ia32/x86_x87.h"

typedef struct amd64_irg_data_t {
	bool omit_fp;
} amd64_irg_data_t;

extern pmap *amd64_constants; /**< A map of entities that store const tarvals */

extern ir_mode *amd64_mode_xmm;

#define AMD64_REGISTER_SIZE   8
/** power of two stack alignment on calls */
#define AMD64_PO2_STACK_ALIGNMENT 4

static inline amd64_irg_data_t *amd64_get_irg_data(ir_graph const *const irg)
{
	return (amd64_irg_data_t*)be_birg_from_irg(irg)->isa_link;
}

/**
 * Determine how function parameters and return values are passed.
 * Decides what goes to register or to stack and what stack offsets/
 * datatypes are used.
 *
 * @param function_type  the type of the caller/callee function
 * @param caller         true for convention for the caller, false for callee
 */
x86_cconv_t *amd64_decide_calling_convention(ir_type *function_type,
                                             ir_graph *irg);

int amd64_get_sp_change(ir_node *const node);

void amd64_cconv_init(void);

void amd64_adjust_pic(ir_graph *irg);

void amd64_simulate_graph_x87(ir_graph *irg);

#endif
