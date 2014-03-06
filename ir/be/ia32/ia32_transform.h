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
ir_entity *ia32_gen_fp_known_const(ir_graph *irg, ia32_known_const_t kct);

/**
 * Skip all Down-Conv's on a given node and return the resulting node.
 */
ir_node *ia32_skip_downconv(ir_node *node);

/** Initialize the ia32 instruction selector. */
void ia32_init_transform(void);

#endif
