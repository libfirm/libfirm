/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   declarations for SPARC backend -- private header
 * @author  Hannes Rapp, Matthias Braun
 * @version $Id$
 */
#ifndef FIRM_BE_SPARC_BEARCH_SPARC_T_H
#define FIRM_BE_SPARC_BEARCH_SPARC_T_H

#include <stdbool.h>
#include "sparc_nodes_attr.h"
#include "be.h"

typedef struct sparc_transform_env_t  sparc_transform_env_t;
typedef struct sparc_isa_t            sparc_isa_t;
typedef struct calling_convention_t   calling_convention_t;

/** Floating point instruction set. */
enum sparc_fp_architectures {
	SPARC_FPU_ARCH_NONE      = 0,
	SPARC_FPU_ARCH_FPU       = 0x00000001,
	SPARC_FPU_ARCH_SOFTFLOAT = 0x00000002,
};

struct sparc_isa_t {
	arch_env_t  base;      /**< must be derived from arch_env_t */
	pmap       *constants;
	int         fpu_arch;  /**< FPU architecture */
};

/**
 * this is a struct to minimize the number of parameters
 * for transformation walker
 */
struct sparc_transform_env_t {
	dbg_info *dbg;      /**< The node debug info */
	ir_graph *irg;      /**< The irg, the node should be created in */
	ir_node  *block;    /**< The block, the node should belong to */
	ir_node  *irn;      /**< The irn, to be transformed */
	ir_mode  *mode;     /**< The mode of the irn */
};

extern const arch_irn_ops_t sparc_irn_ops;

/**
 * SPARC ABI requires some space which is always available at the top of
 * the stack. It contains:
 * 16*4 bytes space for spilling the register window
 * 1*4 byte   holding a pointer to space for aggregate returns (the space is
 *            always reserved, regardless whether we have an aggregate return
 *            or not)
 * 6*4 bytes  Space for spilling parameters 0-5. For the cases when someone
 *            takes the address of a parameter. I guess this is also there so
 *            the implementation of va_args gets easier -> We can simply store
 *            param 0-5 in this spaces and then handle va_next by simply
 *            incrementing the stack pointer
 */
#define SPARC_IMMEDIATE_MIN           -4096
#define SPARC_IMMEDIATE_MAX            4095
#define SPARC_MIN_STACKSIZE              92
#define SPARC_AGGREGATE_RETURN_OFFSET    64
#define SPARC_PARAMS_SPILL_OFFSET        68
#define SPARC_N_PARAM_REGS                6
#define SPARC_STACK_ALIGNMENT             8
#define SPARC_REGISTER_SIZE               4

static inline bool sparc_is_value_imm_encodeable(int32_t value)
{
	return SPARC_IMMEDIATE_MIN <= value && value <= SPARC_IMMEDIATE_MAX;
}

void sparc_finish(ir_graph *irg);

void sparc_introduce_prolog_epilog(ir_graph *irg);

void sparc_lower_64bit(void);

bool sparc_variadic_fixups(ir_graph *irg, calling_convention_t *cconv);
void sparc_create_stacklayout(ir_graph *irg, calling_convention_t *cconv);
void sparc_fix_stack_bias(ir_graph *irg);

#endif
