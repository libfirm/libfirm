/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the x87 support and virtual to stack
 *              register translation for the ia32 backend.
 * @author      Michael Beck
 */
#ifndef FIRM_BE_IA32_X86_X87_H
#define FIRM_BE_IA32_X86_X87_H

#include <stdbool.h>
#include "be_types.h"
#include "firm_types.h"

/** Attributes for x87 nodes. */
typedef struct x87_attr_t {
	arch_register_t const *reg;        /**< The explicit register operand. */
	/** True if result is in the explicit register operand, %st0 otherwise. */
	bool                   res_in_reg;
	bool                   pop;        /**< Emit a pop suffix. */
} x87_attr_t;

/**
 * Run a simulation and fix all virtual instructions for a graph.
 * Replaces all virtual floating point instructions and registers
 * by real ones.
 *
 * @param irg      the graph to simulate and patch
 * @param cls      x87 register class
 *
 * Registers must be allocated.
 */
void x86_x87_simulate_graph(ir_graph *irg, const arch_register_class_t *cls);

/**
 * Initializes the x87 simulator.
 */
void x86_init_x87(void);

/** Initialize x87 mode+type/ */
void x86_init_x87_type(void);

extern ir_mode *x86_mode_E;
extern ir_type *x86_type_E;

#endif
