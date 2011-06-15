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
 * @brief       Generic backend types and interfaces.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#ifndef FIRM_BE_MAIN_H
#define FIRM_BE_MAIN_H

#include <stdio.h>
#include "irarch.h"
#include "lowering.h"
#include "iroptimize.h"
#include "begin.h"

typedef enum {
	ASM_CONSTRAINT_FLAG_NONE                  = 0,
	ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER     = 1u << 0,
	ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP        = 1u << 1,
	ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE    = 1u << 2,
	ASM_CONSTRAINT_FLAG_NO_SUPPORT            = 1u << 3,
	ASM_CONSTRAINT_FLAG_MODIFIER_WRITE        = 1u << 4,
	ASM_CONSTRAINT_FLAG_MODIFIER_NO_WRITE     = 1u << 5,
	ASM_CONSTRAINT_FLAG_MODIFIER_READ         = 1u << 6,
	ASM_CONSTRAINT_FLAG_MODIFIER_NO_READ      = 1u << 7,
	ASM_CONSTRAINT_FLAG_MODIFIER_EARLYCLOBBER = 1u << 8,
	ASM_CONSTRAINT_FLAG_MODIFIER_COMMUTATIVE  = 1u << 9,
	ASM_CONSTRAINT_FLAG_INVALID               = 1u << 10
} asm_constraint_flags_t;
ENUM_BITSET(asm_constraint_flags_t)

/**
 * Build a Trampoline for the closure.
 * @param block       the block where to build the trampoline
 * @param mem         memory
 * @param trampoline  address of a trampoline region
 * @param env         address of the environment
 * @param callee      address of the function to call
 *
 * @return modified memory
 */
typedef ir_node *(create_trampoline_fkt)(ir_node *block, ir_node *mem, ir_node *trampoline, ir_node *env, ir_node *callee);

/**
 * This structure contains parameters that should be
 * propagated to the libFirm parameter set.
 */
typedef struct backend_params {
	/** If set, the backend supports inline assembly. */
	unsigned support_inline_asm:1;
	/** If set, the backend supports Rotl nodes */
	unsigned support_rotl:1;
	/** the backend uses big-endian byte ordering if set, else little endian */
	unsigned byte_order_big_endian:1;

	/** Settings for architecture dependent optimizations. */
	const ir_settings_arch_dep_t *dep_param;

	/** Backend settings for if-conversion. */
	arch_allow_ifconv_func allow_ifconv;

	/** size of machine words. This is usually the size of the general purpose
	 * integer registers. */
	unsigned machine_size;

	/**
	 * some backends like x87 can only do arithmetic in a specific float
	 * mode (load/store are still done in the "normal" float/double modes).
	 */
	ir_mode *mode_float_arithmetic;

	/** size of a long double floating mode in bits (or 0 if not supported) */
	unsigned long_double_size;

	/** Size of the trampoline code. */
	unsigned trampoline_size;

	/** Alignment of the trampoline code. */
	unsigned trampoline_align;

	/** If non-zero, build the trampoline. */
	create_trampoline_fkt *build_trampoline;

	/** Alignment of stack parameters */
	unsigned stack_param_align;
} backend_params;

/**
 * Register the Firm backend command line options.
 */
FIRM_API void be_opt_register(void);

/**
 * Parse one backend argument.
 */
FIRM_API int be_parse_arg(const char *arg);

/**
 * Return the backend configuration parameter.
 *
 * @return libFirm configuration parameters for the selected
 *         backend
 */
FIRM_API const backend_params *be_get_backend_param(void);

/**
 * Lowers current program for the target architecture.
 * This must be run once before using be_main. The idea here is that the backend
 * can perform lowerings like doubleword-lowering, ABI adjustments or
 * implementation of boolean values, if-conversion, with target specific
 * settings.
 * The resulting graph is still a "normal" firm-graph on which you can and
 * should perform further architecture-neutral optimisations before be_main.
 */
FIRM_API void be_lower_for_target(void);

/**
 * Creates an ir_prog pass which performs lowerings necessary for the target
 * architecture. (Calling backend_params->lower_for_target)
 */
FIRM_API ir_prog_pass_t *lower_for_target_pass(const char *name);

/**
 * Main interface to the frontend.
 */
FIRM_API void be_main(FILE *output, const char *compilation_unit_name);

/**
 * parse assembler constraint strings and returns flags (so the frontend knows
 * which operands are inputs/outputs and whether memory is required)
 */
FIRM_API asm_constraint_flags_t be_parse_asm_constraints(const char *constraints);

/**
 * tests whether a string is a valid clobber in an ASM instruction
 */
FIRM_API int be_is_valid_clobber(const char *clobber);

#include "end.h"

#endif
