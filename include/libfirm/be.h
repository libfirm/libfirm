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
	ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER     = 0x0001,
	ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP        = 0x0002,
	ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE    = 0x0004,
	ASM_CONSTRAINT_FLAG_NO_SUPPORT            = 0x0008,
	ASM_CONSTRAINT_FLAG_MODIFIER_WRITE        = 0x0010,
	ASM_CONSTRAINT_FLAG_MODIFIER_NO_WRITE     = 0x0020,
	ASM_CONSTRAINT_FLAG_MODIFIER_READ         = 0x0040,
	ASM_CONSTRAINT_FLAG_MODIFIER_NO_READ      = 0x0080,
	ASM_CONSTRAINT_FLAG_MODIFIER_EARLYCLOBBER = 0x0100,
	ASM_CONSTRAINT_FLAG_MODIFIER_COMMUTATIVE  = 0x0200,
	ASM_CONSTRAINT_FLAG_INVALID               = 0x8000
} asm_constraint_flags_t;

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

/** callback where the backend performs required lowering for the target
 * architecture. Typical examples are transforming doubleword operations into
 * sequences of word operations. The callback should be invoked before the
 * frontend, because it is usually a good idea to perform other optimisations
 * after the lowering
 */
typedef void (*lower_for_target_func)(void);

/**
 * This structure contains parameters that should be
 * propagated to the libFirm parameter set.
 */
typedef struct backend_params {
	/** If set, the backend supports inline assembly. */
	unsigned support_inline_asm:1;
	/** If set, the backend supports Rotl nodes */
	unsigned support_rotl:1;

	/** callback that performs lowerings required for target architecture */
	lower_for_target_func lower_for_target;

	/** Settings for architecture dependent optimizations. */
	const ir_settings_arch_dep_t *dep_param;

	/** Backend settings for if-conversion. */
	arch_allow_ifconv_func allow_ifconv;

	/**
	 * some backends like x87 can only do arithmetic in a specific float
	 * mode (but convert to/from other float modes).
	 */
	ir_mode *mode_float_arithmetic;

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

typedef struct be_main_env_t be_main_env_t;
typedef struct be_options_t  be_options_t;

#include "end.h"

#endif
