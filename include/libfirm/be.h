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
#include "archop.h"
#include "lowering.h"

/**
 * This structure contains parameters that should be
 * propagated to the libFirm parameter set.
 */
typedef struct backend_params {
	/** If set, the backend cannot handle DWORD access. */
	unsigned do_dw_lowering:1;
	/** If set, the backend supports inline assembly. */
	unsigned support_inline_asm:1;
	/** If set, the target architecture use an immediate floating point mode. */
	unsigned has_imm_fp_mode:1;

	/** Additional opcodes settings. */
	const arch_ops_info *arch_op_settings;

	/** Settings for architecture dependent optimizations. */
	const ir_settings_arch_dep_t *dep_param;

	/** The architecture specific intrinsic function creator. */
	create_intrinsic_fkt *arch_create_intrinsic_fkt;

	/** The context parameter for the create intrinsic function. */
	void *create_intrinsic_ctx;

	/** Backend settings for if-conversion. */
	const ir_settings_if_conv_t *if_conv_info;

	/** The immediate floating point mode. Temporaries are calculated using this mode. */
	ir_mode *imm_fp_mode;
} backend_params;

/**
 * Register the Firm backend command line options.
 */
void be_opt_register(void);

/**
 * Parse one backend argument.
 */
int be_parse_arg(const char *arg);

/**
 * Return the backend configuration parameter.
 *
 * @return libFirm configuration parameters for the selected
 *         backend
 */
const backend_params *be_get_backend_param(void);

/**
 * Main interface to the frontend.
 */
void be_main(FILE *output, const char *compilation_unit_name);

typedef struct be_main_env_t be_main_env_t;
typedef struct be_options_t  be_options_t;

#endif
