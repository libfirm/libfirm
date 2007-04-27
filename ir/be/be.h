/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

#ifndef _BE_MAIN_H
#define _BE_MAIN_H

#include <stdio.h>
#include "irarch.h"
#include "archop.h"
#include "lower_dw.h"
#include "dbginfo.h"
#include "ifconv.h"

#include <libcore/lc_timing.h>

#define LC_STOP_AND_RESET_TIMER(timer) do { lc_timer_stop(timer); lc_timer_reset(timer); } while(0)

/**
 * This structure contains parameters that should be
 * propagated to the libFirm parameter set.
 */
typedef struct backend_params {
	/** Additional opcodes settings. */
	const arch_ops_info *arch_op_settings;

	/** Settings for architecture dependent optimizations */
	const arch_dep_params_t *dep_param;

	/** if set, the backend cannot handle DWORD access */
	unsigned do_dw_lowering;

	/** the architecture specific intrinsic function creator */
	create_intrinsic_fkt *arch_create_intrinsic_fkt;

	/** the context parameter for the create intrinsic function */
	void *create_intrinsic_ctx;

	/** backend settings for if-conversion */
	const opt_if_conv_info_t *if_conv_info;
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
 * Initialize the Firm backend. Must be run BEFORE init_firm()!
 *
 * @return libFirm configuration parameters for the selected
 *         backend
 */
const backend_params *be_init(void);

/**
 * Main interface to the frontend.
 */
void be_main(FILE *file_handle, const char *cup_name);

/** The type of the debug info retriever function. */
typedef const char *(*retrieve_dbg_func)(const dbg_info *dbg, unsigned *line);

/**
 * Sets a debug info retriever.
 *
 * @param func   the debug retriever function.
 */
void be_set_debug_retrieve(retrieve_dbg_func func);

/**
 * Retrieve the debug info.
 */
const char *be_retrieve_dbg_info(const dbg_info *dbg, unsigned *line);

typedef struct be_main_env_t be_main_env_t;
typedef struct be_options_t  be_options_t;

#endif /* _BE_MAIN_H */
