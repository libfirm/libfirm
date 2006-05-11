
#ifndef _BE_MAIN_H
#define _BE_MAIN_H

#include <stdio.h>
#include "irarch.h"
#include "archop.h"
#include "lower_dw.h"
#include "dbginfo.h"

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
void be_main(FILE *file_handle);

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

typedef struct _be_main_env_t be_main_env_t;
typedef struct _be_irg_t be_irg_t;
typedef struct _be_options_t be_options_t;

#endif /* _BE_MAIN_H */
