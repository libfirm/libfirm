/**
 * Internal backend global data structures.
 * @author Sebastian Hack
 * @date 8.12.2004
 */

#ifndef _BE_T_H
#define _BE_T_H

#include "obst.h"
#include "debug.h"

#include "irgraph.h"

#include "be.h"
#include "bearch.h"
#include "beirgmod.h"

struct _be_options_t {
	char ilp_solver[128];
	char ilp_server[128];
};

struct _be_main_env_t {
  struct obstack obst;
  struct _be_node_factory_t *node_factory;
  struct _arch_env_t *arch_env;
  struct _be_options_t *options;
  struct _arch_code_generator_t *cg;
  firm_dbg_module_t *dbg;

  const arch_register_t **caller_save; /**< NULL-terminated list of caller save registers. */
  const arch_register_t **callee_save; /**< NULL-terminated list of callee save registers. */
};

struct _be_irg_t {
	ir_graph                      *irg;
	struct _be_main_env_t         *main_env;
	struct _dom_front_info_t      *dom_front;
	struct _be_abi_irg_t          *abi;
	struct _arch_code_generator_t *cg;
};

#endif
