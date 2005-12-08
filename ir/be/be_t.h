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
#include "beirgmod.h"

struct _be_options_t {
	char ilp_server[128];
	char ilp_solver[128];
};

struct _be_main_env_t {
  struct obstack obst;
  struct _be_node_factory_t *node_factory;
  struct _arch_env_t *arch_env;
  struct _be_options_t *options;
  firm_dbg_module_t *dbg;
};

#if 0
struct _be_main_session_env_t {
  const struct _be_main_env_t *main_env;
  ir_graph *irg;
  struct _dom_front_info_t *dom_front;
};
#endif



#endif
