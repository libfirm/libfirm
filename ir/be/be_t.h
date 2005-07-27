/**
 * Internal backend global data structures.
 * @author Sebastian Hack
 * @date 8.12.2004
 */

#ifndef _BE_T_H
#define _BE_T_H

#include "obst.h"
#include "debug.h"

typedef struct _be_main_env_t {
  struct obstack obst;
  struct _be_node_factory_t *node_factory;
  struct _arch_env_t *arch_env;
  firm_dbg_module_t *dbg;
} be_main_env_t;

typedef struct _be_main_session_env_t {
  const be_main_env_t *main_env;
  ir_graph *irg;
  struct _dom_front_info_t *dom_front;
} be_main_session_env_t;

#endif
