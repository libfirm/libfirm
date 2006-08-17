/**
 * Internal backend global data structures.
 * @author Sebastian Hack
 * @date 8.12.2004
 */

#ifndef _BE_T_H
#define _BE_T_H

#include "firm_types.h"
#include "obst.h"
#include "debug.h"

#include "be.h"
#include "bearch.h"
#include "beirgmod.h"

#define DUMP_NONE       0
#define DUMP_INITIAL    (1 << 0)
#define DUMP_ABI        (1 << 1)
#define DUMP_SCHED      (1 << 2)
#define DUMP_PREPARED   (1 << 3)
#define DUMP_RA         (1 << 4)
#define DUMP_FINAL      (1 << 5)
#define DUMP_BE         (1 << 6)

enum {
	BE_TIME_OFF,
	BE_TIME_ON
};

enum {
	BE_VRFY_OFF,
	BE_VRFY_WARN,
	BE_VRFY_ASSERT
};

enum {
	BE_SCHED_SELECT_ISA      = 0,
	BE_SCHED_SELECT_MUCHNIK  = 1,
	BE_SCHED_SELECT_HEUR     = 2,
	BE_SCHED_SELECT_HMUCHNIK = 3
};

struct _be_options_t {
	int  dump_flags;
	int  timing;
	int  sched_select;
	int  mris;
	char ilp_server[128];
	char ilp_solver[128];
};

struct _be_main_env_t {
  struct obstack obst;
  struct _be_node_factory_t *node_factory;
  struct _arch_env_t *arch_env;
  struct _be_options_t *options;
  struct _arch_code_generator_t *cg;
  struct _arch_irn_handler_t *phi_handler;
  DEBUG_ONLY(firm_dbg_module_t *dbg;)
};

struct _be_irg_t {
	ir_graph                      *irg;
	struct _be_main_env_t         *main_env;
	struct _be_abi_irg_t          *abi;
	struct _arch_code_generator_t *cg;
};

#endif /* _BE_T_H */
