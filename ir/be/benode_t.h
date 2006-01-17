/**
 * @file   benode_t.h
 * @date   17.05.2005
 * @author Sebastian Hack
 *
 * Backend node support.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BENODE_T_H
#define _BENODE_T_H

#include "pmap.h"

#include "irmode.h"
#include "irnode.h"

#include "be_t.h"
#include "bearch.h"

struct _be_node_factory_t {
  const arch_isa_if_t *isa;

  struct obstack      obst;
  set                 *ops;
  pmap                *irn_op_map;
  pmap                *reg_req_map;

  arch_irn_handler_t  handler;
  arch_irn_ops_t      irn_ops;
};

typedef struct _be_node_factory_t 			be_node_factory_t;

be_node_factory_t *be_node_factory_init(be_node_factory_t *factory, const arch_isa_t *isa);

const arch_irn_handler_t *be_node_get_irn_handler(const be_node_factory_t *f);

ir_node *new_Spill(const be_node_factory_t *factory,
    const arch_register_class_t *cls,
    ir_graph *irg, ir_node *bl, ir_node *node_to_spill, ir_node *ctx);

ir_node *new_Reload(const be_node_factory_t *factory,
    const arch_register_class_t *cls, ir_graph *irg,
    ir_node *bl, ir_mode *mode, ir_node *spill_node);

ir_node *new_Perm(const be_node_factory_t *factory,
    const arch_register_class_t *cls,
    ir_graph *irg, ir_node *bl, int arity, ir_node **in);

ir_node *new_Copy(const be_node_factory_t *factory,
    const arch_register_class_t *cls,
    ir_graph *irg, ir_node *block, ir_node *in);

ir_node *be_spill(
		const be_node_factory_t *factory,
		const arch_env_t *arch_env,
		ir_node *irn,
		ir_node *spill_ctx);

ir_node *be_reload(
		const be_node_factory_t *factory,
		const arch_env_t *arch_env,
		const arch_register_class_t *cls,
		ir_node *irn, int pos, ir_mode *mode, ir_node *spill);

ir_node *new_Keep(ir_graph *irg, ir_node *bl, int n, ir_node *in[]);

int be_is_Spill(const ir_node *irn);
int be_is_Reload(const ir_node *irn);
int be_is_Copy(const ir_node *irn);
int be_is_Perm(const ir_node *irn);

const arch_register_class_t *be_node_get_reg_class(const ir_node *irn);

void set_Spill_offset(ir_node *irn, unsigned offset);
unsigned get_Spill_offset(ir_node *irn);

ir_node *get_Spill_context(const ir_node *irn);


/**
 * Modify the output register requirements of a Perm.
 * This function incur register constraints to an output value of a Perm.
 * This is used when handling register constraints in general,
 * see beconstrperm.c
 */
void be_set_Perm_out_req(ir_node *irn, int pos, const arch_register_req_t *req);

/**
 * Insert a Perm node after a specific node in the schedule.
 * The Perm permutes over all values live at the given node.
 * This means that all liveness intervals are cut apart at this
 * location in the program.
 */
ir_node *insert_Perm_after(const be_main_env_t *env,
						   const arch_register_class_t *cls,
						   dom_front_info_t *dom_front,
						   ir_node *pos);

#endif /* _BENODE_T_H */
