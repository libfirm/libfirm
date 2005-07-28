/**
 * @file   bedupl.c
 * @date   15.07.2005
 * @author Sebastian Hack
 *
 * Insert duplicates for phi operands which interfere with the phi.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"

#include "be_t.h"
#include "bearch.h"
#include "bera.h"
#include "benode_t.h"
#include "besched_t.h"

static void eliminate_phi_int_walker(ir_node *irn, void *data)
{
  const be_main_session_env_t *env = data;
  const arch_register_class_t *cls =
    arch_get_irn_reg_class(env->main_env->arch_env, irn, arch_pos_make_out(0));

  if(is_Phi(irn) && cls != NULL) {
    int i, n;
    ir_node *phi_bl = get_nodes_block(irn);

    for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
      ir_node *operand   = get_irn_n(irn, i);
      ir_node *bl        = get_Block_cfgpred_block(phi_bl, i);

      if(is_live_in(phi_bl, irn)) { // values_interfere(irn, operand)) {
        ir_node *copy = new_Copy(env->main_env->node_factory, cls, env->irg, bl, operand);
        set_irn_n(irn, i, copy);
        sched_add_after(sched_last(bl), copy);
      }
    }
  }
}

void be_eliminate_phi_interferences(const be_main_session_env_t *env)
{
  irg_walk_graph(env->irg, eliminate_phi_int_walker, NULL, (void *) env);
}
