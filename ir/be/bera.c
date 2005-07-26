/**
 * Base routines for register allocation.
 * @author Sebastian Hack
 * @date 22.11.2004
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pset.h"
#include "impl.h"

#include "irnode.h"
#include "irmode.h"
#include "irdom.h"

#include "beutil.h"
#include "besched_t.h"
#include "belive_t.h"

int value_dominates(const ir_node *a, const ir_node *b)
{
  int res = 0;
	const ir_node *ba = get_block(a);
	const ir_node *bb = get_block(b);

  /*
   * a and b are not in the same block,
   * so dominance is determined by the dominance of the blocks.
   */
  if(ba != bb)
    res = block_dominates(ba, bb);

  /*
   * Dominance is determined by the time steps of the schedule.
   */
  else {
    sched_timestep_t as = sched_get_time_step(a);
    sched_timestep_t bs = sched_get_time_step(b);
    res = as <= bs;
  }

  return res;
}

/**
 * Check, if two values interfere.
 * @param a The first value.
 * @param b The second value.
 * @return 1, if a and b interfere, 0 if not.
 */
int values_interfere(const ir_node *a, const ir_node *b)
{
  int a2b = value_dominates(a, b);
  int b2a = value_dominates(b, a);

  /*
   * Adjust a and b so, that a dominates b if
   * a dominates b or vice versa.
   */
  if(b2a) {
    const ir_node *t = a;
    a = b;
    b = t;
  }

  /*
   * If either a dmoninates b or vice versa
   * check if there is usage which is dominated by b.
   *
   * remind, that if a and b are in a dom relation, a always dominates b
   * here due to the if above.
   */
  if(a2b + b2a) {
    const ir_edge_t *edge;

    /* Look at all usages of a */
    foreach_out_edge(a, edge) {
      const ir_node *user = edge->src;

      /*
       * If the user is a phi we must check the last instruction in the
       * corresponding phi predecessor block since the phi is not a
       * proper user.
       */
      if(is_Phi(user)) {
        ir_node *phi_block = get_nodes_block(user);
        user = sched_last(get_Block_cfgpred_block(phi_block, edge->pos));
      }

      /* If b dominates a user of a, we can safely return 1 here. */
      if(value_dominates(b, user))
        return 1;
    }

  }

  return 0;
}
