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

  /* If there is no dominance relation, they do not interfere. */
  if((a2b | b2a) > 0) {
    const ir_edge_t *edge;
    ir_node *bb;

    /*
     * Adjust a and b so, that a dominates b if
     * a dominates b or vice versa.
     */
    if(b2a) {
      const ir_node *t = a;
      a = b;
      b = t;
    }

		bb = get_nodes_block(b);

    /*
     * If a is live end in b's block it is
     * live at b's definition (a dominates b)
     */
    if(is_live_end(bb, a))
      return 1;

    /*
     * Look at all usages of a.
     * If there's one usage of a in the block of b, then
     * we check, if this use is dominated by b, if that's true
     * a and b interfere. Note that b must strictly dominate the user,
		 * since if b is the last user of in the block, b and a do not
		 * interfere.
     * Uses of a not in b's block can be disobeyed, because the
     * check for a being live at the end of b's block is already
     * performed.
     */
    foreach_out_edge(a, edge) {
      const ir_node *user = edge->src;
      if(get_nodes_block(user) == bb
          && !is_Phi(user)
					&& b != user
          && value_dominates(b, user))
        return 1;
    }
  }
  return 0;
}
