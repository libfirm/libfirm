/*
 * Project:     libFIRM
 * File name:   ir/opt/return.c
 * Purpose:     normalize returns
 * Author:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif

#include "irgraph_t.h"
#include "ircons_t.h"
#include "irnode_t.h"
#include "irgmod.h"

#define set_bit(n)      (returns[(n) >> 3] |= 1 << ((n) & 7))
#define get_bit(n)      (returns[(n) >> 3] & (1 << ((n) & 7)))

#undef IMAX
#define IMAX(a, b)       ((a) > (b) ? (a) : (b))

/*
 * Normalize the Returns of a graph by creating a new End block
 * with One Return(Phi).
 * This is the prefered input for the if-conversion.
 *
 * In pseudocode, it means:
 *
 * if (a)
 *   return b;
 * else
 *   return c;
 *
 * is transformed into
 *
 * if (a)
 *   res = b;
 * else
 *   res = c;
 * return res;
 */
void normalize_one_return(ir_graph *irg)
{
  ir_node *endbl = get_irg_end_block(irg);
  int i, j, k, n, last_idx, n_rets, n_ret_vals = -1;
  unsigned char *returns;
  ir_node **in, **retvals;

  ir_node *block, *new_ret;

  /* look, if we have more than one return */
  n       = get_Block_n_cfgpreds(endbl);
  returns = alloca((n + 7) >> 3);
  memset(returns, 0, (n + 7) >> 3);

  for (n_rets = i = 0; i < n; ++i) {
    ir_node *node = get_Block_cfgpred(endbl, i);

    if (get_irn_op(node) == op_Return) {
      ++n_rets;

      set_bit(i);

      if (n_ret_vals < 0)
        n_ret_vals = get_irn_arity(node);
    }
  }

  /* there should be at least one Return node in Firm */
  if (n_rets <= 1)
    return;

  in      = alloca(sizeof(*in) * IMAX(n_rets, n_ret_vals));
  retvals = alloca(sizeof(*in) * n_rets * n_ret_vals);

  for (j = i = 0; i < n; ++i) {
    if (get_bit(i)) {
      ir_node *ret  = get_Block_cfgpred(endbl, i);
      ir_node *block = get_nodes_block(ret);

      /* create a new Jmp for every Ret and place the in in */
      in[j] = new_r_Jmp(irg, block);

      /* save the return values and shuffle them */
      for (k = 0; k < n_ret_vals; ++k)
        retvals[j + k*n_ret_vals] = get_irn_n(ret, k);

      ++j;

      set_Block_cfgpred(endbl, i, new_r_Bad(irg));
      last_idx = i;
    }
  }

  /* ok, create a new block with all created in's */
  block = new_r_Block(irg, n_rets, in);

  /* now create the Phi nodes */
  for (j = i = 0; i < n_ret_vals; ++i, j += n_rets) {
    /* the return values are already shuffled */
    in[i] = new_r_Phi(irg, block, n_rets, &retvals[j], get_irn_mode(retvals[j]));
  }

  new_ret = new_r_Return(irg, block, in[0], n_ret_vals-1, &in[1]);

  set_Block_cfgpred(endbl, last_idx, new_ret);

  /* invalidate analysis information:
   * a new Block was added, so dominator, outs and loop are inconsistent,
   * trouts and callee-state should be still valid
   */
  set_irg_dom_inconsistent(irg);
  set_irg_outs_inconsistent(irg);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_cf_inconsistent);
}

/**
 * check, whether a Ret can be moved on block upwards.
 *
 * In a block with a Return, all live nodes must be linked
 * with the Return, otherwise they are dead (because the Return leaves
 * the graph, so no more users of the other nodes can exists.
 *
 * We can move a Return, if it's predecessors are Phi nodes or
 * comes from another block. In the later case, it is always possible
 * to move the Return one block up, because the predecessor block must
 * dominate the Return block (SSA) and then it dominates the predecessor
 * block of the Return block as well.
 *
 * All predecessors of the Return block must be Jmp's of course, or we
 * cannot move it up, so we check this either.
 */
static int can_move_ret(ir_node *ret)
{
  ir_node *retbl = get_nodes_block(ret);
  int i, n = get_irn_arity(ret);

  for (i = 0; i < n; ++i) {
    ir_node *pred = get_irn_n(ret, i);

    if (! is_Phi(pred) && retbl == get_nodes_block(pred)) {
      /* first condition failed, found a non-Phi predecessor
       * then is in the Return block */
      return 0;
    }
  }

  /* check, that predecessors are Jmps */
  n = get_Block_n_cfgpreds(retbl);
  for (i = 0; i < n; ++i)
    if (get_irn_op(get_Block_cfgpred(retbl, i)) != op_Jmp)
      return 0;

  /* if we have 0 control flow predecessors, we cannot move :-) */
  return n > 0;
}

/*
 * Normalize the Returns of a graph by moving
 * the Returns upwards as much as possible.
 * This might be prefered for code generation.
 *
 * In pseudocode, it means:
 *
 * if (a)
 *   res = b;
 * else
 *   res = c;
 * return res;
 *
 * is transformed into
 *
 * if (a)
 *   return b;
 * else
 *   return c;
 */
void normalize_n_returns(ir_graph *irg)
{
  int i, j, n, n_rets, n_finals, n_ret_vals;
  ir_node *list  = NULL;
  ir_node *final = NULL;
  ir_node **in;
  ir_node *endbl = get_irg_end_block(irg);
  ir_node *end;

  /* first, link all returns */
  n = get_Block_n_cfgpreds(endbl);
  for (n_finals = n_rets = i = 0; i < n; ++i) {
    ir_node *ret = get_Block_cfgpred(endbl, i);

    if (get_irn_op(ret) == op_Return && can_move_ret(ret)) {
      /*
       * Ok, all conditions met, we can move this Return, put it
       * on our work list.
       */
      set_irn_link(ret, list);
      list = ret;
      ++n_rets;
    }
    else {
      /* Put all nodes that are not changed on the final list. */
      set_irn_link(ret, final);
      final = ret;
      ++n_finals;
    }
  }

  if (n_rets <= 0)
    return;

  /*
   * Now move the Returns upwards. We move always one block up (and create n
   * new Returns), than we check if a newly created Return can be moved even further.
   * If yes, we simply add it to our work list, else to the final list.
   */
  end        = get_irg_end(irg);
  n_ret_vals = get_irn_arity(list);
  in         = alloca(sizeof(*in) * n_ret_vals);
  while (list) {
    ir_node *ret   = list;
    ir_node *block = get_nodes_block(ret);
    ir_node *phiM;

    list = get_irn_link(ret);
    --n_rets;

    n = get_Block_n_cfgpreds(block);
    for (i = 0; i < n; ++i) {
      ir_node *jmp    = get_Block_cfgpred(block, i);
      ir_node *new_bl = get_nodes_block(jmp);
      ir_node *new_ret;

      /* create the in-array for the new Ret */
      for (j = 0; j < n_ret_vals; ++j) {
        ir_node *pred = get_irn_n(ret, j);

        in[j] = is_Phi(pred) ? get_Phi_pred(pred, i) : pred;
      }

      new_ret = new_r_Return(irg, new_bl, in[0], n_ret_vals - 1, &in[1]);

      if (! is_Bad(new_ret)) {
        /*
         * The newly created node might be bad, if we
         * create it in a block with only Bad predecessors.
         * In that case ignore this block.
         *
         * We could even kill the jmp then ...
         */
        if (can_move_ret(new_ret)) {
          set_irn_link(new_ret, list);
          list = new_ret;
          ++n_rets;
        }
        else {
          set_irn_link(new_ret, final);
          final = new_ret;
          ++n_finals;
        }
      }
    }

    /*
     * if the memory of the old Return is a PhiM, remove it
     * from the keep-alives, or it will keep the block which
     * will crash the dominator algorithm.
     */
    phiM = get_Return_mem(ret);
    if (is_Phi(phiM)) {
      n = get_End_n_keepalives(end);
      for (i = 0; i < n; ++i) {
        if (get_End_keepalive(end, i) == phiM) {
          set_End_keepalive(end, i, new_r_Bad(irg));
          break;
        }
      }
    }
  }

  /*
   * Last step: Create a new endblock, with all nodes on the final
   * list as predecessors.
   */
  in = alloca(sizeof(*in) * n_finals);

  for (i = 0; final; ++i, final = get_irn_link(final))
    in[i] = final;

  exchange(endbl, new_r_Block(irg, n_finals, in));

  /* the end block is not automatically skiped, so do it here */
  set_irg_end_block(irg, skip_Id(get_irg_end_block(irg)));

  /* Invalidate analysis information:
   * Blocks become dead and new Eeturns were deleted, so dominator, outs and loop are inconsistent,
   * trouts and callee-state should be still valid
   */
  set_irg_dom_inconsistent(irg);
  set_irg_outs_inconsistent(irg);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_cf_inconsistent);
}
