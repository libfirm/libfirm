/*
 * Project:     libFIRM
 * File name:   ir/opt/cfopt.c
 * Purpose:     control flow optimizations
 * Author:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"

#include "ircons.h"
#include "iropt_t.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irdump.h"
#include "irvrfy.h"

#include "array.h"

#include "irouts.h"
#include "irbackedge_t.h"

#include "irflag_t.h"
#include "firmstat.h"

#include "cfopt.h"

/*------------------------------------------------------------------*/
/* Control flow optimization.                                       */
/*                                                                  */
/* Removes Bad control flow predecessors and empty blocks.  A block */
/* is empty if it contains only a Jmp node.                         */
/* Blocks can only be removed if they are not needed for the        */
/* semantics of Phi nodes.                                          */
/*------------------------------------------------------------------*/

/**
 * Removes Tuples from Block control flow predecessors.
 * Optimizes blocks with equivalent_node().  This is tricky,
 * as we want to avoid nodes that have as block predecessor Bads.
 * Therefore we also optimize at control flow operations, depending
 * how we first reach the Block.
 */
static void merge_blocks(ir_node *n, void *env) {
  int i;
  ir_node *new_block;

  set_irn_link(n, NULL);

  if (get_irn_op(n) == op_Block) {
    /* Remove Tuples */
    for (i = 0; i < get_Block_n_cfgpreds(n); i++) {
      /* GL @@@ : is this possible? if (get_opt_normalize()) -- added, all tests go through.
         A different order of optimizations might cause problems. */
      if (get_opt_normalize())
        set_Block_cfgpred(n, i, skip_Tuple(get_Block_cfgpred(n, i)));
    }
    new_block = equivalent_node(n);
    if (new_block != n)
      exchange (n, new_block);

  } else if (get_opt_optimize() && (get_irn_mode(n) == mode_X)) {
    /* We will soon visit a block.  Optimize it before visiting! */
    ir_node *b        = get_nodes_block(n);

    if (!is_Bad(b)) {
      new_block = equivalent_node(b);

      while (irn_not_visited(b) && (!is_Bad(new_block)) && (new_block != b)) {
        /* We would have to run gigo if new is bad, so we
           promote it directly below. Nevertheless, we somtimes reach a block
           the first time through a dataflow node.  In this case we optimized the
           block as such and have to promote the Bad here. */
        assert(((b == new_block) ||
            get_opt_control_flow_straightening() ||
            get_opt_control_flow_weak_simplification()) &&
           ("strange flag setting"));
        exchange (b, new_block);
        b = new_block;
        new_block = equivalent_node(b);
      }
      b = new_block;
    }

    /*
     * BEWARE: do not kill floating notes here as they might be needed in
     * valid blocks because of global CSE.
     */
    if (is_Bad(b) && get_opt_normalize() &&
	get_op_pinned(get_irn_op(n)) == op_pin_state_pinned)
      exchange(n, new_Bad());
  }
}

/**
 * Collects all Phi nodes in link list of Block.
 * Marks all blocks "block_visited" if they contain a node other
 * than Jmp.
 * Replaces n by Bad if n is unreachable control flow. We do that
 * in the post walker, so we catch all blocks.
 */
static void collect_nodes(ir_node *n, void *env) {
  irg_dom_state *dom_state = env;

  if (is_no_Block(n)) {
    ir_node *b = get_nodes_block(n);

    /*
     * BEWARE: do not kill floating notes here as they might be needed in
     * valid blocks because of global CSE.
     */
    if (is_Bad(b) &&
	get_op_pinned(get_irn_op(n)) == op_pin_state_pinned) {
      /* previous merge_blocks() may have killed dead blocks */
      exchange(n, new_Bad());
    }
    else if ((get_irn_op(n) == op_Phi)) {
      /* Collect Phi nodes to compact ins along with block's ins. */
      set_irn_link(n, get_irn_link(b));
      set_irn_link(b, n);
    } else if ((get_irn_op(n) != op_Jmp) && !is_Bad(b)) {  /* Check for non empty block. */
      mark_Block_block_visited(b);
    }
  }
  else {
    /* delete dead blocks: if we have dominator information, this can easily be detected
     * BEWARE: don't kill the end block */
    if (*dom_state == dom_consistent &&
	n != get_irg_end_block(current_ir_graph) &&
	get_Block_dom_depth(n) == -1 &&
	get_opt_unreachable_code()) {
      exchange (n, new_Bad());
    }
  }
}

/** Returns true if pred is predecessor of block. */
static int is_pred_of(ir_node *pred, ir_node *b) {
  int i;
  for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
    ir_node *b_pred = get_nodes_block(get_Block_cfgpred(b, i));
    if (b_pred == pred) return 1;
  }
  return 0;
}


/** Test wether we can optimize away pred block pos of b.
 *
 *  @param  b    A block node.
 *  @param  pos  The position of the predecessor block to judge about.
 *
 *  @returns     The number of predecessors
 *
 *  The test is rather tricky.
 *
 *  The situation is something like the following:
 *
 *                 if-block
 *                  /   \
 *              then-b  else-b
 *                  \   /
 *                    b
 *
 *     b merges the control flow of an if-then-else.  We may not remove
 *     the 'then' _and_ the 'else' block of an 'if' if there is a Phi
 *     node in b, even if both are empty.  The destruction of this Phi
 *     requires that a copy is added before the merge.  We have to
 *     keep one of the case blocks to place the copies in.
 *
 *     To perform the test for pos, we must regard preds before pos
 *     as already removed.
 **/
static int test_whether_dispensable(ir_node *b, int pos) {
  int i, j, n_preds = 1;
  int dispensable = 1;
  ir_node *cfop = get_Block_cfgpred(b, pos);
  ir_node *pred = get_nodes_block(cfop);

  if (get_Block_block_visited(pred) + 1
      < get_irg_block_visited(current_ir_graph)) {

    if (!get_opt_optimize() || !get_opt_control_flow_strong_simplification()) {
      /* Mark block so that is will not be removed: optimization is turned off. */
      set_Block_block_visited(pred, get_irg_block_visited(current_ir_graph)-1);
      return 1;
    }

    /* Seems to be empty. At least we detected this in collect_nodes. */
    if (!get_irn_link(b)) {
      /* There are no Phi nodes ==> all predecessors are dispensable. */
      n_preds = get_Block_n_cfgpreds(pred);
    } else {
      /* b's pred blocks and pred's pred blocks must be pairwise disjunct.
         Work preds < pos as if they were already removed. */
      for (i = 0; i < pos; i++) {
        ir_node *b_pred = get_nodes_block(get_Block_cfgpred(b, i));
        if (get_Block_block_visited(b_pred) + 1
            < get_irg_block_visited(current_ir_graph)) {
          for (j = 0; j < get_Block_n_cfgpreds(b_pred); j++) {
            ir_node *b_pred_pred = get_nodes_block(get_Block_cfgpred(b_pred, j));
            if (is_pred_of(b_pred_pred, pred)) dispensable = 0;
          }
        } else {
          if (is_pred_of(b_pred, pred)) dispensable = 0;
        }
      }
      for (i = pos +1; i < get_Block_n_cfgpreds(b); i++) {
        ir_node *b_pred = get_nodes_block(get_Block_cfgpred(b, i));
        if (is_pred_of(b_pred, pred)) dispensable = 0;
      }
      if (!dispensable) {
        set_Block_block_visited(pred, get_irg_block_visited(current_ir_graph)-1);
        n_preds = 1;
      } else {
        n_preds = get_Block_n_cfgpreds(pred);
      }
    }
  }

  return n_preds;
}


/* This method removed Bad cf preds from Blocks and Phis, and removes
 * empty blocks.  A block is empty if it only contains Phi and Jmp nodes.
 *
 * We first adapt Phi nodes, then Block nodes, as we need the old ins
 * of the Block to adapt the Phi nodes.  We do this by computing new
 * in arrays, and then replacing the old ones.  So far we compute new in arrays
 * for all nodes, not regarding whether there is a possibility for optimization.
 *
 * For each predecessor p of a Block b there are three cases:
 *  1. The predecessor p is a Bad node:  just skip it.  The in array of b shrinks by one.
 *  2. The predecessor p is empty.  Remove p.  All predecessors of p are now
 *     predecessors of b.
 *  3. The predecessor p is a block containing useful code.  Just keep p as is.
 *
 * For Phi nodes f we have to check the conditions at the Block of f.
 * For cases 1 and 3 we proceed as for Blocks.  For case 2 we can have two
 * cases:
 *  2a: The old precessor of the Phi f is a Phi pred_f IN THE BLOCK REMOVED.  In this
 *      case we proceed as for blocks. We remove pred_f.  All
 *      predecessors of pred_f now are predecessors of f.
 *  2b: The old predecessor of f is NOT in the block removed. It might be a Phi, too.
 *      We have to replicate f for each predecessor of the removed block. Or, with
 *      other words, the removed predecessor block has exactly one predecessor.
 *
 * Further there is a special case for self referencing blocks:
 *
 *    then_b     else_b                              then_b  else_b
 *       \      /				       \      /
 *        \    /				        |    /
 *        pred_b				        |   /
 *         |   ____				        |  /  ____
 *         |  |    |				        |  | |    |
 *         |  |    |       === optimized to ===>        \  | |    |
 *        loop_b   |				         loop_b   |
 *         |  |    |				          |  |    |
 *         |  |____|				          |  |____|
 *         |     				          |
 *
 * If there is a Phi in pred_b, but we remove pred_b, we have to generate a
 * Phi in loop_b, that has the ins of the Phi in pred_b and a self referencing
 * backedge.
 * @@@ It is negotiable whether we should do this ... there might end up a copy
 * from the Phi in the loop when removing the Phis.
 */
static void optimize_blocks(ir_node *b, void *env) {
  int i, j, k, max_preds, n_preds;
  ir_node *pred, *phi;
  ir_node **in;

  /* Count the number of predecessor if this block is merged with pred blocks
     that are empty. */
  max_preds = 0;
  for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
    max_preds += test_whether_dispensable(b, i);
  }
  in = (ir_node **) malloc(max_preds * sizeof(ir_node *));

/*-
  printf(" working on "); DDMN(b);
  for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
    pred = get_nodes_block(get_Block_cfgpred(b, i));
    if (is_Bad(get_Block_cfgpred(b, i))) {
      printf("  removing Bad %i\n ", i);
    } else if (get_Block_block_visited(pred) +1
           < get_irg_block_visited(current_ir_graph)) {
      printf("  removing pred %i ", i); DDMN(pred);
    } else { printf("  Nothing to do for "); DDMN(pred); }
  }
  * end Debug output -*/

  /*- Fix the Phi nodes -*/
  phi = get_irn_link(b);
  while (phi) {
    assert(get_irn_op(phi) == op_Phi);
    /* Find the new predecessors for the Phi */
    n_preds = 0;
    for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
      pred = get_nodes_block(get_Block_cfgpred(b, i));
      if (is_Bad(get_Block_cfgpred(b, i))) {
        /* case Phi 1: Do nothing */
      } else if (get_Block_block_visited(pred) + 1
                 < get_irg_block_visited(current_ir_graph)) {
        /* case Phi 2: It's an empty block and not yet visited. */
        ir_node *phi_pred = get_Phi_pred(phi, i);

        for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
          if (get_nodes_block(phi_pred) == pred) {
	    /* case Phi 2a: */
            assert(get_irn_op(phi_pred) == op_Phi);  /* Block is empty!! */
            in[n_preds] = get_Phi_pred(phi_pred, j);
          } else {
	    /* case Phi 2b: */
            in[n_preds] = phi_pred;
          }
          n_preds++;
        }

        /* The Phi_pred node is replaced now if it is a Phi.

	   Somehow the removed Phi node can be used legally in loops.
	   Therefore we replace the old phi by the new one.

	   Further we have to remove the old Phi node by replacing it
	   by Bad.  Else it will remain in the keepalive array of End
	   and cause illegal situations.  So if there is no loop, we should
	   replace it by Bad.
	*/
        if (get_nodes_block(phi_pred) == pred) {
          /* remove the Phi as it might be kept alive. Further there
             might be other users. */
          exchange(phi_pred, phi);  /* geht, ist aber doch semantisch falsch! Warum?? */
        }

      } else {
	/* case Phi 3: */
        in[n_preds] = get_Phi_pred(phi, i);
        n_preds ++;
      }
    }

    /* Fix the node */
    if (n_preds == 1)
      /* By removal of Bad ins the Phi might be degenerated. */
      exchange(phi, in[0]);
    else
      set_irn_in(phi, n_preds, in);

    phi = get_irn_link(phi);
  }

  /*- This happens only if merge between loop backedge and single loop entry.
      See special case above. -*/
  for (k = 0; k < get_Block_n_cfgpreds(b); k++) {
    pred = get_nodes_block(get_Block_cfgpred(b, k));
    if (get_Block_block_visited(pred)+1 < get_irg_block_visited(current_ir_graph)) {
      phi = get_irn_link(pred);
      while (phi) {
        if (get_irn_op(phi) == op_Phi) {
          set_nodes_block(phi, b);

          n_preds = 0;
          for (i = 0; i < k; i++) {
            pred = get_nodes_block(get_Block_cfgpred(b, i));
            if (is_Bad(get_Block_cfgpred(b, i))) {
              /* Do nothing */
            } else if (get_Block_block_visited(pred) +1
               < get_irg_block_visited(current_ir_graph)) {
              /* It's an empty block and not yet visited. */
              for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
                /* @@@ Hier brauche ich Schleifeninformation!!! Kontrollflusskante
                   muss Rueckwaertskante sein! (An allen vier in[n_preds] = phi
                   Anweisungen.) Trotzdem tuts bisher!! */
                in[n_preds] = phi;
                n_preds++;
              }
            } else {
              in[n_preds] = phi;
              n_preds++;
            }
          }
          for (i = 0; i < get_Phi_n_preds(phi); i++) {
            in[n_preds] = get_Phi_pred(phi, i);
            n_preds++;
          }
          for (i = k+1; i < get_Block_n_cfgpreds(b); i++) {
            pred = get_nodes_block(get_Block_cfgpred(b, i));
            if (is_Bad(get_Block_cfgpred(b, i))) {
              /* Do nothing */
            } else if (get_Block_block_visited(pred) +1
               < get_irg_block_visited(current_ir_graph)) {
              /* It's an empty block and not yet visited. */
              for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
                in[n_preds] = phi;
                n_preds++;
              }
            } else {
              in[n_preds] = phi;
              n_preds++;
            }
          }
          if (n_preds == 1)
            exchange(phi, in[0]);
          else
            set_irn_in(phi, n_preds, in);
        }
        phi = get_irn_link(phi);
      }
    }
  }

  /*- Fix the block -*/
  n_preds = 0;
  for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
    pred = get_nodes_block(get_Block_cfgpred(b, i));
    if (is_Bad(get_Block_cfgpred(b, i))) {
      /* case 1: Do nothing */
    } else if (get_Block_block_visited(pred) +1
           < get_irg_block_visited(current_ir_graph)) {
      /* case 2: It's an empty block and not yet visited. */
      assert(get_Block_n_cfgpreds(b) > 1);
                        /* Else it should be optimized by equivalent_node. */
      for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
        in[n_preds] = get_Block_cfgpred(pred, j);
        n_preds++;
      }
      /* Remove block as it might be kept alive. */
      exchange(pred, b/*new_Bad()*/);
    } else {
      /* case 3: */
      in[n_preds] = get_Block_cfgpred(b, i);
      n_preds ++;
    }
  }
  set_irn_in(b, n_preds, in);

  free(in);
}


/* Optimizations of the control flow that also reqire changes of Phi nodes.
 *
 * This optimization performs two passes over the graph.
 *
 * The first pass collects all Phi nodes in a link list in the block
 * nodes.  Further it performs simple control flow optimizations.
 * Finally it marks all blocks that do not contain useful
 * computations, i.e., these blocks might be removed.
 *
 * The second pass performs the optimizations intended by this algorithm.
 * It walks only over block nodes and adapts these and the Phi nodes in these blocks,
 * which it finds in a linked list computed by the first pass.
 *
 * We use the block_visited flag to mark empty blocks in the first
 * phase.
 * @@@ It would be better to add a struct in the link field
 * that keeps the Phi list and the mark.  Place it on an obstack, as
 * we will lose blocks and thereby generate mem leaks.
 */
void optimize_cf(ir_graph *irg) {
  int i;
  ir_node **in;
  ir_node *end = get_irg_end(irg);
  ir_graph *rem = current_ir_graph;
  irg_dom_state dom_state = get_irg_dom_state(current_ir_graph);
  current_ir_graph = irg;

  /* Handle graph state */
  assert(get_irg_phase_state(irg) != phase_building);
  if (get_irg_outs_state(current_ir_graph) == outs_consistent)
    set_irg_outs_inconsistent(current_ir_graph);
  if (get_irg_dom_state(current_ir_graph) == dom_consistent)
    set_irg_dom_inconsistent(current_ir_graph);

  /* Use block visited flag to mark non-empty blocks. */
  inc_irg_block_visited(irg);
  irg_walk(end, merge_blocks, collect_nodes, &dom_state);

  /* Optimize the standard code. */
  irg_block_walk(get_irg_end_block(irg), optimize_blocks, NULL, NULL);

  /* Walk all keep alives, optimize them if block, add to new in-array
     for end if useful. */
  in = NEW_ARR_F (ir_node *, 1);
  in[0] = get_nodes_block(end);
  inc_irg_visited(current_ir_graph);
  for(i = 0; i < get_End_n_keepalives(end); i++) {
    ir_node *ka = get_End_keepalive(end, i);
    if (irn_not_visited(ka)) {
      if ((get_irn_op(ka) == op_Block) && Block_not_block_visited(ka)) {
        set_irg_block_visited(current_ir_graph,  /* Don't walk all the way to Start. */
              get_irg_block_visited(current_ir_graph)-1);
        irg_block_walk(ka, optimize_blocks, NULL, NULL);
        mark_irn_visited(ka);
        ARR_APP1 (ir_node *, in, ka);
      } else if (get_irn_op(ka) == op_Phi) {
        mark_irn_visited(ka);
        ARR_APP1 (ir_node *, in, ka);
      }
    }
  }
  /* DEL_ARR_F(end->in);   GL @@@ tut nicht ! */
  end->in = in;

  /* after optimize_cf(), only Bad data flow may remain. */
  if (irg_vrfy_bads(irg, BAD_DF | BAD_BLOCK | TUPLE)) {
    dump_ir_block_graph(irg, "-vrfy-cf");
    dump_ir_graph(irg, "-vrfy-cf");
    fprintf(stderr, "VRFY_BAD in optimize_cf()\n");
  }

  current_ir_graph = rem;
}
