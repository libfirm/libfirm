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
# include "config.h"
#endif

#include <assert.h>

#include "xmalloc.h"
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


static void remove_senseless_conds(ir_node *bl, void *data)
{
	int i, j;
	int n = get_irn_arity(bl);

	assert(is_Block(bl));

	for(i = 0; i < n; ++i) {
		ir_node *pred_i = get_irn_n(bl, i);
		ir_node *cond_i = skip_Proj(pred_i);

		for(j = i + 1; j < n; ++j) {
			ir_node *pred_j = get_irn_n(bl, j);
			ir_node *cond_j = skip_Proj(pred_j);

			if(cond_j == cond_i
					&& get_irn_opcode(cond_i) == iro_Cond
					&& get_irn_mode(get_Cond_selector(cond_i)) == mode_b) {

				ir_node *jmp = new_r_Jmp(current_ir_graph, get_nodes_block(cond_i));
				set_irn_n(bl, i, jmp);
				set_irn_n(bl, j, new_Bad());

				break;
			}
		}
	}
}


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

  /* clear the link field for ALL nodes first */
  set_irn_link(n, NULL);

  if (get_irn_op(n) == op_Block) {
    /* Remove Tuples */
    for (i = 0; i < get_Block_n_cfgpreds(n); i++) {
      /* GL @@@ : is this possible? if (get_opt_normalize()) -- added, all tests go through.
         A different order of optimizations might cause problems. */
      if (get_opt_normalize())
        set_Block_cfgpred(n, i, skip_Tuple(get_Block_cfgpred(n, i)));
    }

    /* see below */
    new_block = equivalent_node(n);
    if (new_block != n && ! is_Bad(new_block))
      exchange (n, new_block);

  } else if (get_opt_optimize() && (get_irn_mode(n) == mode_X)) {
    /* We will soon visit a block.  Optimize it before visiting! */
    ir_node *b = get_nodes_block(skip_Proj(n));

    if (!is_Bad(b)) {
      new_block = equivalent_node(b);

      while (irn_not_visited(b) && (!is_Bad(new_block)) && (new_block != b)) {
        /* We would have to run gigo if new is bad, so we
           promote it directly below. Nevertheless, we sometimes reach a block
           the first time through a dataflow node.  In this case we optimized the
           block as such and have to promote the Bad here. */
        assert((get_opt_control_flow_straightening() ||
                get_opt_control_flow_weak_simplification()) &&
               ("strange flag setting"));
        exchange (b, new_block);
        b = new_block;
        new_block = equivalent_node(b);
      }

      /* normally, we would create a Bad block here, but this must be
       * prevented, so just set it's cf to Bad.
       */
      if (is_Bad(new_block))
	exchange(n, new_Bad());
    }
  }
}

/**
 * Remove cf from dead block by inspecting dominance info
 * Do not replace blocks by Bad.  This optimization shall
 * ensure, that all Bad cfg preds are removed, and no new
 * other Bads are introduced.
 *
 * Must be run in the post walker.
 */
static void remove_dead_block_cf(ir_node *block, void *env)
{
  int i, n;

  /* check block predecessors and turn control flow into bad */
  for (i = 0, n = get_Block_n_cfgpreds(block); i < n; ++i) {
    ir_node *pred_X = get_Block_cfgpred(block, i);

    if (! is_Bad(pred_X)) {
      ir_node *pred_bl = get_nodes_block(skip_Proj(pred_X));

      if (is_Bad(pred_bl) || (get_Block_dom_depth(pred_bl) == -1))
        exchange (pred_X, new_Bad());
    }
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
  if (is_no_Block(n)) {
    ir_node *b = get_nodes_block(n);

    if ((get_irn_op(n) == op_Phi)) {
      /* Collect Phi nodes to compact ins along with block's ins. */
      set_irn_link(n, get_irn_link(b));
      set_irn_link(b, n);
    }
    else if ((get_irn_op(n) != op_Jmp) && !is_Bad(b)) {  /* Check for non empty block. */
      mark_Block_block_visited(b);
    }
  }
}

/** Returns true if pred is predecessor of block. */
static int is_pred_of(ir_node *pred, ir_node *b) {
  int i, n;

  for (i = 0, n = get_Block_n_cfgpreds(b); i < n; ++i) {
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

  /* Bad blocks will be optimized away, so we don't need space for them */
  if (is_Bad(pred))
    return 0;

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


/**
 * This method removed Bad cf preds from Blocks and Phis, and removes
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
 *       \      /                                      \      /
 *        \    /                                        |    /
 *        pred_b                                        |   /
 *         |   ____                                     |  /
 *         |  |    |                                    |  | |    |
 *         |  |    |       === optimized to ===>        \  | |    |
 *        loop_b   |                                     loop_b   |
 *         |  |    |                                      |  |    |
 *         |  |____|                                      |  |____|
 *         |                                              |
 *
 * If there is a Phi in pred_b, but we remove pred_b, we have to generate a
 * Phi in loop_b, that has the ins of the Phi in pred_b and a self referencing
 * backedge.
 * @@@ It is negotiable whether we should do this ... there might end up a copy
 * from the Phi in the loop when removing the Phis.
 */
static void optimize_blocks(ir_node *b, void *env) {
  int i, j, k, n, max_preds, n_preds, p_preds;
  ir_node *pred, *phi;
  ir_node **in;

  /* Count the number of predecessor if this block is merged with pred blocks
     that are empty. */
  max_preds = 0;
  for (i = 0, k = get_Block_n_cfgpreds(b); i < k; ++i) {
    max_preds += test_whether_dispensable(b, i);
  }
  in = xmalloc(max_preds * sizeof(*in));

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

  /*- Fix the Phi nodes of the current block -*/
  for (phi = get_irn_link(b); phi; ) {
    assert(get_irn_op(phi) == op_Phi);

    /* Find the new predecessors for the Phi */
    p_preds = 0;
    for (i = 0, n = get_Block_n_cfgpreds(b); i < n; ++i) {
      pred = get_nodes_block(get_Block_cfgpred(b, i));

      if (is_Bad(get_Block_cfgpred(b, i))) {
        /* case Phi 1: Do nothing */
      }
      else if (get_Block_block_visited(pred) + 1
                 < get_irg_block_visited(current_ir_graph)) {
        /* case Phi 2: It's an empty block and not yet visited. */
        ir_node *phi_pred = get_Phi_pred(phi, i);

        for (j = 0, k = get_Block_n_cfgpreds(pred); j < k; j++) {
          /* because of breaking loops, not all predecessors are Bad-clean,
           * so we must check this here again */
          if (! is_Bad(get_Block_cfgpred(pred, j))) {
            if (get_nodes_block(phi_pred) == pred) {
              /* case Phi 2a: */
              assert(get_irn_op(phi_pred) == op_Phi);  /* Block is empty!! */

              in[p_preds++] = get_Phi_pred(phi_pred, j);
            } else {
              /* case Phi 2b: */
              in[p_preds++] = phi_pred;
            }
          }
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
        in[p_preds++] = get_Phi_pred(phi, i);
      }
    }
    assert(p_preds <= max_preds);

    /* Fix the node */
    if (p_preds == 1)
      /* By removal of Bad ins the Phi might be degenerated. */
      exchange(phi, in[0]);
    else
      set_irn_in(phi, p_preds, in);

    phi = get_irn_link(phi);
  }

  /*- This happens only if merge between loop backedge and single loop entry.
      See special case above. -*/
  for (k = 0, n = get_Block_n_cfgpreds(b); k < n; ++k) {
    pred = get_nodes_block(get_Block_cfgpred(b, k));

    if (get_Block_block_visited(pred) + 1 < get_irg_block_visited(current_ir_graph)) {
      /* we found a predecessor block at position k that will be removed */
      for (phi = get_irn_link(pred); phi;) {
        /*
         * the previous phase may already changed the phi, and even
         * removed it at all, so check here if this node is still a phi
         */
        if (get_irn_op(phi) == op_Phi) {
          int q_preds = 0;

          /* move this phi from the predecessor into the block b */
          set_nodes_block(phi, b);

          /* first, copy all 0..k-1 predecessors */
          for (i = 0; i < k; i++) {
            pred = get_nodes_block(get_Block_cfgpred(b, i));

            if (is_Bad(get_Block_cfgpred(b, i))) {
              /* Do nothing */
            } else if (get_Block_block_visited(pred) + 1
               < get_irg_block_visited(current_ir_graph)) {
              /* It's an empty block and not yet visited. */
              for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
                /* @@@ Hier brauche ich Schleifeninformation!!! Kontrollflusskante
                   muss Rueckwaertskante sein! (An allen vier in[q_preds] = phi
                   Anweisungen.) Trotzdem tuts bisher!! */
                if (! is_Bad(get_Block_cfgpred(pred, j)))
                  in[q_preds++] = phi;
              }
            } else {
              in[q_preds++] = phi;
            }
          }

          /* now we are at k, copy the phi predecessors */
          pred = get_nodes_block(get_Block_cfgpred(b, k));
          for (i = 0; i < get_Phi_n_preds(phi); i++) {
            if (! is_Bad(get_Block_cfgpred(pred, i)))
              in[q_preds++] = get_Phi_pred(phi, i);
          }

          /* and now all the rest */
          for (i = k+1; i < get_Block_n_cfgpreds(b); i++) {
            pred = get_nodes_block(get_Block_cfgpred(b, i));

            if (is_Bad(get_Block_cfgpred(b, i))) {
              /* Do nothing */
            } else if (get_Block_block_visited(pred) +1
               < get_irg_block_visited(current_ir_graph)) {
              /* It's an empty block and not yet visited. */
              for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
                if (! is_Bad(get_Block_cfgpred(pred, j)))
                  in[q_preds++] = phi;
              }
            } else {
              in[q_preds++] = phi;
            }
          }

          /* Fix the node */
          if (q_preds == 1)
            exchange(phi, in[0]);
          else
            set_irn_in(phi, q_preds, in);

          assert(q_preds <= max_preds);
//        assert(p_preds == q_preds && "Wrong Phi Fix");
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
        ir_node *pred_block = get_Block_cfgpred(pred, j);

        /* because of breaking loops, not all predecessors are Bad-clean,
         * so we must check this here again */
        if (! is_Bad(pred_block))
          in[n_preds++] = pred_block;
      }
      /* Remove block as it might be kept alive. */
      exchange(pred, b/*new_Bad()*/);
    } else {
      /* case 3: */
      in[n_preds++] = get_Block_cfgpred(b, i);
    }
  }
  assert(n_preds <= max_preds);

  set_irn_in(b, n_preds, in);

  assert(get_irn_link(b) == NULL || (n_preds == p_preds && "Wrong Phi Fix"));

  xfree(in);
}


/* Optimizations of the control flow that also require changes of Phi nodes.
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
  int i, n;
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

  if (dom_state == dom_consistent && get_opt_optimize() && get_opt_unreachable_code()) {
    ir_node *end = get_irg_end(irg);

    /* we have dominace info, we can kill dead block */
    irg_block_walk_graph(irg, NULL, remove_dead_block_cf, NULL);

    /* fix the keep-alives */
    for (i = 0, n = get_End_n_keepalives(end); i < n; ++i) {
      ir_node *ka = get_End_keepalive(end, i);

      if (is_Block(ka) && (get_Block_dom_depth(ka) == -1))
	set_End_keepalive(end, i, new_Bad());
      if (is_Phi(ka) && (get_Block_dom_depth(get_nodes_block(ka)) == -1))
	set_End_keepalive(end, i, new_Bad());
    }
  }

  irg_block_walk_graph(current_ir_graph, NULL, remove_senseless_conds, NULL);
  /* Use block visited flag to mark non-empty blocks. */
  inc_irg_block_visited(irg);
  irg_walk(end, merge_blocks, collect_nodes, NULL);

  /* Optimize the standard code. */
  irg_block_walk(get_irg_end_block(irg), optimize_blocks, NULL, NULL);

  /* Walk all keep alives, optimize them if block, add to new in-array
     for end if useful. */
  in = NEW_ARR_F (ir_node *, 1);
  in[0] = get_nodes_block(end);
  inc_irg_visited(current_ir_graph);

  for (i = 0; i < get_End_n_keepalives(end); i++) {
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


  /* the verifyer doesn't work yet with floating nodes */
  if (get_irg_pinned(irg) == op_pin_state_pinned) {
    /* after optimize_cf(), only Bad data flow may remain. */
    if (irg_vrfy_bads(irg, BAD_DF | BAD_BLOCK | TUPLE)) {
      dump_ir_block_graph(irg, "-vrfy-cf");
      dump_ir_graph(irg, "-vrfy-cf");
      fprintf(stderr, "VRFY_BAD in optimize_cf()\n");
    }
  }

  current_ir_graph = rem;
}
