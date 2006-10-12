/*
 * Project:     libFIRM
 * File name:   ir/ir/irgopt.c
 * Purpose:     Optimizations for a whole ir graph, i.e., a procedure.
 * Author:      Christian Schaefer, Goetz Lindenmaier
 * Modified by: Sebastian Felis, Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"

#include "ircons.h"
#include "iropt_t.h"
#include "cfopt.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "array.h"
#include "pset.h"
#include "pmap.h"
#include "pdeq.h"       /* Fuer code placement */
#include "xmalloc.h"

#include "irouts.h"
#include "irloop_t.h"
#include "irbackedge_t.h"
#include "cgana.h"
#include "trouts.h"


#include "irflag_t.h"
#include "irhooks.h"
#include "iredges_t.h"
#include "irtools.h"

/*------------------------------------------------------------------*/
/* apply optimizations of iropt to all nodes.                       */
/*------------------------------------------------------------------*/

/**
 * A wrapper around optimize_inplace_2() to be called from a walker.
 */
static void optimize_in_place_wrapper (ir_node *n, void *env) {
  ir_node *optimized = optimize_in_place_2(n);
  if (optimized != n) exchange (n, optimized);
}

/**
 * Do local optimizations for a node.
 *
 * @param n  the IR-node where to start. Typically the End node
 *           of a graph
 *
 * @note current_ir_graph must be set
 */
static INLINE void do_local_optimize(ir_node *n) {
  /* Handle graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);

  if (get_opt_global_cse())
    set_irg_pinned(current_ir_graph, op_pin_state_floats);
  set_irg_outs_inconsistent(current_ir_graph);
  set_irg_doms_inconsistent(current_ir_graph);
  set_irg_loopinfo_inconsistent(current_ir_graph);

  /* Clean the value_table in irg for the CSE. */
  del_identities(current_ir_graph->value_table);
  current_ir_graph->value_table = new_identities();

  /* walk over the graph */
  irg_walk(n, firm_clear_link, optimize_in_place_wrapper, NULL);
}

/* Applies local optimizations (see iropt.h) to all nodes reachable from node n */
void local_optimize_node(ir_node *n) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = get_irn_irg(n);

  do_local_optimize(n);

  current_ir_graph = rem;
}

/**
 * Block-Walker: uses dominance depth to mark dead blocks.
 */
static void kill_dead_blocks(ir_node *block, void *env)
{
  if (get_Block_dom_depth(block) < 0) {
    /*
     * Note that the new dominance code correctly handles
     * the End block, i.e. it is always reachable from Start
     */
    set_Block_dead(block);
  }
}

/* Applies local optimizations (see iropt.h) to all nodes reachable from node n. */
void local_optimize_graph(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  if (get_irg_dom_state(irg) == dom_consistent)
    irg_block_walk_graph(irg, NULL, kill_dead_blocks, NULL);

  do_local_optimize(get_irg_end(irg));

  current_ir_graph = rem;
}

/**
 * Enqueue all users of a node to a wait queue.
 * Handles mode_T nodes.
 */
static void enqueue_users(ir_node *n, pdeq *waitq) {
  const ir_edge_t *edge;

  foreach_out_edge(n, edge) {
    ir_node *succ = get_edge_src_irn(edge);

    if (get_irn_link(succ) != waitq) {
      pdeq_putr(waitq, succ);
      set_irn_link(succ, waitq);
    }
    if (get_irn_mode(succ) == mode_T) {
      /* A mode_T node has Proj's. Because most optimizations
         run on the Proj's we have to enqueue them also. */
      enqueue_users(succ, waitq);
    }
  }
}

/**
 * Data flow optimization walker.
 * Optimizes all nodes and enqueue it's users
 * if done.
 */
static void opt_walker(ir_node *n, void *env) {
  pdeq *waitq = env;
  ir_node *optimized;

  optimized = optimize_in_place_2(n);
  set_irn_link(optimized, NULL);

  if (optimized != n) {
    enqueue_users(n, waitq);
    exchange(n, optimized);
  }
}

/* Applies local optimizations to all nodes in the graph until fixpoint. */
void optimize_graph_df(ir_graph *irg) {
  pdeq     *waitq = new_pdeq();
  int      state = edges_activated(irg);
  ir_graph *rem = current_ir_graph;

  current_ir_graph = irg;

  if (! state)
    edges_activate(irg);

  if (get_opt_global_cse())
    set_irg_pinned(current_ir_graph, op_pin_state_floats);

  /* Clean the value_table in irg for the CSE. */
  del_identities(irg->value_table);
  irg->value_table = new_identities();

  if (get_irg_dom_state(irg) == dom_consistent)
    irg_block_walk_graph(irg, NULL, kill_dead_blocks, NULL);

  /* invalidate info */
  set_irg_outs_inconsistent(irg);
  set_irg_doms_inconsistent(irg);
  set_irg_loopinfo_inconsistent(irg);

  /* walk over the graph */
  irg_walk_graph(irg, NULL, opt_walker, waitq);

  /* finish the wait queue */
  while (! pdeq_empty(waitq)) {
    ir_node *n = pdeq_getl(waitq);
    if (! is_Bad(n))
      opt_walker(n, waitq);
  }

  del_pdeq(waitq);

  if (! state)
    edges_deactivate(irg);

  current_ir_graph = rem;
}


/*------------------------------------------------------------------*/
/* Routines for dead node elimination / copying garbage collection  */
/* of the obstack.                                                  */
/*------------------------------------------------------------------*/

/**
 * Remember the new node in the old node by using a field all nodes have.
 */
#define set_new_node(oldn, newn)  set_irn_link(oldn, newn)

/**
 * Get this new node, before the old node is forgotten.
 */
#define get_new_node(oldn) get_irn_link(oldn)

/**
 * Check if a new node was set.
 */
#define has_new_node(n) (get_new_node(n) != NULL)

/**
 * We use the block_visited flag to mark that we have computed the
 * number of useful predecessors for this block.
 * Further we encode the new arity in this flag in the old blocks.
 * Remembering the arity is useful, as it saves a lot of pointer
 * accesses.  This function is called for all Phi and Block nodes
 * in a Block.
 */
static INLINE int
compute_new_arity(ir_node *b) {
  int i, res, irn_arity;
  int irg_v, block_v;

  irg_v = get_irg_block_visited(current_ir_graph);
  block_v = get_Block_block_visited(b);
  if (block_v >= irg_v) {
    /* we computed the number of preds for this block and saved it in the
       block_v flag */
    return block_v - irg_v;
  } else {
    /* compute the number of good predecessors */
    res = irn_arity = get_irn_arity(b);
    for (i = 0; i < irn_arity; i++)
      if (get_irn_opcode(get_irn_n(b, i)) == iro_Bad) res--;
    /* save it in the flag. */
    set_Block_block_visited(b, irg_v + res);
    return res;
  }
}

/**
 * Copies the node to the new obstack. The Ins of the new node point to
 * the predecessors on the old obstack.  For block/phi nodes not all
 * predecessors might be copied.  n->link points to the new node.
 * For Phi and Block nodes the function allocates in-arrays with an arity
 * only for useful predecessors.  The arity is determined by counting
 * the non-bad predecessors of the block.
 *
 * @param n    The node to be copied
 * @param env  if non-NULL, the node number attribute will be copied to the new node
 *
 * Note: Also used for loop unrolling.
 */
static void copy_node(ir_node *n, void *env) {
  ir_node *nn, *block;
  int new_arity;
  ir_op *op = get_irn_op(n);
  int copy_node_nr = env != NULL;

  /* The end node looses it's flexible in array.  This doesn't matter,
     as dead node elimination builds End by hand, inlineing doesn't use
     the End node. */
  /* assert(op == op_End ||  ((_ARR_DESCR(n->in))->cookie != ARR_F_MAGIC)); */

  if (op == op_Bad) {
    /* node copied already */
    return;
  } else if (op == op_Block) {
    block = NULL;
    new_arity = compute_new_arity(n);
    n->attr.block.graph_arr = NULL;
  } else {
    block = get_nodes_block(n);
    if (op == op_Phi) {
      new_arity = compute_new_arity(block);
    } else {
      new_arity = get_irn_arity(n);
    }
  }
  nn = new_ir_node(get_irn_dbg_info(n),
           current_ir_graph,
           block,
           op,
           get_irn_mode(n),
           new_arity,
           get_irn_in(n) + 1);
  /* Copy the attributes.  These might point to additional data.  If this
     was allocated on the old obstack the pointers now are dangling.  This
     frees e.g. the memory of the graph_arr allocated in new_immBlock. */
  copy_node_attr(n, nn);
  new_backedge_info(nn);

#if DEBUG_libfirm
  if (copy_node_nr) {
    /* for easier debugging, we want to copy the node numbers too */
    nn->node_nr = n->node_nr;
  }
#endif

  set_new_node(n, nn);
  hook_dead_node_elim_subst(current_ir_graph, n, nn);
}

/**
 * Copies new predecessors of old node to new node remembered in link.
 * Spare the Bad predecessors of Phi and Block nodes.
 */
void
copy_preds(ir_node *n, void *env) {
  ir_node *nn, *block;
  int i, j, irn_arity;

  nn = get_new_node(n);

  /* printf("\n old node: "); DDMSG2(n);
     printf(" new node: "); DDMSG2(nn);
     printf(" arities: old: %d, new: %d\n", get_irn_arity(n), get_irn_arity(nn)); */

  if (is_Block(n)) {
    /* Don't copy Bad nodes. */
    j = 0;
    irn_arity = get_irn_arity(n);
    for (i = 0; i < irn_arity; i++)
      if (! is_Bad(get_irn_n(n, i))) {
        set_irn_n (nn, j, get_new_node(get_irn_n(n, i)));
        /*if (is_backedge(n, i)) set_backedge(nn, j);*/
        j++;
      }
    /* repair the block visited flag from above misuse. Repair it in both
       graphs so that the old one can still be used. */
    set_Block_block_visited(nn, 0);
    set_Block_block_visited(n, 0);
    /* Local optimization could not merge two subsequent blocks if
       in array contained Bads.  Now it's possible.
       We don't call optimize_in_place as it requires
       that the fields in ir_graph are set properly. */
    if ((get_opt_control_flow_straightening()) &&
        (get_Block_n_cfgpreds(nn) == 1) &&
        (get_irn_op(get_Block_cfgpred(nn, 0)) == op_Jmp)) {
      ir_node *old = get_nodes_block(get_Block_cfgpred(nn, 0));
      if (nn == old) {
        /* Jmp jumps into the block it is in -- deal self cycle. */
        assert(is_Bad(get_new_node(get_irg_bad(current_ir_graph))));
        exchange(nn, get_new_node(get_irg_bad(current_ir_graph)));
      } else {
        exchange(nn, old);
      }
    }
  } else if (get_irn_op(n) == op_Phi) {
    /* Don't copy node if corresponding predecessor in block is Bad.
       The Block itself should not be Bad. */
    block = get_nodes_block(n);
    set_irn_n(nn, -1, get_new_node(block));
    j = 0;
    irn_arity = get_irn_arity(n);
    for (i = 0; i < irn_arity; i++)
      if (! is_Bad(get_irn_n(block, i))) {
        set_irn_n(nn, j, get_new_node(get_irn_n(n, i)));
        /*if (is_backedge(n, i)) set_backedge(nn, j);*/
        j++;
      }
    /* If the pre walker reached this Phi after the post walker visited the
       block block_visited is > 0. */
    set_Block_block_visited(get_nodes_block(n), 0);
    /* Compacting the Phi's ins might generate Phis with only one
       predecessor. */
    if (get_irn_arity(nn) == 1)
      exchange(nn, get_irn_n(nn, 0));
  } else {
    irn_arity = get_irn_arity(n);
    for (i = -1; i < irn_arity; i++)
      set_irn_n (nn, i, get_new_node(get_irn_n(n, i)));
  }
  /* Now the new node is complete.  We can add it to the hash table for CSE.
     @@@ inlining aborts if we identify End. Why? */
  if (get_irn_op(nn) != op_End)
    add_identities(current_ir_graph->value_table, nn);
}

/**
 * Copies the graph recursively, compacts the keep-alives of the end node.
 *
 * @param irg           the graph to be copied
 * @param copy_node_nr  If non-zero, the node number will be copied
 */
static void copy_graph(ir_graph *irg, int copy_node_nr) {
  ir_node *oe, *ne, *ob, *nb, *om, *nm; /* old end, new end, old bad, new bad, old NoMem, new NoMem */
  ir_node *ka;      /* keep alive */
  int i, irn_arity;
  unsigned long vfl;

  /* Some nodes must be copied by hand, sigh */
  vfl = get_irg_visited(irg);
  set_irg_visited(irg, vfl + 1);

  oe = get_irg_end(irg);
  mark_irn_visited(oe);
  /* copy the end node by hand, allocate dynamic in array! */
  ne = new_ir_node(get_irn_dbg_info(oe),
           irg,
           NULL,
           op_End,
           mode_X,
           -1,
           NULL);
  /* Copy the attributes.  Well, there might be some in the future... */
  copy_node_attr(oe, ne);
  set_new_node(oe, ne);

  /* copy the Bad node */
  ob = get_irg_bad(irg);
  mark_irn_visited(ob);
  nb = new_ir_node(get_irn_dbg_info(ob),
           irg,
           NULL,
           op_Bad,
           mode_T,
           0,
           NULL);
  copy_node_attr(ob, nb);
  set_new_node(ob, nb);

  /* copy the NoMem node */
  om = get_irg_no_mem(irg);
  mark_irn_visited(om);
  nm = new_ir_node(get_irn_dbg_info(om),
           irg,
           NULL,
           op_NoMem,
           mode_M,
           0,
           NULL);
  copy_node_attr(om, nm);
  set_new_node(om, nm);

  /* copy the live nodes */
  set_irg_visited(irg, vfl);
  irg_walk(get_nodes_block(oe), copy_node, copy_preds, INT_TO_PTR(copy_node_nr));

  /* Note: from yet, the visited flag of the graph is equal to vfl + 1 */

  /* visit the anchors as well */
  for (i = anchor_max - 1; i >= 0; --i) {
    ir_node *n = irg->anchors[i];

    if (n && (get_irn_visited(n) <= vfl)) {
      set_irg_visited(irg, vfl);
      irg_walk(n, copy_node, copy_preds, INT_TO_PTR(copy_node_nr));
    }
  }

  /* copy_preds for the end node ... */
  set_nodes_block(ne, get_new_node(get_nodes_block(oe)));

  /*- ... and now the keep alives. -*/
  /* First pick the not marked block nodes and walk them.  We must pick these
     first as else we will oversee blocks reachable from Phis. */
  irn_arity = get_End_n_keepalives(oe);
  for (i = 0; i < irn_arity; i++) {
    ka = get_End_keepalive(oe, i);
    if (is_Block(ka)) {
      if (get_irn_visited(ka) <= vfl) {
        /* We must keep the block alive and copy everything reachable */
        set_irg_visited(irg, vfl);
        irg_walk(ka, copy_node, copy_preds, INT_TO_PTR(copy_node_nr));
      }
      add_End_keepalive(ne, get_new_node(ka));
    }
  }

  /* Now pick other nodes.  Here we will keep all! */
  irn_arity = get_End_n_keepalives(oe);
  for (i = 0; i < irn_arity; i++) {
    ka = get_End_keepalive(oe, i);
    if (!is_Block(ka)) {
      if (get_irn_visited(ka) <= vfl) {
        /* We didn't copy the node yet.  */
        set_irg_visited(irg, vfl);
        irg_walk(ka, copy_node, copy_preds, INT_TO_PTR(copy_node_nr));
      }
      add_End_keepalive(ne, get_new_node(ka));
    }
  }

  /* start block sometimes only reached after keep alives */
  set_nodes_block(nb, get_new_node(get_nodes_block(ob)));
  set_nodes_block(nm, get_new_node(get_nodes_block(om)));
}

/**
 * Copies the graph reachable from current_ir_graph->end to the obstack
 * in current_ir_graph and fixes the environment.
 * Then fixes the fields in current_ir_graph containing nodes of the
 * graph.
 *
 * @param copy_node_nr  If non-zero, the node number will be copied
 */
static void
copy_graph_env(int copy_node_nr) {
  ir_graph *irg = current_ir_graph;
  ir_node *old_end, *n;
  int i;

  /* remove end_except and end_reg nodes */
  old_end = get_irg_end(irg);
  set_irg_end_except (irg, old_end);
  set_irg_end_reg    (irg, old_end);

  /* Not all nodes remembered in irg might be reachable
     from the end node.  Assure their link is set to NULL, so that
     we can test whether new nodes have been computed. */
  for (i = anchor_max - 1; i >= 0; --i) {
    if (irg->anchors[i])
      set_new_node(irg->anchors[i], NULL);
  }
  /* we use the block walk flag for removing Bads from Blocks ins. */
  inc_irg_block_visited(irg);

  /* copy the graph */
  copy_graph(irg, copy_node_nr);

  /* fix the fields in irg */
  old_end = get_irg_end(irg);
  for (i = anchor_max - 1; i >= 0; --i) {
    n = irg->anchors[i];
    if (n)
      irg->anchors[i] = get_new_node(n);
  }
  free_End(old_end);
}

/**
 * Copies all reachable nodes to a new obstack.  Removes bad inputs
 * from block nodes and the corresponding inputs from Phi nodes.
 * Merges single exit blocks with single entry blocks and removes
 * 1-input Phis.
 * Adds all new nodes to a new hash table for CSE.  Does not
 * perform CSE, so the hash table might contain common subexpressions.
 */
void
dead_node_elimination(ir_graph *irg) {
  if (get_opt_optimize() && get_opt_dead_node_elimination()) {
    ir_graph *rem;
    int rem_ipview = get_interprocedural_view();
    struct obstack *graveyard_obst = NULL;
    struct obstack *rebirth_obst   = NULL;
    assert(! edges_activated(irg) && "dead node elimination requires disabled edges");

    /* inform statistics that we started a dead-node elimination run */
    hook_dead_node_elim(irg, 1);

    /* Remember external state of current_ir_graph. */
    rem = current_ir_graph;
    current_ir_graph = irg;
    set_interprocedural_view(0);

    assert(get_irg_phase_state(irg) != phase_building);

    /* Handle graph state */
    free_callee_info(irg);
    free_irg_outs(irg);
    free_trouts();

    /* @@@ so far we loose loops when copying */
    free_loop_information(irg);

    set_irg_doms_inconsistent(irg);

    /* A quiet place, where the old obstack can rest in peace,
       until it will be cremated. */
    graveyard_obst = irg->obst;

    /* A new obstack, where the reachable nodes will be copied to. */
    rebirth_obst = xmalloc(sizeof(*rebirth_obst));
    irg->obst = rebirth_obst;
    obstack_init(irg->obst);
    irg->last_node_idx = 0;

    /* We also need a new value table for CSE */
    del_identities(irg->value_table);
    irg->value_table = new_identities();

    /* Copy the graph from the old to the new obstack */
    copy_graph_env(/*copy_node_nr=*/1);

    /* Free memory from old unoptimized obstack */
    obstack_free(graveyard_obst, 0);  /* First empty the obstack ... */
    xfree (graveyard_obst);           /* ... then free it.           */

    /* inform statistics that the run is over */
    hook_dead_node_elim(irg, 0);

    current_ir_graph = rem;
    set_interprocedural_view(rem_ipview);
  }
}

/**
 * Relink bad predecessors of a block and store the old in array to the
 * link field. This function is called by relink_bad_predecessors().
 * The array of link field starts with the block operand at position 0.
 * If block has bad predecessors, create a new in array without bad preds.
 * Otherwise let in array untouched.
 */
static void relink_bad_block_predecessors(ir_node *n, void *env) {
  ir_node **new_in, *irn;
  int i, new_irn_n, old_irn_arity, new_irn_arity = 0;

  /* if link field of block is NULL, look for bad predecessors otherwise
     this is already done */
  if (get_irn_op(n) == op_Block &&
      get_irn_link(n) == NULL) {

    /* save old predecessors in link field (position 0 is the block operand)*/
    set_irn_link(n, get_irn_in(n));

    /* count predecessors without bad nodes */
    old_irn_arity = get_irn_arity(n);
    for (i = 0; i < old_irn_arity; i++)
      if (!is_Bad(get_irn_n(n, i))) new_irn_arity++;

    /* arity changing: set new predecessors without bad nodes */
    if (new_irn_arity < old_irn_arity) {
      /* Get new predecessor array. We do not resize the array, as we must
         keep the old one to update Phis. */
      new_in = NEW_ARR_D (ir_node *, current_ir_graph->obst, (new_irn_arity+1));

      /* set new predecessors in array */
      new_in[0] = NULL;
      new_irn_n = 1;
      for (i = 0; i < old_irn_arity; i++) {
        irn = get_irn_n(n, i);
        if (!is_Bad(irn)) {
          new_in[new_irn_n] = irn;
          is_backedge(n, i) ? set_backedge(n, new_irn_n-1) : set_not_backedge(n, new_irn_n-1);
          ++new_irn_n;
        }
      }
      //ARR_SETLEN(int, n->attr.block.backedge, new_irn_arity);
      ARR_SHRINKLEN(n->attr.block.backedge, new_irn_arity);
      n->in = new_in;

    } /* ir node has bad predecessors */

  } /* Block is not relinked */
}

/**
 * Relinks Bad predecessors from Blocks and Phis called by walker
 * remove_bad_predecesors(). If n is a Block, call
 * relink_bad_block_redecessors(). If n is a Phi-node, call also the relinking
 * function of Phi's Block. If this block has bad predecessors, relink preds
 * of the Phi-node.
 */
static void relink_bad_predecessors(ir_node *n, void *env) {
  ir_node *block, **old_in;
  int i, old_irn_arity, new_irn_arity;

  /* relink bad predecessors of a block */
  if (get_irn_op(n) == op_Block)
    relink_bad_block_predecessors(n, env);

  /* If Phi node relink its block and its predecessors */
  if (get_irn_op(n) == op_Phi) {

    /* Relink predecessors of phi's block */
    block = get_nodes_block(n);
    if (get_irn_link(block) == NULL)
      relink_bad_block_predecessors(block, env);

    old_in = (ir_node **)get_irn_link(block); /* Of Phi's Block */
    old_irn_arity = ARR_LEN(old_in);

    /* Relink Phi predecessors if count of predecessors changed */
    if (old_irn_arity != ARR_LEN(get_irn_in(block))) {
      /* set new predecessors in array
         n->in[0] remains the same block */
      new_irn_arity = 1;
      for(i = 1; i < old_irn_arity; i++)
        if (!is_Bad((ir_node *)old_in[i])) {
          n->in[new_irn_arity] = n->in[i];
          is_backedge(n, i) ? set_backedge(n, new_irn_arity) : set_not_backedge(n, new_irn_arity);
          ++new_irn_arity;
        }

      ARR_SETLEN(ir_node *, n->in, new_irn_arity);
      ARR_SETLEN(int, n->attr.phi_backedge, new_irn_arity);
    }

  } /* n is a Phi node */
}

/*
 * Removes Bad Bad predecessors from Blocks and the corresponding
 * inputs to Phi nodes as in dead_node_elimination but without
 * copying the graph.
 * On walking up set the link field to NULL, on walking down call
 * relink_bad_predecessors() (This function stores the old in array
 * to the link field and sets a new in array if arity of predecessors
 * changes).
 */
void remove_bad_predecessors(ir_graph *irg) {
  irg_walk_graph(irg, firm_clear_link, relink_bad_predecessors, NULL);
}


/*
   __                      _  __ __
  (_     __    o     _    | \/  |_
  __)|_| | \_/ | \_/(/_   |_/\__|__

  The following stuff implements a facility that automatically patches
  registered ir_node pointers to the new node when a dead node elimination occurs.
*/

struct _survive_dce_t {
  struct obstack obst;
  pmap *places;
  pmap *new_places;
  hook_entry_t dead_node_elim;
  hook_entry_t dead_node_elim_subst;
};

typedef struct _survive_dce_list_t {
  struct _survive_dce_list_t *next;
  ir_node **place;
} survive_dce_list_t;

static void dead_node_hook(void *context, ir_graph *irg, int start)
{
  survive_dce_t *sd = context;

  /* Create a new map before the dead node elimination is performed. */
  if (start) {
    sd->new_places = pmap_create_ex(pmap_count(sd->places));
  }

  /* Patch back all nodes if dead node elimination is over and something is to be done. */
  else {
    pmap_destroy(sd->places);
    sd->places     = sd->new_places;
    sd->new_places = NULL;
  }
}

/**
 * Hook called when dead node elimination replaces old by nw.
 */
static void dead_node_subst_hook(void *context, ir_graph *irg, ir_node *old, ir_node *nw)
{
  survive_dce_t *sd = context;
  survive_dce_list_t *list = pmap_get(sd->places, old);

  /* If the node is to be patched back, write the new address to all registered locations. */
  if (list) {
    survive_dce_list_t *p;

    for(p = list; p; p = p->next)
      *(p->place) = nw;

    pmap_insert(sd->new_places, nw, list);
  }
}

/**
 * Make a new Survive DCE environment.
 */
survive_dce_t *new_survive_dce(void)
{
  survive_dce_t *res = xmalloc(sizeof(res[0]));
  obstack_init(&res->obst);
  res->places     = pmap_create();
  res->new_places = NULL;

  res->dead_node_elim.hook._hook_dead_node_elim = dead_node_hook;
  res->dead_node_elim.context                   = res;
  res->dead_node_elim.next                      = NULL;

  res->dead_node_elim_subst.hook._hook_dead_node_elim_subst = dead_node_subst_hook;
  res->dead_node_elim_subst.context = res;
  res->dead_node_elim_subst.next    = NULL;

  register_hook(hook_dead_node_elim, &res->dead_node_elim);
  register_hook(hook_dead_node_elim_subst, &res->dead_node_elim_subst);
  return res;
}

/**
 * Free a Survive DCE environment.
 */
void free_survive_dce(survive_dce_t *sd)
{
  obstack_free(&sd->obst, NULL);
  pmap_destroy(sd->places);
  unregister_hook(hook_dead_node_elim, &sd->dead_node_elim);
  unregister_hook(hook_dead_node_elim_subst, &sd->dead_node_elim_subst);
  free(sd);
}

/**
 * Register a node pointer to be patched upon DCE.
 * When DCE occurs, the node pointer specified by @p place will be
 * patched to the new address of the node it is pointing to.
 *
 * @param sd    The Survive DCE environment.
 * @param place The address of the node pointer.
 */
void survive_dce_register_irn(survive_dce_t *sd, ir_node **place)
{
  if(*place != NULL) {
    ir_node *irn      = *place;
    survive_dce_list_t *curr = pmap_get(sd->places, irn);
    survive_dce_list_t *nw   = obstack_alloc(&sd->obst, sizeof(nw));

    nw->next  = curr;
    nw->place = place;

    pmap_insert(sd->places, irn, nw);
  }
}

/*--------------------------------------------------------------------*/
/*  Functionality for inlining                                         */
/*--------------------------------------------------------------------*/

/**
 * Copy node for inlineing.  Updates attributes that change when
 * inlineing but not for dead node elimination.
 *
 * Copies the node by calling copy_node() and then updates the entity if
 * it's a local one.  env must be a pointer of the frame type of the
 * inlined procedure. The new entities must be in the link field of
 * the entities.
 */
static INLINE void
copy_node_inline (ir_node *n, void *env) {
  ir_node *nn;
  ir_type *frame_tp = (ir_type *)env;

  copy_node(n, NULL);
  if (get_irn_op(n) == op_Sel) {
    nn = get_new_node (n);
    assert(is_Sel(nn));
    if (get_entity_owner(get_Sel_entity(n)) == frame_tp) {
      set_Sel_entity(nn, get_entity_link(get_Sel_entity(n)));
    }
  } else if (get_irn_op(n) == op_Block) {
    nn = get_new_node (n);
    nn->attr.block.irg = current_ir_graph;
  }
}

/**
 * Walker: checks if P_value_arg_base is used.
 */
static void find_addr(ir_node *node, void *env) {
  int *allow_inline = env;
  if (is_Proj(node) && get_irn_op(get_Proj_pred(node)) == op_Start) {
    if (get_Proj_proj(node) == pn_Start_P_value_arg_base)
      *allow_inline = 0;
  }
}

/*
 * currently, we cannot inline two cases:
 * - call with compound arguments
 * - graphs that take the address of a parameter
 *
 * check these conditions here
 */
static int can_inline(ir_node *call, ir_graph *called_graph)
{
  ir_type *call_type = get_Call_type(call);
  int params, ress, i, res;
  assert(is_Method_type(call_type));

  params = get_method_n_params(call_type);
  ress   = get_method_n_ress(call_type);

  /* check parameters for compound arguments */
  for (i = 0; i < params; ++i) {
    ir_type *p_type = get_method_param_type(call_type, i);

    if (is_compound_type(p_type))
      return 0;
  }

  /* check results for compound arguments */
  for (i = 0; i < ress; ++i) {
    ir_type *r_type = get_method_res_type(call_type, i);

    if (is_compound_type(r_type))
      return 0;
  }

  res = 1;
  irg_walk_graph(called_graph, find_addr, NULL, &res);

  return res;
}

/* Inlines a method at the given call site. */
int inline_method(ir_node *call, ir_graph *called_graph) {
  ir_node *pre_call;
  ir_node *post_call, *post_bl;
  ir_node *in[pn_Start_max];
  ir_node *end, *end_bl;
  ir_node **res_pred;
  ir_node **cf_pred;
  ir_node *ret, *phi;
  int arity, n_ret, n_exc, n_res, i, j, rem_opt, irn_arity;
  int exc_handling;
  ir_type *called_frame;
  irg_inline_property prop = get_irg_inline_property(called_graph);

  if ( (prop < irg_inline_forced) &&
       (!get_opt_optimize() || !get_opt_inline() || (prop == irg_inline_forbidden))) return 0;

  /* Do not inline variadic functions. */
  if (get_method_variadicity(get_entity_type(get_irg_entity(called_graph))) == variadicity_variadic)
    return 0;

  assert(get_method_n_params(get_entity_type(get_irg_entity(called_graph))) ==
         get_method_n_params(get_Call_type(call)));

  /*
   * currently, we cannot inline two cases:
   * - call with compound arguments
   * - graphs that take the address of a parameter
   */
  if (! can_inline(call, called_graph))
    return 0;

  /* --  Turn off optimizations, this can cause problems when allocating new nodes. -- */
  rem_opt = get_opt_optimize();
  set_optimize(0);

  /* Handle graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  assert(get_irg_pinned(current_ir_graph) == op_pin_state_pinned);
  assert(get_irg_pinned(called_graph) == op_pin_state_pinned);
  set_irg_outs_inconsistent(current_ir_graph);
  set_irg_extblk_inconsistent(current_ir_graph);
  set_irg_doms_inconsistent(current_ir_graph);
  set_irg_loopinfo_inconsistent(current_ir_graph);
  set_irg_callee_info_state(current_ir_graph, irg_callee_info_inconsistent);

  /* -- Check preconditions -- */
  assert(is_Call(call));
  /* @@@ does not work for InterfaceIII.java after cgana
     assert(get_Call_type(call) == get_entity_type(get_irg_entity(called_graph)));
     assert(smaller_type(get_entity_type(get_irg_entity(called_graph)),
     get_Call_type(call)));
  */
  if (called_graph == current_ir_graph) {
    set_optimize(rem_opt);
    return 0;
  }

  /* here we know we WILL inline, so inform the statistics */
  hook_inline(call, called_graph);

  /* -- Decide how to handle exception control flow: Is there a handler
     for the Call node, or do we branch directly to End on an exception?
     exc_handling:
     0 There is a handler.
     1 Branches to End.
     2 Exception handling not represented in Firm. -- */
  {
    ir_node *proj, *Mproj = NULL, *Xproj = NULL;
    for (proj = get_irn_link(call); proj; proj = get_irn_link(proj)) {
      assert(is_Proj(proj));
      if (get_Proj_proj(proj) == pn_Call_X_except) Xproj = proj;
      if (get_Proj_proj(proj) == pn_Call_M_except) Mproj = proj;
    }
    if      (Mproj) { assert(Xproj); exc_handling = 0; } /*  Mproj           */
    else if (Xproj) {                exc_handling = 1; } /* !Mproj &&  Xproj   */
    else            {                exc_handling = 2; } /* !Mproj && !Xproj   */
  }


  /* --
     the procedure and later replaces the Start node of the called graph.
     Post_call is the old Call node and collects the results of the called
     graph. Both will end up being a tuple.  -- */
  post_bl = get_nodes_block(call);
  set_irg_current_block(current_ir_graph, post_bl);
  /* XxMxPxPxPxT of Start + parameter of Call */
  in[pn_Start_X_initial_exec]   = new_Jmp();
  in[pn_Start_M]                = get_Call_mem(call);
  in[pn_Start_P_frame_base]     = get_irg_frame(current_ir_graph);
  in[pn_Start_P_globals]        = get_irg_globals(current_ir_graph);
  in[pn_Start_P_tls]            = get_irg_tls(current_ir_graph);
  in[pn_Start_T_args]           = new_Tuple(get_Call_n_params(call), get_Call_param_arr(call));
  /* in[pn_Start_P_value_arg_base] = ??? */
  assert(pn_Start_P_value_arg_base == pn_Start_max - 1 && "pn_Start_P_value_arg_base not supported, fix");
  pre_call = new_Tuple(pn_Start_max - 1, in);
  post_call = call;

  /* --
     The new block gets the ins of the old block, pre_call and all its
     predecessors and all Phi nodes. -- */
  part_block(pre_call);

  /* -- Prepare state for dead node elimination -- */
  /* Visited flags in calling irg must be >= flag in called irg.
     Else walker and arity computation will not work. */
  if (get_irg_visited(current_ir_graph) <= get_irg_visited(called_graph))
    set_irg_visited(current_ir_graph, get_irg_visited(called_graph)+1);
  if (get_irg_block_visited(current_ir_graph)< get_irg_block_visited(called_graph))
    set_irg_block_visited(current_ir_graph, get_irg_block_visited(called_graph));
  /* Set pre_call as new Start node in link field of the start node of
     calling graph and pre_calls block as new block for the start block
     of calling graph.
     Further mark these nodes so that they are not visited by the
     copying. */
  set_irn_link(get_irg_start(called_graph), pre_call);
  set_irn_visited(get_irg_start(called_graph), get_irg_visited(current_ir_graph));
  set_irn_link(get_irg_start_block(called_graph), get_nodes_block(pre_call));
  set_irn_visited(get_irg_start_block(called_graph), get_irg_visited(current_ir_graph));
  set_irn_link(get_irg_bad(called_graph), get_irg_bad(current_ir_graph));
  set_irn_visited(get_irg_bad(called_graph), get_irg_visited(current_ir_graph));

  /* Initialize for compaction of in arrays */
  inc_irg_block_visited(current_ir_graph);

  /* -- Replicate local entities of the called_graph -- */
  /* copy the entities. */
  called_frame = get_irg_frame_type(called_graph);
  for (i = 0; i < get_class_n_members(called_frame); i++) {
    entity *new_ent, *old_ent;
    old_ent = get_class_member(called_frame, i);
    new_ent = copy_entity_own(old_ent, get_cur_frame_type());
    set_entity_link(old_ent, new_ent);
  }

  /* visited is > than that of called graph.  With this trick visited will
     remain unchanged so that an outer walker, e.g., searching the call nodes
     to inline, calling this inline will not visit the inlined nodes. */
  set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph)-1);

  /* -- Performing dead node elimination inlines the graph -- */
  /* Copies the nodes to the obstack of current_ir_graph. Updates links to new
     entities. */
  /* @@@ endless loops are not copied!! -- they should be, I think... */
  irg_walk(get_irg_end(called_graph), copy_node_inline, copy_preds,
           get_irg_frame_type(called_graph));

  /* Repair called_graph */
  set_irg_visited(called_graph, get_irg_visited(current_ir_graph));
  set_irg_block_visited(called_graph, get_irg_block_visited(current_ir_graph));
  set_Block_block_visited(get_irg_start_block(called_graph), 0);

  /* -- Merge the end of the inlined procedure with the call site -- */
  /* We will turn the old Call node into a Tuple with the following
     predecessors:
     -1:  Block of Tuple.
     0: Phi of all Memories of Return statements.
     1: Jmp from new Block that merges the control flow from all exception
     predecessors of the old end block.
     2: Tuple of all arguments.
     3: Phi of Exception memories.
     In case the old Call directly branches to End on an exception we don't
     need the block merging all exceptions nor the Phi of the exception
     memories.
  */

  /* -- Precompute some values -- */
  end_bl = get_new_node(get_irg_end_block(called_graph));
  end = get_new_node(get_irg_end(called_graph));
  arity = get_irn_arity(end_bl);    /* arity = n_exc + n_ret  */
  n_res = get_method_n_ress(get_Call_type(call));

  res_pred = xmalloc (n_res * sizeof(*res_pred));
  cf_pred  = xmalloc (arity * sizeof(*res_pred));

  set_irg_current_block(current_ir_graph, post_bl); /* just to make sure */

  /* -- archive keepalives -- */
  irn_arity = get_irn_arity(end);
  for (i = 0; i < irn_arity; i++)
    add_End_keepalive(get_irg_end(current_ir_graph), get_irn_n(end, i));

  /* The new end node will die.  We need not free as the in array is on the obstack:
     copy_node() only generated 'D' arrays. */

  /* -- Replace Return nodes by Jump nodes. -- */
  n_ret = 0;
  for (i = 0; i < arity; i++) {
    ir_node *ret;
    ret = get_irn_n(end_bl, i);
    if (is_Return(ret)) {
      cf_pred[n_ret] = new_r_Jmp(current_ir_graph, get_nodes_block(ret));
      n_ret++;
    }
  }
  set_irn_in(post_bl, n_ret, cf_pred);

  /* -- Build a Tuple for all results of the method.
     Add Phi node if there was more than one Return.  -- */
  turn_into_tuple(post_call, 4);
  /* First the Memory-Phi */
  n_ret = 0;
  for (i = 0; i < arity; i++) {
    ret = get_irn_n(end_bl, i);
    if (is_Return(ret)) {
      cf_pred[n_ret] = get_Return_mem(ret);
      n_ret++;
    }
  }
  phi = new_Phi(n_ret, cf_pred, mode_M);
  set_Tuple_pred(call, pn_Call_M_regular, phi);
  /* Conserve Phi-list for further inlinings -- but might be optimized */
  if (get_nodes_block(phi) == post_bl) {
    set_irn_link(phi, get_irn_link(post_bl));
    set_irn_link(post_bl, phi);
  }
  /* Now the real results */
  if (n_res > 0) {
    for (j = 0; j < n_res; j++) {
      n_ret = 0;
      for (i = 0; i < arity; i++) {
        ret = get_irn_n(end_bl, i);
        if (get_irn_op(ret) == op_Return) {
          cf_pred[n_ret] = get_Return_res(ret, j);
          n_ret++;
        }
      }
      if (n_ret > 0)
        phi = new_Phi(n_ret, cf_pred, get_irn_mode(cf_pred[0]));
      else
        phi = new_Bad();
      res_pred[j] = phi;
      /* Conserve Phi-list for further inlinings -- but might be optimized */
      if (get_nodes_block(phi) == post_bl) {
        set_irn_link(phi, get_irn_link(post_bl));
        set_irn_link(post_bl, phi);
      }
    }
    set_Tuple_pred(call, pn_Call_T_result, new_Tuple(n_res, res_pred));
  } else {
    set_Tuple_pred(call, pn_Call_T_result, new_Bad());
  }
  /* Finally the exception control flow.
     We have two (three) possible situations:
     First if the Call branches to an exception handler: We need to add a Phi node to
     collect the memory containing the exception objects.  Further we need
     to add another block to get a correct representation of this Phi.  To
     this block we add a Jmp that resolves into the X output of the Call
     when the Call is turned into a tuple.
     Second the Call branches to End, the exception is not handled.  Just
     add all inlined exception branches to the End node.
     Third: there is no Exception edge at all. Handle as case two. */
  if (exc_handling == 0) {
    n_exc = 0;
    for (i = 0; i < arity; i++) {
      ir_node *ret;
      ret = get_irn_n(end_bl, i);
      if (is_fragile_op(skip_Proj(ret)) || (get_irn_op(skip_Proj(ret)) == op_Raise)) {
        cf_pred[n_exc] = ret;
        n_exc++;
      }
    }
    if (n_exc > 0) {
      new_Block(n_exc, cf_pred);      /* watch it: current_block is changed! */
      set_Tuple_pred(call, pn_Call_X_except, new_Jmp());
      /* The Phi for the memories with the exception objects */
      n_exc = 0;
      for (i = 0; i < arity; i++) {
        ir_node *ret;
        ret = skip_Proj(get_irn_n(end_bl, i));
        if (is_Call(ret)) {
          cf_pred[n_exc] = new_r_Proj(current_ir_graph, get_nodes_block(ret), ret, mode_M, 3);
          n_exc++;
        } else if (is_fragile_op(ret)) {
          /* We rely that all cfops have the memory output at the same position. */
          cf_pred[n_exc] = new_r_Proj(current_ir_graph, get_nodes_block(ret), ret, mode_M, 0);
          n_exc++;
        } else if (get_irn_op(ret) == op_Raise) {
          cf_pred[n_exc] = new_r_Proj(current_ir_graph, get_nodes_block(ret), ret, mode_M, 1);
          n_exc++;
        }
      }
      set_Tuple_pred(call, pn_Call_M_except, new_Phi(n_exc, cf_pred, mode_M));
    } else {
      set_Tuple_pred(call, pn_Call_X_except, new_Bad());
      set_Tuple_pred(call, pn_Call_M_except, new_Bad());
    }
  } else {
    ir_node *main_end_bl;
    int main_end_bl_arity;
    ir_node **end_preds;

    /* assert(exc_handling == 1 || no exceptions. ) */
    n_exc = 0;
    for (i = 0; i < arity; i++) {
      ir_node *ret = get_irn_n(end_bl, i);

      if (is_fragile_op(skip_Proj(ret)) || (get_irn_op(skip_Proj(ret)) == op_Raise)) {
        cf_pred[n_exc] = ret;
        n_exc++;
      }
    }
    main_end_bl = get_irg_end_block(current_ir_graph);
    main_end_bl_arity = get_irn_arity(main_end_bl);
    end_preds =  xmalloc ((n_exc + main_end_bl_arity) * sizeof(*end_preds));

    for (i = 0; i < main_end_bl_arity; ++i)
      end_preds[i] = get_irn_n(main_end_bl, i);
    for (i = 0; i < n_exc; ++i)
      end_preds[main_end_bl_arity + i] = cf_pred[i];
    set_irn_in(main_end_bl, n_exc + main_end_bl_arity, end_preds);
    set_Tuple_pred(call, pn_Call_X_except, new_Bad());
    set_Tuple_pred(call, pn_Call_M_except, new_Bad());
    free(end_preds);
  }
  free(res_pred);
  free(cf_pred);

  /* --  Turn CSE back on. -- */
  set_optimize(rem_opt);

  return 1;
}

/********************************************************************/
/* Apply inlineing to small methods.                                */
/********************************************************************/

/** Represents a possible inlinable call in a graph. */
typedef struct _call_entry call_entry;
struct _call_entry {
  ir_node    *call;   /**< the Call */
  ir_graph   *callee; /**< the callee called here */
  call_entry *next;   /**< for linking the next one */
};

/**
 * environment for inlining small irgs
 */
typedef struct _inline_env_t {
  struct obstack obst;  /**< an obstack where call_entries are allocated on. */
  call_entry *head;     /**< the head of the call entry list */
  call_entry *tail;     /**< the tail of the call entry list */
} inline_env_t;

/**
 * Returns the irg called from a Call node. If the irg is not
 * known, NULL is returned.
 */
static ir_graph *get_call_called_irg(ir_node *call) {
  ir_node *addr;
  ir_graph *called_irg = NULL;

  addr = get_Call_ptr(call);
  if (is_SymConst(addr) && get_SymConst_kind(addr) == symconst_addr_ent) {
    called_irg = get_entity_irg(get_SymConst_entity(addr));
  }

  return called_irg;
}

/**
 * Walker: Collect all calls to known graphs inside a graph.
 */
static void collect_calls(ir_node *call, void *env) {
  if (is_Call(call)) {
    ir_graph *called_irg = get_call_called_irg(call);
    if (called_irg) {
      /* The Call node calls a locally defined method.  Remember to inline. */
      inline_env_t *ienv  = env;
      call_entry   *entry = obstack_alloc(&ienv->obst, sizeof(*entry));
      entry->call   = call;
      entry->callee = called_irg;
      entry->next   = NULL;

      if (ienv->tail == NULL)
        ienv->head = entry;
      else
        ienv->tail->next = entry;
      ienv->tail = entry;
    }
  }
}

/**
 * Inlines all small methods at call sites where the called address comes
 * from a Const node that references the entity representing the called
 * method.
 * The size argument is a rough measure for the code size of the method:
 * Methods where the obstack containing the firm graph is smaller than
 * size are inlined.
 */
void inline_small_irgs(ir_graph *irg, int size) {
  ir_graph *rem = current_ir_graph;
  inline_env_t env;
  call_entry *entry;
  DEBUG_ONLY(firm_dbg_module_t *dbg;)

  if (!(get_opt_optimize() && get_opt_inline())) return;

  FIRM_DBG_REGISTER(dbg, "firm.opt.inline");

  current_ir_graph = irg;
  /* Handle graph state */
  assert(get_irg_phase_state(irg) != phase_building);
  free_callee_info(irg);

  /* Find Call nodes to inline.
     (We can not inline during a walk of the graph, as inlineing the same
     method several times changes the visited flag of the walked graph:
     after the first inlineing visited of the callee equals visited of
     the caller.  With the next inlineing both are increased.) */
  obstack_init(&env.obst);
  env.head = env.tail = NULL;
  irg_walk_graph(irg, NULL, collect_calls, &env);

  if (env.head != NULL) {
    /* There are calls to inline */
    collect_phiprojs(irg);
    for (entry = env.head; entry != NULL; entry = entry->next) {
      ir_graph *callee = entry->callee;
      if (((_obstack_memory_used(callee->obst) - (int)obstack_room(callee->obst)) < size) ||
          (get_irg_inline_property(callee) >= irg_inline_forced)) {
        inline_method(entry->call, callee);
      }
    }
  }
  obstack_free(&env.obst, NULL);
  current_ir_graph = rem;
}

/**
 * Environment for inlining irgs.
 */
typedef struct {
  int n_nodes;             /**< Number of nodes in graph except Id, Tuple, Proj, Start, End. */
  int n_nodes_orig;        /**< for statistics */
  call_entry *call_head;   /**< The head of the list of all call nodes in this graph. */
  call_entry *call_tail;   /**< The tail of the list of all call nodes in this graph .*/
  int n_call_nodes;        /**< Number of Call nodes in the graph. */
  int n_call_nodes_orig;   /**< for statistics */
  int n_callers;           /**< Number of known graphs that call this graphs. */
  int n_callers_orig;      /**< for statistics */
  int got_inline;          /**< Set, if at leat one call inside this graph was inlined. */
} inline_irg_env;

/**
 * Allocate a new environment for inlining.
 */
static inline_irg_env *alloc_inline_irg_env(struct obstack *obst) {
  inline_irg_env *env    = obstack_alloc(obst, sizeof(*env));
  env->n_nodes           = -2; /* do not count count Start, End */
  env->n_nodes_orig      = -2; /* do not count Start, End */
  env->call_head         = NULL;
  env->call_tail         = NULL;
  env->n_call_nodes      = 0;
  env->n_call_nodes_orig = 0;
  env->n_callers         = 0;
  env->n_callers_orig    = 0;
  env->got_inline        = 0;
  return env;
}

typedef struct walker_env {
  struct obstack *obst; /**< the obstack for allocations. */
  inline_irg_env *x;    /**< the inline environment */
  int ignore_runtime;   /**< the ignore runtime flag */
} wenv_t;

/**
 * post-walker: collect all calls in the inline-environment
 * of a graph and sum some statistics.
 */
static void collect_calls2(ir_node *call, void *ctx) {
  wenv_t         *env = ctx;
  inline_irg_env *x = env->x;
  ir_op          *op = get_irn_op(call);
  ir_graph       *callee;
  call_entry     *entry;

  /* count meaningful nodes in irg */
  if (op != op_Proj && op != op_Tuple && op != op_Sync) {
    ++x->n_nodes;
    ++x->n_nodes_orig;
  }

  if (op != op_Call) return;

  /* check, if it's a runtime call */
  if (env->ignore_runtime) {
    ir_node *symc = get_Call_ptr(call);

    if (is_SymConst(symc) && get_SymConst_kind(symc) == symconst_addr_ent) {
      entity *ent = get_SymConst_entity(symc);

      if (get_entity_additional_properties(ent) & mtp_property_runtime)
        return;
    }
  }

  /* collect all call nodes */
  ++x->n_call_nodes;
  ++x->n_call_nodes_orig;

  callee = get_call_called_irg(call);
  if (callee) {
    inline_irg_env *callee_env = get_irg_link(callee);
    /* count all static callers */
    ++callee_env->n_callers;
    ++callee_env->n_callers_orig;

    /* link it in the list of possible inlinable entries */
    entry = obstack_alloc(env->obst, sizeof(*entry));
    entry->call   = call;
    entry->callee = callee;
    entry->next   = NULL;
    if (x->call_tail == NULL)
      x->call_head = entry;
    else
      x->call_tail->next = entry;
    x->call_tail = entry;
  }
}

/**
 * Returns TRUE if the number of callers in 0 in the irg's environment,
 * hence this irg is a leave.
 */
INLINE static int is_leave(ir_graph *irg) {
  inline_irg_env *env = get_irg_link(irg);
  return env->n_call_nodes == 0;
}

/**
 * Returns TRUE if the number of callers is smaller size in the irg's environment.
 */
INLINE static int is_smaller(ir_graph *callee, int size) {
  inline_irg_env *env = get_irg_link(callee);
  return env->n_nodes < size;
}

/**
 * Append the nodes of the list src to the nodes of the list in environment dst.
 */
static void append_call_list(struct obstack *obst, inline_irg_env *dst, call_entry *src) {
  call_entry *entry, *nentry;

  /* Note that the src list points to Call nodes in the inlined graph, but
     we need Call nodes in our graph. Luckily the inliner leaves this information
     in the link field. */
  for (entry = src; entry != NULL; entry = entry->next) {
    nentry = obstack_alloc(obst, sizeof(*nentry));
    nentry->call   = get_irn_link(entry->call);
    nentry->callee = entry->callee;
    nentry->next   = NULL;
    dst->call_tail->next = nentry;
    dst->call_tail       = nentry;
  }
}

/*
 * Inlines small leave methods at call sites where the called address comes
 * from a Const node that references the entity representing the called
 * method.
 * The size argument is a rough measure for the code size of the method:
 * Methods where the obstack containing the firm graph is smaller than
 * size are inlined.
 */
void inline_leave_functions(int maxsize, int leavesize, int size, int ignore_runtime) {
  inline_irg_env   *env;
  ir_graph         *irg;
  int              i, n_irgs;
  ir_graph         *rem;
  int              did_inline;
  wenv_t           wenv;
  call_entry       *entry, *tail;
  const call_entry *centry;
  struct obstack   obst;
  DEBUG_ONLY(firm_dbg_module_t *dbg;)

  if (!(get_opt_optimize() && get_opt_inline())) return;

  FIRM_DBG_REGISTER(dbg, "firm.opt.inline");
  rem = current_ir_graph;
  obstack_init(&obst);

  /* extend all irgs by a temporary data structure for inlining. */
  n_irgs = get_irp_n_irgs();
  for (i = 0; i < n_irgs; ++i)
    set_irg_link(get_irp_irg(i), alloc_inline_irg_env(&obst));

  /* Precompute information in temporary data structure. */
  wenv.obst           = &obst;
  wenv.ignore_runtime = ignore_runtime;
  for (i = 0; i < n_irgs; ++i) {
    ir_graph *irg = get_irp_irg(i);

    assert(get_irg_phase_state(irg) != phase_building);
    free_callee_info(irg);

    wenv.x = get_irg_link(irg);
    irg_walk_graph(irg, NULL, collect_calls2, &wenv);
  }

  /* -- and now inline. -- */

  /* Inline leaves recursively -- we might construct new leaves. */
  do {
    did_inline = 0;

    for (i = 0; i < n_irgs; ++i) {
      ir_node *call;
      int phiproj_computed = 0;

      current_ir_graph = get_irp_irg(i);
      env = (inline_irg_env *)get_irg_link(current_ir_graph);

      tail = NULL;
      for (entry = env->call_head; entry != NULL; entry = entry->next) {
        ir_graph *callee;

        if (env->n_nodes > maxsize) break;

        call   = entry->call;
        callee = entry->callee;

        if (is_leave(callee) && is_smaller(callee, leavesize)) {
          if (!phiproj_computed) {
            phiproj_computed = 1;
            collect_phiprojs(current_ir_graph);
          }
          did_inline = inline_method(call, callee);

          if (did_inline) {
            /* Do some statistics */
            inline_irg_env *callee_env = (inline_irg_env *)get_irg_link(callee);

            env->got_inline = 1;
            --env->n_call_nodes;
            env->n_nodes += callee_env->n_nodes;
            --callee_env->n_callers;

            /* remove this call from the list */
            if (tail != NULL)
              tail->next = entry->next;
            else
              env->call_head = entry->next;
            continue;
          }
        }
        tail = entry;
      }
      env->call_tail = tail;
    }
  } while (did_inline);

  /* inline other small functions. */
  for (i = 0; i < n_irgs; ++i) {
    ir_node *call;
    int phiproj_computed = 0;

    current_ir_graph = get_irp_irg(i);
    env = (inline_irg_env *)get_irg_link(current_ir_graph);

    /* note that the list of possible calls is updated during the process */
    tail = NULL;
    for (entry = env->call_head; entry != NULL; entry = entry->next) {
      ir_graph *callee;

      call   = entry->call;
      callee = entry->callee;

      if (((is_smaller(callee, size) && (env->n_nodes < maxsize)) ||    /* small function */
           (get_irg_inline_property(callee) >= irg_inline_forced))) {
        if (!phiproj_computed) {
            phiproj_computed = 1;
            collect_phiprojs(current_ir_graph);
        }
        if (inline_method(call, callee)) {
          inline_irg_env *callee_env = (inline_irg_env *)get_irg_link(callee);

          /* callee was inline. Append it's call list. */
          env->got_inline = 1;
          --env->n_call_nodes;
          append_call_list(&obst, env, callee_env->call_head);
          env->n_call_nodes += callee_env->n_call_nodes;
          env->n_nodes += callee_env->n_nodes;
          --callee_env->n_callers;

          /* after we have inlined callee, all called methods inside callee
             are now called once more */
          for (centry = callee_env->call_head; centry != NULL; centry = centry->next) {
            inline_irg_env *penv = get_irg_link(centry->callee);
            ++penv->n_callers;
          }

          /* remove this call from the list */
          if (tail != NULL)
            tail->next = entry->next;
          else
            env->call_head = entry->next;
          continue;
        }
      }
      tail = entry;
    }
    env->call_tail = tail;
  }

  for (i = 0; i < n_irgs; ++i) {
    irg = get_irp_irg(i);
    env = (inline_irg_env *)get_irg_link(irg);

    if (env->got_inline) {
      /* this irg got calls inlined */
      set_irg_outs_inconsistent(irg);
      set_irg_doms_inconsistent(irg);

      optimize_graph_df(irg);
      optimize_cf(irg);
    }
    if (env->got_inline || (env->n_callers_orig != env->n_callers))
      DB((dbg, SET_LEVEL_1, "Nodes:%3d ->%3d, calls:%3d ->%3d, callers:%3d ->%3d, -- %s\n",
             env->n_nodes_orig, env->n_nodes, env->n_call_nodes_orig, env->n_call_nodes,
             env->n_callers_orig, env->n_callers,
             get_entity_name(get_irg_entity(irg))));
  }

  obstack_free(&obst, NULL);
  current_ir_graph = rem;
}

/*******************************************************************/
/*  Code Placement.  Pins all floating nodes to a block where they */
/*  will be executed only if needed.                               */
/*******************************************************************/

/**
 * Returns non-zero, is a block is not reachable from Start.
 *
 * @param block  the block to test
 */
static int
is_Block_unreachable(ir_node *block) {
  return is_Block_dead(block) || get_Block_dom_depth(block) < 0;
}

/**
 * Find the earliest correct block for N.  --- Place N into the
 * same Block as its dominance-deepest Input.
 *
 * We have to avoid calls to get_nodes_block() here
 * because the graph is floating.
 *
 * move_out_of_loops() expects that place_floats_early() have placed
 * all "living" nodes into a living block. That's why we must
 * move nodes in dead block with "live" successors into a valid
 * block.
 * We move them just into the same block as it's successor (or
 * in case of a Phi into the effective use block). For Phi successors,
 * this may still be a dead block, but then there is no real use, as
 * the control flow will be dead later.
 */
static void
place_floats_early(ir_node *n, waitq *worklist)
{
  int i, irn_arity;

  /* we must not run into an infinite loop */
  assert(irn_not_visited(n));
  mark_irn_visited(n);

  /* Place floating nodes. */
  if (get_irn_pinned(n) == op_pin_state_floats) {
    ir_node *curr_block = get_irn_n(n, -1);
    int in_dead_block   = is_Block_unreachable(curr_block);
    int depth           = 0;
    ir_node *b          = NULL;   /* The block to place this node in */

    assert(is_no_Block(n));

    if (is_irn_start_block_placed(n)) {
      /* These nodes will not be placed by the loop below. */
      b = get_irg_start_block(current_ir_graph);
      depth = 1;
    }

    /* find the block for this node. */
    irn_arity = get_irn_arity(n);
    for (i = 0; i < irn_arity; i++) {
      ir_node *pred = get_irn_n(n, i);
      ir_node *pred_block;

      if ((irn_not_visited(pred))
         && (get_irn_pinned(pred) == op_pin_state_floats)) {

        /*
         * If the current node is NOT in a dead block, but one of its
         * predecessors is, we must move the predecessor to a live block.
         * Such thing can happen, if global CSE chose a node from a dead block.
         * We move it simply to our block.
         * Note that neither Phi nor End nodes are floating, so we don't
         * need to handle them here.
         */
        if (! in_dead_block) {
          if (get_irn_pinned(pred) == op_pin_state_floats &&
              is_Block_unreachable(get_irn_n(pred, -1)))
            set_nodes_block(pred, curr_block);
        }
        place_floats_early(pred, worklist);
      }

      /*
       * A node in the Bad block must stay in the bad block,
       * so don't compute a new block for it.
       */
      if (in_dead_block)
        continue;

      /* Because all loops contain at least one op_pin_state_pinned node, now all
         our inputs are either op_pin_state_pinned or place_early() has already
         been finished on them.  We do not have any unfinished inputs!  */
      pred_block = get_irn_n(pred, -1);
      if ((!is_Block_dead(pred_block)) &&
          (get_Block_dom_depth(pred_block) > depth)) {
        b = pred_block;
        depth = get_Block_dom_depth(pred_block);
      }
      /* Avoid that the node is placed in the Start block */
      if ((depth == 1) && (get_Block_dom_depth(get_irn_n(n, -1)) > 1)) {
        b = get_Block_cfg_out(get_irg_start_block(current_ir_graph), 0);
        assert(b != get_irg_start_block(current_ir_graph));
        depth = 2;
      }
    }
    if (b)
      set_nodes_block(n, b);
  }

  /*
   * Add predecessors of non floating nodes and non-floating predecessors
   * of floating nodes to worklist and fix their blocks if the are in dead block.
   */
  irn_arity = get_irn_arity(n);

  if (get_irn_op(n) == op_End) {
    /*
     * Simplest case: End node. Predecessors are keep-alives,
     * no need to move out of dead block.
     */
    for (i = -1; i < irn_arity; ++i) {
      ir_node *pred = get_irn_n(n, i);
      if (irn_not_visited(pred))
        waitq_put(worklist, pred);
    }
  }
  else if (is_Block(n)) {
    /*
     * Blocks: Predecessors are control flow, no need to move
     * them out of dead block.
     */
    for (i = irn_arity - 1; i >= 0; --i) {
      ir_node *pred = get_irn_n(n, i);
      if (irn_not_visited(pred))
        waitq_put(worklist, pred);
    }
  }
  else if (is_Phi(n)) {
    ir_node *pred;
    ir_node *curr_block = get_irn_n(n, -1);
    int in_dead_block   = is_Block_unreachable(curr_block);

    /*
     * Phi nodes: move nodes from dead blocks into the effective use
     * of the Phi-input if the Phi is not in a bad block.
     */
    pred = get_irn_n(n, -1);
    if (irn_not_visited(pred))
      waitq_put(worklist, pred);

    for (i = irn_arity - 1; i >= 0; --i) {
      ir_node *pred = get_irn_n(n, i);

      if (irn_not_visited(pred)) {
        if (! in_dead_block &&
            get_irn_pinned(pred) == op_pin_state_floats &&
            is_Block_unreachable(get_irn_n(pred, -1))) {
          set_nodes_block(pred, get_Block_cfgpred_block(curr_block, i));
        }
        waitq_put(worklist, pred);
      }
    }
  }
  else {
    ir_node *pred;
    ir_node *curr_block = get_irn_n(n, -1);
    int in_dead_block   = is_Block_unreachable(curr_block);

    /*
     * All other nodes: move nodes from dead blocks into the same block.
     */
    pred = get_irn_n(n, -1);
    if (irn_not_visited(pred))
      waitq_put(worklist, pred);

    for (i = irn_arity - 1; i >= 0; --i) {
      ir_node *pred = get_irn_n(n, i);

      if (irn_not_visited(pred)) {
        if (! in_dead_block &&
            get_irn_pinned(pred) == op_pin_state_floats &&
            is_Block_unreachable(get_irn_n(pred, -1))) {
          set_nodes_block(pred, curr_block);
        }
        waitq_put(worklist, pred);
      }
    }
  }
}

/**
 * Floating nodes form subgraphs that begin at nodes as Const, Load,
 * Start, Call and that end at op_pin_state_pinned nodes as Store, Call.  Place_early
 * places all floating nodes reachable from its argument through floating
 * nodes and adds all beginnings at op_pin_state_pinned nodes to the worklist.
 */
static INLINE void place_early(waitq *worklist) {
  assert(worklist);
  inc_irg_visited(current_ir_graph);

  /* this inits the worklist */
  place_floats_early(get_irg_end(current_ir_graph), worklist);

  /* Work the content of the worklist. */
  while (!waitq_empty(worklist)) {
    ir_node *n = waitq_get(worklist);
    if (irn_not_visited(n))
      place_floats_early(n, worklist);
  }

  set_irg_outs_inconsistent(current_ir_graph);
  set_irg_pinned(current_ir_graph, op_pin_state_pinned);
}

/**
 * Compute the deepest common ancestor of block and dca.
 */
static ir_node *calc_dca(ir_node *dca, ir_node *block)
{
  assert(block);

  /* we do not want to place nodes in dead blocks */
  if (is_Block_dead(block))
    return dca;

  /* We found a first legal placement. */
  if (!dca) return block;

  /* Find a placement that is dominates both, dca and block. */
  while (get_Block_dom_depth(block) > get_Block_dom_depth(dca))
    block = get_Block_idom(block);

  while (get_Block_dom_depth(dca) > get_Block_dom_depth(block)) {
    dca = get_Block_idom(dca);
  }

  while (block != dca)
    { block = get_Block_idom(block); dca = get_Block_idom(dca); }

  return dca;
}

/** Deepest common dominance ancestor of DCA and CONSUMER of PRODUCER.
 * I.e., DCA is the block where we might place PRODUCER.
 * A data flow edge points from producer to consumer.
 */
static ir_node *
consumer_dom_dca(ir_node *dca, ir_node *consumer, ir_node *producer)
{
  ir_node *block = NULL;

  /* Compute the latest block into which we can place a node so that it is
     before consumer. */
  if (get_irn_op(consumer) == op_Phi) {
    /* our consumer is a Phi-node, the effective use is in all those
       blocks through which the Phi-node reaches producer */
    int i, irn_arity;
    ir_node *phi_block = get_nodes_block(consumer);
    irn_arity = get_irn_arity(consumer);

    for (i = 0;  i < irn_arity; i++) {
      if (get_irn_n(consumer, i) == producer) {
        ir_node *new_block = get_nodes_block(get_Block_cfgpred(phi_block, i));

        if (! is_Block_unreachable(new_block))
          block = calc_dca(block, new_block);
      }
    }

    if (! block)
      block = get_irn_n(producer, -1);
  }
  else {
    assert(is_no_Block(consumer));
    block = get_nodes_block(consumer);
  }

  /* Compute the deepest common ancestor of block and dca. */
  return calc_dca(dca, block);
}

/* FIXME: the name clashes here with the function from ana/field_temperature.c
 * please rename. */
static INLINE int get_irn_loop_depth(ir_node *n) {
  return get_loop_depth(get_irn_loop(n));
}

/**
 * Move n to a block with less loop depth than it's current block. The
 * new block must be dominated by early.
 *
 * @param n      the node that should be moved
 * @param early  the earliest block we can n move to
 */
static void move_out_of_loops(ir_node *n, ir_node *early)
{
  ir_node *best, *dca;
  assert(n && early);


  /* Find the region deepest in the dominator tree dominating
     dca with the least loop nesting depth, but still dominated
     by our early placement. */
  dca = get_nodes_block(n);

  best = dca;
  while (dca != early) {
    dca = get_Block_idom(dca);
    if (!dca || is_Bad(dca)) break; /* may be Bad if not reachable from Start */
    if (get_irn_loop_depth(dca) < get_irn_loop_depth(best)) {
      best = dca;
    }
  }
  if (best != get_nodes_block(n)) {
    /* debug output
    printf("Moving out of loop: "); DDMN(n);
    printf(" Outermost block: "); DDMN(early);
    printf(" Best block: "); DDMN(best);
    printf(" Innermost block: "); DDMN(get_nodes_block(n));
    */
    set_nodes_block(n, best);
  }
}

/**
 * Find the latest legal block for N and place N into the
 * `optimal' Block between the latest and earliest legal block.
 * The `optimal' block is the dominance-deepest block of those
 * with the least loop-nesting-depth.  This places N out of as many
 * loops as possible and then makes it as control dependent as
 * possible.
 */
static void place_floats_late(ir_node *n, pdeq *worklist)
{
  int i;
  ir_node *early_blk;

  assert(irn_not_visited(n)); /* no multiple placement */

  mark_irn_visited(n);

  /* no need to place block nodes, control nodes are already placed. */
  if ((get_irn_op(n) != op_Block) &&
      (!is_cfop(n)) &&
      (get_irn_mode(n) != mode_X)) {
    /* Remember the early_blk placement of this block to move it
       out of loop no further than the early_blk placement. */
    early_blk = get_irn_n(n, -1);

    /*
     * BEWARE: Here we also get code, that is live, but
     * was in a dead block.  If the node is life, but because
     * of CSE in a dead block, we still might need it.
     */

    /* Assure that our users are all placed, except the Phi-nodes.
       --- Each data flow cycle contains at least one Phi-node.  We
       have to break the `user has to be placed before the
       producer' dependence cycle and the Phi-nodes are the
       place to do so, because we need to base our placement on the
       final region of our users, which is OK with Phi-nodes, as they
       are op_pin_state_pinned, and they never have to be placed after a
       producer of one of their inputs in the same block anyway. */
    for (i = get_irn_n_outs(n) - 1; i >= 0; --i) {
      ir_node *succ = get_irn_out(n, i);
      if (irn_not_visited(succ) && (get_irn_op(succ) != op_Phi))
        place_floats_late(succ, worklist);
    }

    if (! is_Block_dead(early_blk)) {
      /* do only move things that where not dead */

      /* We have to determine the final block of this node... except for
         constants. */
      if ((get_irn_pinned(n) == op_pin_state_floats) &&
          (get_irn_op(n) != op_Const) &&
          (get_irn_op(n) != op_SymConst)) {
        ir_node *dca = NULL;  /* deepest common ancestor in the
                     dominator tree of all nodes'
                     blocks depending on us; our final
                     placement has to dominate DCA. */
        for (i = get_irn_n_outs(n) - 1; i >= 0; --i) {
          ir_node *succ = get_irn_out(n, i);
          ir_node *succ_blk;

          if (get_irn_op(succ) == op_End) {
            /*
             * This consumer is the End node, a keep alive edge.
             * This is not a real consumer, so we ignore it
             */
            continue;
          }

          /* ignore if succ is in dead code */
          succ_blk = get_irn_n(succ, -1);
          if (is_Block_unreachable(succ_blk))
            continue;
          dca = consumer_dom_dca(dca, succ, n);
        }
        if (dca) {
          set_nodes_block(n, dca);
          move_out_of_loops(n, early_blk);
        }
      }
    }
  }

  /* Add predecessors of all non-floating nodes on list. (Those of floating
     nodes are placed already and therefore are marked.)  */
  for (i = 0; i < get_irn_n_outs(n); i++) {
    ir_node *succ = get_irn_out(n, i);
    if (irn_not_visited(get_irn_out(n, i))) {
      pdeq_putr(worklist, succ);
    }
  }
}

static INLINE void place_late(waitq *worklist) {
  assert(worklist);
  inc_irg_visited(current_ir_graph);

  /* This fills the worklist initially. */
  place_floats_late(get_irg_start_block(current_ir_graph), worklist);

  /* And now empty the worklist again... */
  while (!waitq_empty(worklist)) {
    ir_node *n = waitq_get(worklist);
    if (irn_not_visited(n))
      place_floats_late(n, worklist);
  }
}

void place_code(ir_graph *irg) {
  waitq *worklist;
  ir_graph *rem = current_ir_graph;

  current_ir_graph = irg;

  if (!(get_opt_optimize() && get_opt_global_cse())) return;

  /* Handle graph state */
  assert(get_irg_phase_state(irg) != phase_building);
  assure_doms(irg);

  if (1 || get_irg_loopinfo_state(irg) != loopinfo_consistent) {
    free_loop_information(irg);
    construct_backedges(irg);
  }

  /* Place all floating nodes as early as possible. This guarantees
     a legal code placement. */
  worklist = new_waitq();
  place_early(worklist);

  /* place_early() invalidates the outs, place_late needs them. */
  compute_irg_outs(irg);

  /* Now move the nodes down in the dominator tree. This reduces the
     unnecessary executions of the node. */
  place_late(worklist);

  set_irg_outs_inconsistent(current_ir_graph);
  set_irg_loopinfo_inconsistent(current_ir_graph);
  del_waitq(worklist);
  current_ir_graph = rem;
}

/**
 * Called by walker of remove_critical_cf_edges().
 *
 * Place an empty block to an edge between a blocks of multiple
 * predecessors and a block of multiple successors.
 *
 * @param n   IR node
 * @param env Environment of walker. The changed field.
 */
static void walk_critical_cf_edges(ir_node *n, void *env) {
  int arity, i;
  ir_node *pre, *block, *jmp;
  int *changed = env;
  ir_graph *irg = get_irn_irg(n);

  /* Block has multiple predecessors */
  arity = get_irn_arity(n);
  if (arity > 1) {
    if (n == get_irg_end_block(irg))
      return;  /*  No use to add a block here.      */

    for (i = 0; i < arity; ++i) {
	  const ir_op *cfop;

      pre = get_irn_n(n, i);
      cfop = get_irn_op(skip_Proj(pre));
      /* Predecessor has multiple successors. Insert new control flow edge but
         ignore exception edges. */
      if (! is_op_fragile(cfop) && is_op_forking(cfop)) {
        /* set predecessor of new block */
        block = new_r_Block(irg, 1, &pre);
        /* insert new jmp node to new block */
        jmp = new_r_Jmp(irg, block);
        /* set successor of new block */
        set_irn_n(n, i, jmp);
        *changed = 1;
      } /* predecessor has multiple successors */
    } /* for all predecessors */
  } /* n is a multi-entry block */
}

void remove_critical_cf_edges(ir_graph *irg) {
  int changed = 0;

  irg_block_walk_graph(irg, NULL, walk_critical_cf_edges, &changed);
  if (changed) {
    /* control flow changed */
    set_irg_outs_inconsistent(irg);
    set_irg_extblk_inconsistent(irg);
    set_irg_doms_inconsistent(irg);
    set_irg_loopinfo_inconsistent(irg);
  }
}
