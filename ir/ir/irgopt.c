/*
 * Project:     libFIRM
 * File name:   ir/ir/irgopt.c
 * Purpose:     Optimizations for a whole ir graph, i.e., a procedure.
 * Author:      Christian Schaefer, Goetz Lindenmaier
 * Modified by: Sebastian Felis
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <assert.h>
# include <stdbool.h>

# include "irprog.h"
# include "irgopt.h"
# include "irnode_t.h"
# include "irgraph_t.h"
# include "iropt_t.h"
# include "irgwalk.h"
# include "ircons.h"
# include "irgmod.h"
# include "array.h"
# include "pset.h"
# include "eset.h"
# include "pdeq.h"       /* Fuer code placement */
# include "irouts.h"
# include "irloop.h"
# include "irbackedge_t.h"
# include "irflag_t.h"
# include "firmstat.h"

/* Defined in iropt.c */
pset *new_identities (void);
void  del_identities (pset *value_table);
void  add_identities   (pset *value_table, ir_node *node);

/*------------------------------------------------------------------*/
/* apply optimizations of iropt to all nodes.                       */
/*------------------------------------------------------------------*/

static void init_link (ir_node *n, void *env) {
  set_irn_link(n, NULL);
}

#if 0   /* Old version. Avoids Ids.
       This is not necessary:  we do a postwalk, and get_irn_n
       removes ids anyways.  So it's much cheaper to call the
       optimization less often and use the exchange() algorithm. */
static void
optimize_in_place_wrapper (ir_node *n, void *env) {
  int i, irn_arity;
  ir_node *optimized, *old;

  irn_arity = get_irn_arity(n);
  for (i = 0; i < irn_arity; i++) {
    /* get_irn_n skips Id nodes, so comparison old != optimized does not
       show all optimizations. Therefore always set new predecessor. */
    old = get_irn_intra_n(n, i);
    optimized = optimize_in_place_2(old);
    set_irn_n(n, i, optimized);
  }

  if (get_irn_op(n) == op_Block) {
    optimized = optimize_in_place_2(n);
    if (optimized != n) exchange (n, optimized);
  }
}
#else
static void
optimize_in_place_wrapper (ir_node *n, void *env) {
  ir_node *optimized = optimize_in_place_2(n);
  if (optimized != n) exchange (n, optimized);
}
#endif



void
local_optimize_graph (ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  /* Handle graph state */
  assert(get_irg_phase_state(irg) != phase_building);
  if (get_opt_global_cse())
    set_irg_pinned(current_ir_graph, floats);
  if (get_irg_outs_state(current_ir_graph) == outs_consistent)
    set_irg_outs_inconsistent(current_ir_graph);
  if (get_irg_dom_state(current_ir_graph) == dom_consistent)
    set_irg_dom_inconsistent(current_ir_graph);

  /* Clean the value_table in irg for the cse. */
  del_identities(irg->value_table);
  irg->value_table = new_identities();

  /* walk over the graph */
  irg_walk(irg->end, init_link, optimize_in_place_wrapper, NULL);

  current_ir_graph = rem;
}

/*------------------------------------------------------------------*/
/* Routines for dead node elimination / copying garbage collection  */
/* of the obstack.                                                  */
/*------------------------------------------------------------------*/

/**
 * Remember the new node in the old node by using a field all nodes have.
 */
static INLINE void
set_new_node (ir_node *old, ir_node *new)
{
  old->link = new;
}

/**
 * Get this new node, before the old node is forgotton.
 */
static INLINE ir_node *
get_new_node (ir_node * n)
{
  return n->link;
}

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

/* TODO: add an ir_op operation */
static INLINE void new_backedge_info(ir_node *n) {
  switch(get_irn_opcode(n)) {
  case iro_Block:
    n->attr.block.cg_backedge = NULL;
    n->attr.block.backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  case iro_Phi:
    n->attr.phi_backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  case iro_Filter:
    n->attr.filter.backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  default: ;
  }
}

/**
 * Copies the node to the new obstack. The Ins of the new node point to
 * the predecessors on the old obstack.  For block/phi nodes not all
 * predecessors might be copied.  n->link points to the new node.
 * For Phi and Block nodes the function allocates in-arrays with an arity
 * only for useful predecessors.  The arity is determined by counting
 * the non-bad predecessors of the block.
 */
static void
copy_node (ir_node *n, void *env) {
  ir_node *nn, *block;
  int new_arity;

  /* The end node looses it's flexible in array.  This doesn't matter,
     as dead node elimination builds End by hand, inlineing doesn't use
     the End node. */
  /* assert(n->op == op_End ||  ((_ARR_DESCR(n->in))->cookie != ARR_F_MAGIC)); */

  if (get_irn_opcode(n) == iro_Block) {
    block = NULL;
    new_arity = compute_new_arity(n);
    n->attr.block.graph_arr = NULL;
  } else {
    block = get_nodes_Block(n);
    if (get_irn_opcode(n) == iro_Phi) {
      new_arity = compute_new_arity(block);
    } else {
      new_arity = get_irn_arity(n);
    }
  }
  nn = new_ir_node(get_irn_dbg_info(n),
           current_ir_graph,
           block,
           get_irn_op(n),
           get_irn_mode(n),
           new_arity,
           get_irn_in(n));
  /* Copy the attributes.  These might point to additional data.  If this
     was allocated on the old obstack the pointers now are dangling.  This
     frees e.g. the memory of the graph_arr allocated in new_immBlock. */
  copy_attrs(n, nn);
  new_backedge_info(nn);
  set_new_node(n, nn);

  /*  printf("\n old node: "); DDMSG2(n);
      printf(" new node: "); DDMSG2(nn); */

}

/**
 * Copies new predecessors of old node to new node remembered in link.
 * Spare the Bad predecessors of Phi and Block nodes.
 */
static void
copy_preds (ir_node *n, void *env) {
  ir_node *nn, *block;
  int i, j, irn_arity;

  nn = get_new_node(n);

  /* printf("\n old node: "); DDMSG2(n);
     printf(" new node: "); DDMSG2(nn);
     printf(" arities: old: %d, new: %d\n", get_irn_arity(n), get_irn_arity(nn)); */

  if (get_irn_opcode(n) == iro_Block) {
    /* Don't copy Bad nodes. */
    j = 0;
    irn_arity = get_irn_arity(n);
    for (i = 0; i < irn_arity; i++)
      if (get_irn_opcode(get_irn_n(n, i)) != iro_Bad) {
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
    (get_irn_op(get_Block_cfgpred(nn, 0)) == op_Jmp))
      exchange(nn, get_nodes_Block(get_Block_cfgpred(nn, 0)));
  } else if (get_irn_opcode(n) == iro_Phi) {
    /* Don't copy node if corresponding predecessor in block is Bad.
       The Block itself should not be Bad. */
    block = get_nodes_Block(n);
    set_irn_n (nn, -1, get_new_node(block));
    j = 0;
    irn_arity = get_irn_arity(n);
    for (i = 0; i < irn_arity; i++)
      if (get_irn_opcode(get_irn_n(block, i)) != iro_Bad) {
    set_irn_n (nn, j, get_new_node(get_irn_n(n, i)));
    /*if (is_backedge(n, i)) set_backedge(nn, j);*/
    j++;
      }
    /* If the pre walker reached this Phi after the post walker visited the
       block block_visited is > 0. */
    set_Block_block_visited(get_nodes_Block(n), 0);
    /* Compacting the Phi's ins might generate Phis with only one
       predecessor. */
    if (get_irn_arity(n) == 1)
      exchange(n, get_irn_n(n, 0));
  } else {
    irn_arity = get_irn_arity(n);
    for (i = -1; i < irn_arity; i++)
      set_irn_n (nn, i, get_new_node(get_irn_n(n, i)));
  }
  /* Now the new node is complete.  We can add it to the hash table for cse.
     @@@ inlinening aborts if we identify End. Why? */
  if(get_irn_op(nn) != op_End)
    add_identities (current_ir_graph->value_table, nn);
}

/**
 * Copies the graph recursively, compacts the keepalive of the end node.
 */
static void
copy_graph (void) {
  ir_node *oe, *ne; /* old end, new end */
  ir_node *ka;      /* keep alive */
  int i, irn_arity;

  oe = get_irg_end(current_ir_graph);
  /* copy the end node by hand, allocate dynamic in array! */
  ne = new_ir_node(get_irn_dbg_info(oe),
           current_ir_graph,
           NULL,
           op_End,
           mode_X,
           -1,
           NULL);
  /* Copy the attributes.  Well, there might be some in the future... */
  copy_attrs(oe, ne);
  set_new_node(oe, ne);

  /* copy the live nodes */
  irg_walk(get_nodes_Block(oe), copy_node, copy_preds, NULL);
  /* copy_preds for the end node ... */
  set_nodes_Block(ne, get_new_node(get_nodes_Block(oe)));

  /*- ... and now the keep alives. -*/
  /* First pick the not marked block nodes and walk them.  We must pick these
     first as else we will oversee blocks reachable from Phis. */
  irn_arity = get_irn_arity(oe);
  for (i = 0; i < irn_arity; i++) {
    ka = get_irn_intra_n(oe, i);
    if ((get_irn_op(ka) == op_Block) &&
    (get_irn_visited(ka) < get_irg_visited(current_ir_graph))) {
      /* We must keep the block alive and copy everything reachable */
      set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph)-1);
      irg_walk(ka, copy_node, copy_preds, NULL);
      add_End_keepalive(ne, get_new_node(ka));
    }
  }

  /* Now pick the Phis.  Here we will keep all! */
  irn_arity = get_irn_arity(oe);
  for (i = 0; i < irn_arity; i++) {
    ka = get_irn_intra_n(oe, i);
    if ((get_irn_op(ka) == op_Phi)) {
      if (get_irn_visited(ka) < get_irg_visited(current_ir_graph)) {
    /* We didn't copy the Phi yet.  */
    set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph)-1);
    irg_walk(ka, copy_node, copy_preds, NULL);
      }
      add_End_keepalive(ne, get_new_node(ka));
    }
  }
}

/**
 * Copies the graph reachable from current_ir_graph->end to the obstack
 * in current_ir_graph and fixes the environment.
 * Then fixes the fields in current_ir_graph containing nodes of the
 * graph.
 */
static void
copy_graph_env (void) {
  ir_node *old_end;
  /* Not all nodes remembered in current_ir_graph might be reachable
     from the end node.  Assure their link is set to NULL, so that
     we can test whether new nodes have been computed. */
  set_irn_link(get_irg_frame  (current_ir_graph), NULL);
  set_irn_link(get_irg_globals(current_ir_graph), NULL);
  set_irn_link(get_irg_args   (current_ir_graph), NULL);

  /* we use the block walk flag for removing Bads from Blocks ins. */
  inc_irg_block_visited(current_ir_graph);

  /* copy the graph */
  copy_graph();

  /* fix the fields in current_ir_graph */
  old_end = get_irg_end(current_ir_graph);
  set_irg_end (current_ir_graph, get_new_node(old_end));
  set_irg_end_except (current_ir_graph, get_irg_end(current_ir_graph));
  set_irg_end_reg    (current_ir_graph, get_irg_end(current_ir_graph));
  free_End(old_end);
  set_irg_end_block  (current_ir_graph, get_new_node(get_irg_end_block(current_ir_graph)));
  if (get_irn_link(get_irg_frame(current_ir_graph)) == NULL) {
    copy_node (get_irg_frame(current_ir_graph), NULL);
    copy_preds(get_irg_frame(current_ir_graph), NULL);
  }
  if (get_irn_link(get_irg_globals(current_ir_graph)) == NULL) {
    copy_node (get_irg_globals(current_ir_graph), NULL);
    copy_preds(get_irg_globals(current_ir_graph), NULL);
  }
  if (get_irn_link(get_irg_args(current_ir_graph)) == NULL) {
    copy_node (get_irg_args(current_ir_graph), NULL);
    copy_preds(get_irg_args(current_ir_graph), NULL);
  }
  set_irg_start  (current_ir_graph, get_new_node(get_irg_start(current_ir_graph)));

  set_irg_start_block(current_ir_graph,
              get_new_node(get_irg_start_block(current_ir_graph)));
  set_irg_frame  (current_ir_graph, get_new_node(get_irg_frame(current_ir_graph)));
  set_irg_globals(current_ir_graph, get_new_node(get_irg_globals(current_ir_graph)));
  set_irg_args   (current_ir_graph, get_new_node(get_irg_args(current_ir_graph)));
  if (get_irn_link(get_irg_bad(current_ir_graph)) == NULL) {
    copy_node(get_irg_bad(current_ir_graph), NULL);
    copy_preds(get_irg_bad(current_ir_graph), NULL);
  }
  set_irg_bad(current_ir_graph, get_new_node(get_irg_bad(current_ir_graph)));
  /* GL removed: we need unknown with mode for analyses.
  if (get_irn_link(get_irg_unknown(current_ir_graph)) == NULL) {
    copy_node(get_irg_unknown(current_ir_graph), NULL);
    copy_preds(get_irg_unknown(current_ir_graph), NULL);
  }
  set_irg_unknown(current_ir_graph, get_new_node(get_irg_unknown(current_ir_graph)));
  */
}

/**
 * Copies all reachable nodes to a new obstack.  Removes bad inputs
 * from block nodes and the corresponding inputs from Phi nodes.
 * Merges single exit blocks with single entry blocks and removes
 * 1-input Phis.
 * Adds all new nodes to a new hash table for cse.  Does not
 * perform cse, so the hash table might contain common subexpressions.
 */
/* Amroq call this emigrate() */
void
dead_node_elimination(ir_graph *irg) {
  ir_graph *rem;
  int rem_ipview = interprocedural_view;
  struct obstack *graveyard_obst = NULL;
  struct obstack *rebirth_obst   = NULL;

  stat_dead_node_elim_start(irg);

  /* Remember external state of current_ir_graph. */
  rem = current_ir_graph;
  current_ir_graph = irg;
  interprocedural_view = 0;

  /* Handle graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  assert(get_irg_callee_info_state(current_ir_graph) == irg_callee_info_none);
  free_outs(current_ir_graph);

  /* @@@ so far we loose loops when copying */
  free_loop_information(current_ir_graph);

  if (get_opt_optimize() && get_opt_dead_node_elimination()) {

    /* A quiet place, where the old obstack can rest in peace,
       until it will be cremated. */
    graveyard_obst = irg->obst;

    /* A new obstack, where the reachable nodes will be copied to. */
    rebirth_obst = (struct obstack *) xmalloc (sizeof (struct obstack));
    current_ir_graph->obst = rebirth_obst;
    obstack_init (current_ir_graph->obst);

    /* We also need a new hash table for cse */
    del_identities (irg->value_table);
    irg->value_table = new_identities ();

    /* Copy the graph from the old to the new obstack */
    copy_graph_env();

    /* Free memory from old unoptimized obstack */
    obstack_free(graveyard_obst, 0);  /* First empty the obstack ... */
    xfree (graveyard_obst);           /* ... then free it.           */
  }

  stat_dead_node_elim_stop(irg);

  current_ir_graph = rem;
  interprocedural_view = rem_ipview;
}

/**
 * Relink bad predeseccors of a block and store the old in array to the
 * link field. This function is called by relink_bad_predecessors().
 * The array of link field starts with the block operand at position 0.
 * If block has bad predecessors, create a new in array without bad preds.
 * Otherwise let in array untouched.
 */
static void relink_bad_block_predecessors(ir_node *n, void *env) {
  ir_node **new_in, *irn;
  int i, new_irn_n, old_irn_arity, new_irn_arity = 0;

  /* if link field of block is NULL, look for bad predecessors otherwise
     this is allready done */
  if (get_irn_op(n) == op_Block &&
      get_irn_link(n) == NULL) {

    /* save old predecessors in link field (position 0 is the block operand)*/
    set_irn_link(n, (void *)get_irn_in(n));

    /* count predecessors without bad nodes */
    old_irn_arity = get_irn_arity(n);
    for (i = 0; i < old_irn_arity; i++)
      if (!is_Bad(get_irn_n(n, i))) new_irn_arity++;

    /* arity changing: set new predecessors without bad nodes */
    if (new_irn_arity < old_irn_arity) {
      /* get new predecessor array without Block predecessor */
      new_in = NEW_ARR_D (ir_node *, current_ir_graph->obst, (new_irn_arity+1));

      /* set new predeseccors in array */
      new_in[0] = NULL;
      new_irn_n = 1;
      for (i = 1; i < old_irn_arity; i++) {
    irn = get_irn_n(n, i);
    if (!is_Bad(irn)) new_in[new_irn_n++] = irn;
      }
      n->in = new_in;
    } /* ir node has bad predecessors */

  } /* Block is not relinked */
}

/*
 * Relinks Bad predecesors from Bocks and Phis called by walker
 * remove_bad_predecesors(). If n is a Block, call
 * relink_bad_block_redecessors(). If n is a Phinode, call also the relinking
 * function of Phi's Block. If this block has bad predecessors, relink preds
 * of the Phinode.
 */
static void relink_bad_predecessors(ir_node *n, void *env) {
  ir_node *block, **old_in;
  int i, old_irn_arity, new_irn_arity;

  /* relink bad predeseccors of a block */
  if (get_irn_op(n) == op_Block)
    relink_bad_block_predecessors(n, env);

  /* If Phi node relink its block and its predecessors */
  if (get_irn_op(n) == op_Phi) {

    /* Relink predeseccors of phi's block */
    block = get_nodes_Block(n);
    if (get_irn_link(block) == NULL)
      relink_bad_block_predecessors(block, env);

    old_in = (ir_node **)get_irn_link(block); /* Of Phi's Block */
    old_irn_arity = ARR_LEN(old_in);

    /* Relink Phi predeseccors if count of predeseccors changed */
    if (old_irn_arity != ARR_LEN(get_irn_in(block))) {
      /* set new predeseccors in array
     n->in[0] remains the same block */
      new_irn_arity = 1;
      for(i = 1; i < old_irn_arity; i++)
    if (!is_Bad((ir_node *)old_in[i])) n->in[new_irn_arity++] = n->in[i];

      ARR_SETLEN(ir_node *, n->in, new_irn_arity);
    }

  } /* n is a Phi node */
}

/**
 * Removes Bad Bad predecesors from Blocks and the corresponding
 * inputs to Phi nodes as in dead_node_elimination but without
 * copying the graph.
 * On walking up set the link field to NULL, on walking down call
 * relink_bad_predecessors() (This function stores the old in array
 * to the link field and sets a new in array if arity of predecessors
 * changes).
 */
void remove_bad_predecessors(ir_graph *irg) {
  irg_walk_graph(irg, init_link, relink_bad_predecessors, NULL);
}


/*--------------------------------------------------------------------*/
/*  Funcionality for inlining                                         */
/*--------------------------------------------------------------------*/

/**
 * Copy node for inlineing.  Updates attributes that change when
 * inlineing but not for dead node elimination.
 *
 * Copies the node by calling copy_node and then updates the entity if
 * it's a local one.  env must be a pointer of the frame type of the
 * inlined procedure. The new entities must be in the link field of
 * the entities.
 */
static INLINE void
copy_node_inline (ir_node *n, void *env) {
  ir_node *new;
  type *frame_tp = (type *)env;

  copy_node(n, NULL);
  if (get_irn_op(n) == op_Sel) {
    new = get_new_node (n);
    assert(get_irn_op(new) == op_Sel);
    if (get_entity_owner(get_Sel_entity(n)) == frame_tp) {
      set_Sel_entity(new, get_entity_link(get_Sel_entity(n)));
    }
  } else if (get_irn_op(n) == op_Block) {
    new = get_new_node (n);
    new->attr.block.irg = current_ir_graph;
  }
}

static void find_addr(ir_node *node, void *env)
{
  if (get_irn_opcode(node) == iro_Proj) {
    if (get_Proj_proj(node) == pn_Start_P_value_arg_base)
      *(int *)env = 0;
  }
}

/*
 * currently, we cannot inline two cases:
 * - call with compound arguments
 * - graphs that take the address of a parameter
 *
 * check these condition here
 */
static int can_inline(ir_node *call, ir_graph *called_graph)
{
  type *call_type = get_Call_type(call);
  int params, ress, i, res;

  assert(is_method_type(call_type));

  params = get_method_n_params(call_type);
  ress   = get_method_n_ress(call_type);

  /* check params */
  for (i = 0; i < params; ++i) {
    type *p_type = get_method_param_type(call_type, i);

    if (is_compound_type(p_type))
      return 0;
  }

  /* check res */
  for (i = 0; i < ress; ++i) {
    type *r_type = get_method_res_type(call_type, i);

    if (is_compound_type(r_type))
      return 0;
  }

  res = 1;
  irg_walk_graph(called_graph, find_addr, NULL, &res);

  return res;
}

int inline_method(ir_node *call, ir_graph *called_graph) {
  ir_node *pre_call;
  ir_node *post_call, *post_bl;
  ir_node *in[5];
  ir_node *end, *end_bl;
  ir_node **res_pred;
  ir_node **cf_pred;
  ir_node *ret, *phi;
  int arity, n_ret, n_exc, n_res, i, j, rem_opt, irn_arity;
  int exc_handling;
  type *called_frame;
  irg_inline_property prop = get_irg_inline_property(called_graph);

  if ( (prop != irg_inline_forced) && (!get_opt_optimize() || !get_opt_inline() ||
      (prop == irg_inline_forbidden))) return 0;


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
  assert(get_irg_pinned(current_ir_graph) == pinned);
  assert(get_irg_pinned(called_graph) == pinned);
  if (get_irg_outs_state(current_ir_graph) == outs_consistent)
    set_irg_outs_inconsistent(current_ir_graph);

  /* -- Check preconditions -- */
  assert(get_irn_op(call) == op_Call);
  /* @@@ does not work for InterfaceIII.java after cgana
     assert(get_Call_type(call) == get_entity_type(get_irg_ent(called_graph)));
     assert(smaller_type(get_entity_type(get_irg_ent(called_graph)),
     get_Call_type(call)));
  */
  assert(get_type_tpop(get_Call_type(call)) == type_method);
  if (called_graph == current_ir_graph) {
    set_optimize(rem_opt);
    return 0;
  }

  /* here we know we WILL inline, so inform the statistics */
  stat_inline(call, called_graph);

  /* -- Decide how to handle exception control flow: Is there a handler
     for the Call node, or do we branch directly to End on an exception?
     exc_handling: 0 There is a handler.
                   1 Branches to End.
           2 Exception handling not represented in Firm. -- */
  {
    ir_node *proj, *Mproj = NULL, *Xproj = NULL;
    for (proj = (ir_node *)get_irn_link(call); proj; proj = (ir_node *)get_irn_link(proj)) {
      assert(get_irn_op(proj) == op_Proj);
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
  post_bl = get_nodes_Block(call);
  set_irg_current_block(current_ir_graph, post_bl);
  /* XxMxPxP of Start + parameter of Call */
  in[pn_Start_X_initial_exec] = new_Jmp();
  in[pn_Start_M]              = get_Call_mem(call);
  in[pn_Start_P_frame_base]   = get_irg_frame(current_ir_graph);
  in[pn_Start_P_globals]      = get_irg_globals(current_ir_graph);
  in[pn_Start_T_args]         = new_Tuple(get_Call_n_params(call), get_Call_param_arr(call));
  /* in[pn_Start_P_value_arg_base] = ??? */
  pre_call = new_Tuple(5, in);
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
  set_irn_visited(get_irg_start(called_graph),
          get_irg_visited(current_ir_graph));
  set_irn_link(get_irg_start_block(called_graph),
           get_nodes_Block(pre_call));
  set_irn_visited(get_irg_start_block(called_graph),
          get_irg_visited(current_ir_graph));

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

  res_pred = (ir_node **) malloc (n_res * sizeof (ir_node *));
  cf_pred =  (ir_node **) malloc (arity * sizeof (ir_node *));

  set_irg_current_block(current_ir_graph, post_bl); /* just to make sure */

  /* -- archive keepalives -- */
  irn_arity = get_irn_arity(end);
  for (i = 0; i < irn_arity; i++)
    add_End_keepalive(get_irg_end(current_ir_graph), get_irn_n(end, i));

  /* The new end node will die.  We need not free as the in array is on the obstack:
     copy_node only generated 'D' arrays. */

  /* -- Replace Return nodes by Jump nodes. -- */
  n_ret = 0;
  for (i = 0; i < arity; i++) {
    ir_node *ret;
    ret = get_irn_n(end_bl, i);
    if (get_irn_op(ret) == op_Return) {
      cf_pred[n_ret] = new_r_Jmp(current_ir_graph, get_nodes_Block(ret));
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
    if (get_irn_op(ret) == op_Return) {
      cf_pred[n_ret] = get_Return_mem(ret);
      n_ret++;
    }
  }
  phi = new_Phi(n_ret, cf_pred, mode_M);
  set_Tuple_pred(call, pn_Call_M_regular, phi);
  /* Conserve Phi-list for further inlinings -- but might be optimized */
  if (get_nodes_Block(phi) == post_bl) {
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
      phi = new_Phi(n_ret, cf_pred, get_irn_mode(cf_pred[0]));
      res_pred[j] = phi;
      /* Conserve Phi-list for further inlinings -- but might be optimized */
      if (get_nodes_Block(phi) == post_bl) {
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
    if (get_irn_op(ret) == op_Call) {
      cf_pred[n_exc] = new_r_Proj(current_ir_graph, get_nodes_Block(ret), ret, mode_M, 3);
      n_exc++;
    } else if (is_fragile_op(ret)) {
    /* We rely that all cfops have the memory output at the same position. */
      cf_pred[n_exc] = new_r_Proj(current_ir_graph, get_nodes_Block(ret), ret, mode_M, 0);
      n_exc++;
    } else if (get_irn_op(ret) == op_Raise) {
      cf_pred[n_exc] = new_r_Proj(current_ir_graph, get_nodes_Block(ret), ret, mode_M, 1);
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
    end_preds =  (ir_node **) malloc ((n_exc + main_end_bl_arity) * sizeof (ir_node *));

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

#if 0  /* old. now better, correcter, faster implementation. */
  if (n_exc > 0) {
    /* -- If the exception control flow from the inlined Call directly
       branched to the end block we now have the following control
       flow predecessor pattern: ProjX -> Tuple -> Jmp.  We must
       remove the Jmp along with it's empty block and add Jmp's
       predecessors as predecessors of this end block.  No problem if
       there is no exception, because then branches Bad to End which
       is fine. --
       @@@ can't we know this beforehand: by getting the Proj(1) from
       the Call link list and checking whether it goes to Proj. */
    /* find the problematic predecessor of the end block. */
    end_bl = get_irg_end_block(current_ir_graph);
    for (i = 0; i < get_Block_n_cfgpreds(end_bl); i++) {
      cf_op = get_Block_cfgpred(end_bl, i);
      if (get_irn_op(cf_op) == op_Proj) {
    cf_op = get_Proj_pred(cf_op);
    if ((get_irn_op(cf_op) == op_Tuple) && (cf_op == call)) {
      /*  There are unoptimized tuples from inlineing before when no exc */
      assert(get_Proj_proj(get_Block_cfgpred(end_bl, i)) == pn_Call_X_except);
      cf_op = get_Tuple_pred(cf_op, pn_Call_X_except);
      assert(get_irn_op(cf_op) == op_Jmp);
      break;
    }
      }
    }
    /* repair */
    if (i < get_Block_n_cfgpreds(end_bl)) {
      bl = get_nodes_Block(cf_op);
      arity = get_Block_n_cfgpreds(end_bl) + get_Block_n_cfgpreds(bl) - 1;
      cf_pred = (ir_node **) malloc (arity * sizeof (ir_node *));
      for (j = 0; j < i; j++)
    cf_pred[j] = get_Block_cfgpred(end_bl, j);
      for (j = j; j < i + get_Block_n_cfgpreds(bl); j++)
    cf_pred[j] = get_Block_cfgpred(bl, j-i);
      for (j = j; j < arity; j++)
    cf_pred[j] = get_Block_cfgpred(end_bl, j-get_Block_n_cfgpreds(bl) +1);
      set_irn_in(end_bl, arity, cf_pred);
      free(cf_pred);
      /*  Remove the exception pred from post-call Tuple. */
      set_Tuple_pred(call, pn_Call_X_except, new_Bad());
    }
  }
#endif

  /* --  Turn cse back on. -- */
  set_optimize(rem_opt);

  return 1;
}

/********************************************************************/
/* Apply inlineing to small methods.                                */
/********************************************************************/

/* It makes no sense to inline too many calls in one procedure. Anyways,
   I didn't get a version with NEW_ARR_F to run. */
#define MAX_INLINE 1024

/**
 * environment for inlining small irgs
 */
typedef struct _inline_env_t {
  int pos;
  ir_node *calls[MAX_INLINE];
} inline_env_t;

/**
 * Returns the irg called from a Call node. If the irg is not
 * known, NULL is returned.
 */
static ir_graph *get_call_called_irg(ir_node *call) {
  ir_node *addr;
  tarval *tv;
  ir_graph *called_irg = NULL;

  assert(get_irn_op(call) == op_Call);

  addr = get_Call_ptr(call);
  if (get_irn_op(addr) == op_Const) {
    /* Check whether the constant is the pointer to a compiled entity. */
    tv = get_Const_tarval(addr);
    if (tarval_to_entity(tv))
      called_irg = get_entity_irg(tarval_to_entity(tv));
  }
  return called_irg;
}

static void collect_calls(ir_node *call, void *env) {
  inline_env_t *ienv = env;
  ir_node *addr;
  tarval *tv;
  ir_graph *called_irg;

  if (get_irn_op(call) != op_Call) return;

  addr = get_Call_ptr(call);
  if (get_irn_op(addr) == op_Const) {
    /* Check whether the constant is the pointer to a compiled entity. */
    tv = get_Const_tarval(addr);
    if (tarval_to_entity(tv)) {
      called_irg = get_entity_irg(tarval_to_entity(tv));
      if (called_irg && ienv->pos < MAX_INLINE) {
        /* The Call node calls a locally defined method.  Remember to inline. */
        ienv->calls[ienv->pos++] = call;
      }
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
  int i;
  ir_graph *rem = current_ir_graph;
  inline_env_t env;

  if (!(get_opt_optimize() && get_opt_inline())) return;

  current_ir_graph = irg;
  /* Handle graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  assert(get_irg_callee_info_state(current_ir_graph) == irg_callee_info_none);

  /* Find Call nodes to inline.
     (We can not inline during a walk of the graph, as inlineing the same
     method several times changes the visited flag of the walked graph:
     after the first inlineing visited of the callee equals visited of
     the caller.  With the next inlineing both are increased.) */
  env.pos = 0;
  irg_walk(get_irg_end(irg), NULL, collect_calls, &env);

  if ((env.pos > 0) && (env.pos < MAX_INLINE)) {
    /* There are calls to inline */
    collect_phiprojs(irg);
    for (i = 0; i < env.pos; i++) {
      tarval *tv;
      ir_graph *callee;
      tv = get_Const_tarval(get_Call_ptr(env.calls[i]));
      callee = get_entity_irg(tarval_to_entity(tv));
      if (((_obstack_memory_used(callee->obst) - obstack_room(callee->obst)) < size) ||
        (get_irg_inline_property(callee) == irg_inline_forced)) {
        inline_method(env.calls[i], callee);
      }
    }
  }

  current_ir_graph = rem;
}

/**
 * Environment for inlining irgs.
 */
typedef struct {
  int n_nodes;       /**< Nodes in graph except Id, Tuple, Proj, Start, End */
  int n_nodes_orig;  /**< for statistics */
  eset *call_nodes;  /**< All call nodes in this graph */
  int n_call_nodes;
  int n_call_nodes_orig; /**< for statistics */
  int n_callers;   /**< Number of known graphs that call this graphs. */
  int n_callers_orig; /**< for statistics */
} inline_irg_env;

static inline_irg_env *new_inline_irg_env(void) {
  inline_irg_env *env = malloc(sizeof(inline_irg_env));
  env->n_nodes = -2; /* uncount Start, End */
  env->n_nodes_orig = -2; /* uncount Start, End */
  env->call_nodes = eset_create();
  env->n_call_nodes = 0;
  env->n_call_nodes_orig = 0;
  env->n_callers = 0;
  env->n_callers_orig = 0;
  return env;
}

static void free_inline_irg_env(inline_irg_env *env) {
  eset_destroy(env->call_nodes);
  free(env);
}

static void collect_calls2(ir_node *call, void *env) {
  inline_irg_env *x = (inline_irg_env *)env;
  ir_op *op = get_irn_op(call);
  ir_graph *callee;

  /* count nodes in irg */
  if (op != op_Proj && op != op_Tuple && op != op_Sync) {
    x->n_nodes++;
    x->n_nodes_orig++;
  }

  if (op != op_Call) return;

  /* collect all call nodes */
  eset_insert(x->call_nodes, (void *)call);
  x->n_call_nodes++;
  x->n_call_nodes_orig++;

  /* count all static callers */
  callee = get_call_called_irg(call);
  if (callee) {
    ((inline_irg_env *)get_irg_link(callee))->n_callers++;
    ((inline_irg_env *)get_irg_link(callee))->n_callers_orig++;
  }
}

INLINE static int is_leave(ir_graph *irg) {
  return (((inline_irg_env *)get_irg_link(irg))->n_call_nodes == 0);
}

INLINE static int is_smaller(ir_graph *callee, int size) {
  return (((inline_irg_env *)get_irg_link(callee))->n_nodes < size);
}


/*
 * Inlines small leave methods at call sites where the called address comes
 * from a Const node that references the entity representing the called
 * method.
 * The size argument is a rough measure for the code size of the method:
 * Methods where the obstack containing the firm graph is smaller than
 * size are inlined.
 */
void inline_leave_functions(int maxsize, int leavesize, int size) {
  inline_irg_env *env;
  int i, n_irgs = get_irp_n_irgs();
  ir_graph *rem = current_ir_graph;
  int did_inline = 1;

  if (!(get_opt_optimize() && get_opt_inline())) return;

  /* extend all irgs by a temporary data structure for inlineing. */
  for (i = 0; i < n_irgs; ++i)
    set_irg_link(get_irp_irg(i), new_inline_irg_env());

  /* Precompute information in temporary data structure. */
  for (i = 0; i < n_irgs; ++i) {
    current_ir_graph = get_irp_irg(i);
    assert(get_irg_phase_state(current_ir_graph) != phase_building);
    assert(get_irg_callee_info_state(current_ir_graph) == irg_callee_info_none);

    irg_walk(get_irg_end(current_ir_graph), NULL, collect_calls2,
         get_irg_link(current_ir_graph));
  }

  /* and now inline.
     Inline leaves recursively -- we might construct new leaves. */
  /* int itercnt = 1; */
  while (did_inline) {
    /* printf("iteration %d\n", itercnt++); */
    did_inline = 0;
    for (i = 0; i < n_irgs; ++i) {
      ir_node *call;
      eset *walkset;
      int phiproj_computed = 0;

      current_ir_graph = get_irp_irg(i);
      env = (inline_irg_env *)get_irg_link(current_ir_graph);

      /* we can not walk and change a set, nor remove from it.
      So recompute.*/
      walkset = env->call_nodes;
      env->call_nodes = eset_create();
      for (call = eset_first(walkset); call; call = eset_next(walkset)) {
        inline_irg_env *callee_env;
        ir_graph *callee = get_call_called_irg(call);

        if (env->n_nodes > maxsize) break;
        if (callee &&
        ((is_leave(callee) && is_smaller(callee, leavesize)) ||
         (get_irg_inline_property(callee) == irg_inline_forced))) {
          if (!phiproj_computed) {
            phiproj_computed = 1;
            collect_phiprojs(current_ir_graph);
          }
          callee_env = (inline_irg_env *)get_irg_link(callee);
/*         printf(" %s: Inlineing %s.\n", get_entity_name(get_irg_entity(current_ir_graph)), */
/*         get_entity_name(get_irg_entity(callee))); */
          if (inline_method(call, callee)) {
            did_inline = 1;
	    env->n_call_nodes--;
	    eset_insert_all(env->call_nodes, callee_env->call_nodes);
	    env->n_call_nodes += callee_env->n_call_nodes;
	    env->n_nodes += callee_env->n_nodes;
	    callee_env->n_callers--;
	  }
        } else {
          eset_insert(env->call_nodes, call);
        }
      }
      eset_destroy(walkset);
    }
  }

  /* printf("Non leaves\n"); */
  /* inline other small functions. */
  for (i = 0; i < n_irgs; ++i) {
    ir_node *call;
    eset *walkset;
    int phiproj_computed = 0;

    current_ir_graph = get_irp_irg(i);
    env = (inline_irg_env *)get_irg_link(current_ir_graph);

    /* we can not walk and change a set, nor remove from it.
       So recompute.*/
    walkset = env->call_nodes;
    env->call_nodes = eset_create();
    for (call = eset_first(walkset); call; call = eset_next(walkset)) {
      inline_irg_env *callee_env;
      ir_graph *callee = get_call_called_irg(call);

      if (env->n_nodes > maxsize) break;
      if (callee && is_smaller(callee, size)) {
        if (!phiproj_computed) {
            phiproj_computed = 1;
            collect_phiprojs(current_ir_graph);
        }
        callee_env = (inline_irg_env *)get_irg_link(callee);
/*       printf(" %s: Inlineing %s.\n", get_entity_name(get_irg_entity(current_ir_graph)), */
/*       get_entity_name(get_irg_entity(callee))); */
        if (inline_method(call, callee)) {
	  did_inline = 1;
	  env->n_call_nodes--;
	  eset_insert_all(env->call_nodes, callee_env->call_nodes);
	  env->n_call_nodes += callee_env->n_call_nodes;
	  env->n_nodes += callee_env->n_nodes;
	  callee_env->n_callers--;
	}
      } else {
        eset_insert(env->call_nodes, call);
      }
    }
    eset_destroy(walkset);
  }

  for (i = 0; i < n_irgs; ++i) {
    current_ir_graph = get_irp_irg(i);
#if 0
    env = (inline_irg_env *)get_irg_link(current_ir_graph);
    if ((env->n_call_nodes_orig != env->n_call_nodes) ||
    (env->n_callers_orig != env->n_callers))
      printf("Nodes:%3d ->%3d, calls:%3d ->%3d, callers:%3d ->%3d, -- %s\n",
         env->n_nodes_orig, env->n_nodes, env->n_call_nodes_orig, env->n_call_nodes,
         env->n_callers_orig, env->n_callers,
         get_entity_name(get_irg_entity(current_ir_graph)));
#endif
    free_inline_irg_env((inline_irg_env *)get_irg_link(current_ir_graph));
  }

  current_ir_graph = rem;
}

/*******************************************************************/
/*  Code Placement.  Pins all floating nodes to a block where they */
/*  will be executed only if needed.                               */
/*******************************************************************/

/**
 * Find the earliest correct block for N.  --- Place N into the
 * same Block as its dominance-deepest Input.
 */
static void
place_floats_early(ir_node *n, pdeq *worklist)
{
  int i, start, irn_arity;

  /* we must not run into an infinite loop */
  assert (irn_not_visited(n));
  mark_irn_visited(n);

  /* Place floating nodes. */
  if (get_op_pinned(get_irn_op(n)) == floats) {
    int depth = 0;
    ir_node *b = new_Bad();   /* The block to place this node in */

    assert(get_irn_op(n) != op_Block);

    if ((get_irn_op(n) == op_Const) ||
    (get_irn_op(n) == op_SymConst) ||
    (is_Bad(n)) ||
    (get_irn_op(n) == op_Unknown)) {
      /* These nodes will not be placed by the loop below. */
      b = get_irg_start_block(current_ir_graph);
      depth = 1;
    }

    /* find the block for this node. */
    irn_arity = get_irn_arity(n);
    for (i = 0; i < irn_arity; i++) {
      ir_node *dep = get_irn_n(n, i);
      ir_node *dep_block;
      if ((irn_not_visited(dep)) &&
      (get_op_pinned(get_irn_op(dep)) == floats)) {
	place_floats_early(dep, worklist);
      }
      /* Because all loops contain at least one pinned node, now all
         our inputs are either pinned or place_early has already
         been finished on them.  We do not have any unfinished inputs!  */
      dep_block = get_nodes_Block(dep);
      if ((!is_Bad(dep_block)) &&
	  (get_Block_dom_depth(dep_block) > depth)) {
	b = dep_block;
	depth = get_Block_dom_depth(dep_block);
      }
      /* Avoid that the node is placed in the Start block */
      if ((depth == 1) && (get_Block_dom_depth(get_nodes_Block(n)) > 1)) {
	b = get_Block_cfg_out(get_irg_start_block(current_ir_graph), 0);
	assert(b != get_irg_start_block(current_ir_graph));
	depth = 2;
      }
    }
    set_nodes_Block(n, b);
  }

  /* Add predecessors of non floating nodes on worklist. */
  start = (get_irn_op(n) == op_Block) ? 0 : -1;
  irn_arity = get_irn_arity(n);
  for (i = start; i < irn_arity; i++) {
    ir_node *pred = get_irn_n(n, i);
    if (irn_not_visited(pred)) {
      pdeq_putr (worklist, pred);
    }
  }
}

/**
 * Floating nodes form subgraphs that begin at nodes as Const, Load,
 * Start, Call and end at pinned nodes as Store, Call.  Place_early
 * places all floating nodes reachable from its argument through floating
 * nodes and adds all beginnings at pinned nodes to the worklist.
 */
static INLINE void place_early(pdeq* worklist) {
  assert(worklist);
  inc_irg_visited(current_ir_graph);

  /* this inits the worklist */
  place_floats_early(get_irg_end(current_ir_graph), worklist);

  /* Work the content of the worklist. */
  while (!pdeq_empty (worklist)) {
    ir_node *n = pdeq_getl (worklist);
    if (irn_not_visited(n)) place_floats_early(n, worklist);
  }

  set_irg_outs_inconsistent(current_ir_graph);
  current_ir_graph->pinned = pinned;
}


/** deepest common dominance ancestor of DCA and CONSUMER of PRODUCER. */
static ir_node *
consumer_dom_dca (ir_node *dca, ir_node *consumer, ir_node *producer)
{
  ir_node *block = NULL;

  /* Compute the latest block into which we can place a node so that it is
     before consumer. */
  if (get_irn_op(consumer) == op_Phi) {
    /* our consumer is a Phi-node, the effective use is in all those
       blocks through which the Phi-node reaches producer */
    int i, irn_arity;
    ir_node *phi_block = get_nodes_Block(consumer);
    irn_arity = get_irn_arity(consumer);
    for (i = 0;  i < irn_arity; i++) {
      if (get_irn_n(consumer, i) == producer) {
    block = get_nodes_Block(get_Block_cfgpred(phi_block, i));
      }
    }
  } else {
    assert(is_no_Block(consumer));
    block = get_nodes_Block(consumer);
  }

  /* Compute the deepest common ancestor of block and dca. */
  assert(block);
  if (!dca) return block;
  while (get_Block_dom_depth(block) > get_Block_dom_depth(dca))
    block = get_Block_idom(block);
  while (get_Block_dom_depth(dca) > get_Block_dom_depth(block))
    dca = get_Block_idom(dca);
  while (block != dca)
    { block = get_Block_idom(block); dca = get_Block_idom(dca); }

  return dca;
}

static INLINE int get_irn_loop_depth(ir_node *n) {
  return get_loop_depth(get_irn_loop(n));
}

/**
 * Move n to a block with less loop depth than it's current block. The
 * new block must be dominated by early.
 */
static void
move_out_of_loops (ir_node *n, ir_node *early)
{
  ir_node *best, *dca;
  assert(n && early);


  /* Find the region deepest in the dominator tree dominating
     dca with the least loop nesting depth, but still dominated
     by our early placement. */
  dca = get_nodes_Block(n);
  best = dca;
  while (dca != early) {
    dca = get_Block_idom(dca);
    if (!dca) break; /* should we put assert(dca)? */
    if (get_irn_loop_depth(dca) < get_irn_loop_depth(best)) {
      best = dca;
    }
  }
  if (best != get_nodes_Block(n)) {
    /* debug output
    printf("Moving out of loop: "); DDMN(n);
    printf(" Outermost block: "); DDMN(early);
    printf(" Best block: "); DDMN(best);
    printf(" Innermost block: "); DDMN(get_nodes_Block(n));
    */
    set_nodes_Block(n, best);
  }
}

/**
 * Find the latest legal block for N and place N into the
 * `optimal' Block between the latest and earliest legal block.
 * The `optimal' block is the dominance-deepest block of those
 * with the least loop-nesting-depth.  This places N out of as many
 * loops as possible and then makes it as control dependant as
 * possible.
 */
static void
place_floats_late(ir_node *n, pdeq *worklist)
{
  int i;
  ir_node *early;

  assert (irn_not_visited(n)); /* no multiple placement */

  /* no need to place block nodes, control nodes are already placed. */
  if ((get_irn_op(n) != op_Block) &&
      (!is_cfop(n)) &&
      (get_irn_mode(n) != mode_X)) {
    /* Remember the early placement of this block to move it
       out of loop no further than the early placement. */
    early = get_nodes_Block(n);
    /* Assure that our users are all placed, except the Phi-nodes.
       --- Each data flow cycle contains at least one Phi-node.  We
       have to break the `user has to be placed before the
       producer' dependence cycle and the Phi-nodes are the
       place to do so, because we need to base our placement on the
       final region of our users, which is OK with Phi-nodes, as they
       are pinned, and they never have to be placed after a
       producer of one of their inputs in the same block anyway. */
    for (i = 0; i < get_irn_n_outs(n); i++) {
      ir_node *succ = get_irn_out(n, i);
      if (irn_not_visited(succ) && (get_irn_op(succ) != op_Phi))
    place_floats_late(succ, worklist);
    }

    /* We have to determine the final block of this node... except for
       constants. */
    if ((get_op_pinned(get_irn_op(n)) == floats) &&
    (get_irn_op(n) != op_Const) &&
    (get_irn_op(n) != op_SymConst)) {
      ir_node *dca = NULL;  /* deepest common ancestor in the
                   dominator tree of all nodes'
                   blocks depending on us; our final
                   placement has to dominate DCA. */
      for (i = 0; i < get_irn_n_outs(n); i++) {
    dca = consumer_dom_dca (dca, get_irn_out(n, i), n);
      }
      set_nodes_Block(n, dca);

      move_out_of_loops (n, early);
    }
  }

  mark_irn_visited(n);

  /* Add predecessors of all non-floating nodes on list. (Those of floating
     nodes are placeded already and therefore are marked.)  */
  for (i = 0; i < get_irn_n_outs(n); i++) {
    if (irn_not_visited(get_irn_out(n, i))) {
      pdeq_putr (worklist, get_irn_out(n, i));
    }
  }
}

static INLINE void place_late(pdeq* worklist) {
  assert(worklist);
  inc_irg_visited(current_ir_graph);

  /* This fills the worklist initially. */
  place_floats_late(get_irg_start_block(current_ir_graph), worklist);
  /* And now empty the worklist again... */
  while (!pdeq_empty (worklist)) {
    ir_node *n = pdeq_getl (worklist);
    if (irn_not_visited(n)) place_floats_late(n, worklist);
  }
}

void place_code(ir_graph *irg) {
  pdeq* worklist;
  ir_graph *rem = current_ir_graph;

  current_ir_graph = irg;

  if (!(get_opt_optimize() && get_opt_global_cse())) return;

  /* Handle graph state */
  assert(get_irg_phase_state(irg) != phase_building);
  if (get_irg_dom_state(irg) != dom_consistent)
    compute_doms(irg);

  if (get_irg_loopinfo_state(irg) != loopinfo_consistent) {
    free_loop_information(irg);
    construct_backedges(irg);
  }

  /* Place all floating nodes as early as possible. This guarantees
     a legal code placement. */
  worklist = new_pdeq();
  place_early(worklist);

  /* place_early invalidates the outs, place_late needs them. */
  compute_outs(irg);
  /* Now move the nodes down in the dominator tree. This reduces the
     unnecessary executions of the node. */
  place_late(worklist);

  set_irg_outs_inconsistent(current_ir_graph);
  set_irg_loopinfo_inconsistent(current_ir_graph);
  del_pdeq(worklist);
  current_ir_graph = rem;
}



/********************************************************************/
/* Control flow optimization.                                       */
/* Removes Bad control flow predecessors and empty blocks.  A block */
/* is empty if it contains only a Jmp node.                         */
/* Blocks can only be removed if they are not needed for the        */
/* semantics of Phi nodes.                                          */
/********************************************************************/

/**
 * Removes Tuples from Block control flow predecessors.
 * Optimizes blocks with equivalent_node().
 * Replaces n by Bad if n is unreachable control flow.
 */
static void merge_blocks(ir_node *n, void *env) {
  int i;
  set_irn_link(n, NULL);

  if (get_irn_op(n) == op_Block) {
    /* Remove Tuples */
    for (i = 0; i < get_Block_n_cfgpreds(n); i++)
      /* GL @@@ : is this possible? if (get_opt_normalize()) -- added, all tests go through.
       A different order of optimizations might cause problems. */
      if (get_opt_normalize())
	set_Block_cfgpred(n, i, skip_Tuple(get_Block_cfgpred(n, i)));
  } else if (get_opt_optimize() && (get_irn_mode(n) == mode_X)) {
    /* We will soon visit a block.  Optimize it before visiting! */
    ir_node *b = get_nodes_Block(n);
    ir_node *new_node = equivalent_node(b);
    while (irn_not_visited(b) && (!is_Bad(new_node)) && (new_node != b)) {
      /* We would have to run gigo if new is bad, so we
     promote it directly below. */
      assert(((b == new_node) ||
          get_opt_control_flow_straightening() ||
          get_opt_control_flow_weak_simplification()) &&
         ("strange flag setting"));
      exchange (b, new_node);
      b = new_node;
      new_node = equivalent_node(b);
    }
    /* GL @@@ get_opt_normalize hinzugefuegt, 5.5.2003 */
    if (is_Bad(new_node) && get_opt_normalize()) exchange(n, new_Bad());
  }
}

/**
 * Collects all Phi nodes in link list of Block.
 * Marks all blocks "block_visited" if they contain a node other
 * than Jmp.
 */
static void collect_nodes(ir_node *n, void *env) {
  if (is_no_Block(n)) {
    ir_node *b = get_nodes_Block(n);

    if ((get_irn_op(n) == op_Phi)) {
      /* Collect Phi nodes to compact ins along with block's ins. */
      set_irn_link(n, get_irn_link(b));
      set_irn_link(b, n);
    } else if (get_irn_op(n) != op_Jmp) {  /* Check for non empty block. */
      mark_Block_block_visited(b);
    }
  }
}

/** Returns true if pred is predecessor of block. */
static int is_pred_of(ir_node *pred, ir_node *b) {
  int i;
  for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
    ir_node *b_pred = get_nodes_Block(get_Block_cfgpred(b, i));
    if (b_pred == pred) return 1;
  }
  return 0;
}

static int test_whether_dispensable(ir_node *b, int pos) {
  int i, j, n_preds = 1;
  int dispensable = 1;
  ir_node *cfop = get_Block_cfgpred(b, pos);
  ir_node *pred = get_nodes_Block(cfop);

  if (get_Block_block_visited(pred) + 1
      < get_irg_block_visited(current_ir_graph)) {
    if (!get_opt_optimize() || !get_opt_control_flow_strong_simplification()) {
      /* Mark block so that is will not be removed. */
      set_Block_block_visited(pred, get_irg_block_visited(current_ir_graph)-1);
      return 1;
    }
    /* Seems to be empty. */
    if (!get_irn_link(b)) {
      /* There are no Phi nodes ==> dispensable. */
      n_preds = get_Block_n_cfgpreds(pred);
    } else {
      /* b's pred blocks and pred's pred blocks must be pairwise disjunct.
     Work preds < pos as if they were already removed. */
      for (i = 0; i < pos; i++) {
	ir_node *b_pred = get_nodes_Block(get_Block_cfgpred(b, i));
	if (get_Block_block_visited(b_pred) + 1
	    < get_irg_block_visited(current_ir_graph)) {
	  for (j = 0; j < get_Block_n_cfgpreds(b_pred); j++) {
	    ir_node *b_pred_pred = get_nodes_Block(get_Block_cfgpred(b_pred, j));
	    if (is_pred_of(b_pred_pred, pred)) dispensable = 0;
	  }
	} else {
	  if (is_pred_of(b_pred, pred)) dispensable = 0;
	}
      }
      for (i = pos +1; i < get_Block_n_cfgpreds(b); i++) {
	ir_node *b_pred = get_nodes_Block(get_Block_cfgpred(b, i));
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
    pred = get_nodes_Block(get_Block_cfgpred(b, i));
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
      pred = get_nodes_Block(get_Block_cfgpred(b, i));
      if (is_Bad(get_Block_cfgpred(b, i))) {
	/* Do nothing */
      } else if (get_Block_block_visited(pred) +1
         < get_irg_block_visited(current_ir_graph)) {
	/* It's an empty block and not yet visited. */
	ir_node *phi_pred = get_Phi_pred(phi, i);
	for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
	  if (get_nodes_Block(phi_pred) == pred) {
	    assert(get_irn_op(phi_pred) == op_Phi);  /* Block is empty!! */
	    in[n_preds] = get_Phi_pred(phi_pred, j);
	  } else {
	    in[n_preds] = phi_pred;
	  }
	  n_preds++;
	}
	/* The Phi_pred node is replaced now if it is a Phi.
	   In Schleifen kann offenbar der entfernte Phi Knoten legal verwendet werden.
	   Daher muss der Phiknoten durch den neuen ersetzt werden.
	   Weiter muss der alte Phiknoten entfernt werden (durch ersetzen oder
	   durch einen Bad) damit er aus den keep_alive verschwinden kann.
	   Man sollte also, falls keine Schleife vorliegt, exchange mit new_Bad
	   aufrufen.  */
	if (get_nodes_Block(phi_pred) == pred) {
	  /* remove the Phi as it might be kept alive. Further there
	     might be other users. */
	  exchange(phi_pred, phi);  /* geht, ist aber doch semantisch falsch! Warum?? */
	}
      } else {
	in[n_preds] = get_Phi_pred(phi, i);
	n_preds ++;
      }
    }
    /* Fix the node */
    set_irn_in(phi, n_preds, in);

    phi = get_irn_link(phi);
  }

/*-
      This happens only if merge between loop backedge and single loop entry. -*/
  for (k = 0; k < get_Block_n_cfgpreds(b); k++) {
    pred = get_nodes_Block(get_Block_cfgpred(b, k));
    if (get_Block_block_visited(pred)+1 < get_irg_block_visited(current_ir_graph)) {
      phi = get_irn_link(pred);
      while (phi) {
	if (get_irn_op(phi) == op_Phi) {
	  set_nodes_Block(phi, b);

	  n_preds = 0;
	  for (i = 0; i < k; i++) {
	    pred = get_nodes_Block(get_Block_cfgpred(b, i));
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
	    pred = get_nodes_Block(get_Block_cfgpred(b, i));
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
	  set_irn_in(phi, n_preds, in);
	}
	phi = get_irn_link(phi);
      }
    }
  }

  /*- Fix the block -*/
  n_preds = 0;
  for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
    pred = get_nodes_Block(get_Block_cfgpred(b, i));
    if (is_Bad(get_Block_cfgpred(b, i))) {
      /* Do nothing */
    } else if (get_Block_block_visited(pred) +1
           < get_irg_block_visited(current_ir_graph)) {
      /* It's an empty block and not yet visited. */
      assert(get_Block_n_cfgpreds(b) > 1);
                        /* Else it should be optimized by equivalent_node. */
      for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
	in[n_preds] = get_Block_cfgpred(pred, j);
	n_preds++;
      }
      /* Remove block as it might be kept alive. */
      exchange(pred, b/*new_Bad()*/);
    } else {
      in[n_preds] = get_Block_cfgpred(b, i);
      n_preds ++;
    }
  }
  set_irn_in(b, n_preds, in);
  free(in);
}

void optimize_cf(ir_graph *irg) {
  int i;
  ir_node **in;
  ir_node *end = get_irg_end(irg);
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  /* Handle graph state */
  assert(get_irg_phase_state(irg) != phase_building);
  if (get_irg_outs_state(current_ir_graph) == outs_consistent)
    set_irg_outs_inconsistent(current_ir_graph);
  if (get_irg_dom_state(current_ir_graph) == dom_consistent)
    set_irg_dom_inconsistent(current_ir_graph);

  /* Use block visited flag to mark non-empty blocks. */
  inc_irg_block_visited(irg);
  irg_walk(end, merge_blocks, collect_nodes, NULL);

  /* Optimize the standard code. */
  irg_block_walk(get_irg_end_block(irg), optimize_blocks, NULL, NULL);

  /* Walk all keep alives, optimize them if block, add to new in-array
     for end if useful. */
  in = NEW_ARR_F (ir_node *, 1);
  in[0] = get_nodes_Block(end);
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

  current_ir_graph = rem;
}


/**
 * Called by walker of remove_critical_cf_edges().
 *
 * Place an empty block to an edge between a blocks of multiple
 * predecessors and a block of multiple successors.
 *
 * @param n IR node
 * @param env Environment of walker. This field is unused and has
 *            the value NULL.
 */
static void walk_critical_cf_edges(ir_node *n, void *env) {
  int arity, i;
  ir_node *pre, *block, **in, *jmp;

  /* Block has multiple predecessors */
  if ((op_Block == get_irn_op(n)) &&
      (get_irn_arity(n) > 1)) {
    arity = get_irn_arity(n);

    if (n == get_irg_end_block(current_ir_graph))
      return;  /*  No use to add a block here.      */

    for (i=0; i<arity; i++) {
      pre = get_irn_n(n, i);
      /* Predecessor has multiple successors. Insert new flow edge */
      if ((NULL != pre) &&
	(op_Proj == get_irn_op(pre)) &&
	op_Raise != get_irn_op(skip_Proj(pre))) {

	/* set predecessor array for new block */
	in = NEW_ARR_D (ir_node *, current_ir_graph->obst, 1);
	/* set predecessor of new block */
	in[0] = pre;
	block = new_Block(1, in);
	/* insert new jmp node to new block */
	switch_block(block);
	jmp = new_Jmp();
	switch_block(n);
	/* set successor of new block */
	set_irn_n(n, i, jmp);

      } /* predecessor has multiple successors */
    } /* for all predecessors */
  } /* n is a block */
}

void remove_critical_cf_edges(ir_graph *irg) {
  if (get_opt_critical_edges())
    irg_walk_graph(irg, NULL, walk_critical_cf_edges, NULL);
}
