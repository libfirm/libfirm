/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer
**
** Optimizations for a whole ir graph, i.e., a procedure.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <assert.h>

# include "irgopt.h"
# include "irnode_t.h"
# include "irgraph_t.h"
# include "iropt.h"
# include "irgwalk.h"
# include "ircons.h"
# include "misc.h"
# include "irgmod.h"

# include "pset.h"

/* @@@ for testing */
#include "irdump.h"

/* Defined in iropt.c */
pset *new_identities (void);
void  del_identities (pset *value_table);
void  add_identities   (pset *value_table, ir_node *node);

#if 0  /* Warum ist das hier nochmal definiert?
	  Hat's nicht gelinkt wegen typeO tities - tity ?? */
/* To fill the hash table */
void
add_identity (pset *value_table, ir_node *n) {
  /* identify_remember (value_table, n);*/
}
#endif

/********************************************************************/
/* apply optimizations of iropt to all nodes.                       */
/********************************************************************/

void init_link (ir_node *n, void *env) {
  set_irn_link(n, NULL);
}

void
optimize_in_place_wrapper (ir_node *n, void *env) {
  int i;
  ir_node *optimized;

  /* optimize all sons after recursion, i.e., the sons' sons are
     optimized already. */
  for (i = -1; i < get_irn_arity(n); i++) {
    optimized = optimize_in_place(get_irn_n(n, i));
    set_irn_n(n, i, optimized);
  }
}

void
local_optimize_graph (ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  /* Should we clean the value_table in irg for the cse? Better do so... */
  del_identities(irg->value_table);
  irg->value_table = new_identities();

  /* walk over the graph */
  irg_walk(irg->end, init_link, optimize_in_place_wrapper, NULL);

  current_ir_graph = rem;
}

/********************************************************************/
/* Routines for dead node elimination / copying garbage collection  */
/* of the obstack.                                                  */
/********************************************************************/

/* Remeber the new node in the old node by using a field all nodes have. */
inline void
set_new_node (ir_node *old, ir_node *new)
{
  old->link = new;
}

/* Get this new node, before the old node is forgotton.*/
inline ir_node *
get_new_node (ir_node * n)
{
  return n->link;
}


/* We use the block_visited flag to mark that we have computed the
   number of useful predecessors for this block.
   Further we encode the new arity in this flag in the old blocks.
   Remembering the arity is useful, as it saves a lot of pointer
   accesses.  This function is called for all Phi and Block nodes
   in a Block. */
inline int
compute_new_arity(ir_node *b) {
  int i, res;
  int irg_v, block_v;

  irg_v = get_irg_block_visited(current_ir_graph);
  block_v = get_Block_block_visited(b);
  if (block_v >= irg_v) {
    /* we computed the number of preds for this block and saved it in the
       block_v flag */
    return block_v - irg_v;
  } else {
    /* compute the number of good predecessors */
    res = get_irn_arity(b);
    for (i = 0; i < get_irn_arity(b); i++)
      if (get_irn_opcode(get_irn_n(b, i)) == iro_Bad) res--;
    /* save it in the flag. */
    set_Block_block_visited(b, irg_v + res);
    return res;
  }
}

/* Copies the node to the new obstack. The Ins of the new node point to
   the predecessors on the old obstack.  For block/phi nodes not all
   predecessors might be copied.  n->link points to the new node.
   For Phi and Block nodes the function allocates in-arrays with an arity
   only for useful predecessors.  The arity is determined by counting
   the non-bad predecessors of the block. */
void
copy_node (ir_node *n, void *env) {
  ir_node *nn, *block;
  int new_arity;

  if (get_irn_opcode(n) == iro_Block) {
    block = NULL;
    new_arity = compute_new_arity(n);
  } else {
    block = get_nodes_Block(n);
    if (get_irn_opcode(n) == iro_Phi) {
      new_arity = compute_new_arity(block);
    } else {
      new_arity = get_irn_arity(n);
    }
  }
  nn = new_ir_node(current_ir_graph,
		   block,
		   get_irn_op(n),
		   get_irn_mode(n),
		   new_arity,
		   get_irn_in(n));
  /* Copy the attributes.  These might point to additional data.  If this
     was allocated on the old obstack the pointers now are dangling.  This
     frees e.g. the memory of the graph_arr allocated in new_immBlock. */
  copy_attrs(n, nn);
  set_new_node(n, nn);

  /*  printf("\n old node: "); DDMSG2(n);
      printf(" new node: "); DDMSG2(nn); */

}

/* Copies new predecessors of old node to new node remembered in link.
   Spare the Bad predecessors of Phi and Block nodes. */
void
copy_preds (ir_node *n, void *env) {
  ir_node *nn, *block, *on;
  int i, j;

  nn = get_new_node(n);

  /* printf("\n old node: "); DDMSG2(n);
     printf(" new node: "); DDMSG2(nn);
     printf(" arities: old: %d, new: %d\n", get_irn_arity(n), get_irn_arity(nn)); */

  if (get_irn_opcode(n) == iro_Block) {
    /* Don't copy Bad nodes. */
    j = 0;
    for (i = 0; i < get_irn_arity(n); i++)
      if (get_irn_opcode(get_irn_n(n, i)) != iro_Bad) {
	set_irn_n (nn, j, get_new_node(get_irn_n(n, i)));
	j++;
      }
    /* repair the block visited flag from above misuse. Repair it in both
       graphs so that the old one can still be used. */
    set_Block_block_visited(nn, 0);
    set_Block_block_visited(n, 0);
    /* Local optimization could not merge two subsequent blocks if
       in array contained Bads.  Now it's possible.
       @@@ I removed the call to optimize_in_place as it requires
       that the fields in ir_graph are set properly. */
    if (get_Block_n_cfgpreds(nn) == 1
	&& get_irn_op(get_Block_cfgpred(nn, 0)) == op_Jmp)
      exchange(nn, get_nodes_Block(get_Block_cfgpred(nn, 0)));
  } else if (get_irn_opcode(n) == iro_Phi) {
    /* Don't copy node if corresponding predecessor in block is Bad.
       The Block itself should not be Bad. */
    block = get_nodes_Block(n);
    set_irn_n (nn, -1, get_new_node(block));
    j = 0;
    for (i = 0; i < get_irn_arity(n); i++)
      if (get_irn_opcode(get_irn_n(block, i)) != iro_Bad) {
	set_irn_n (nn, j, get_new_node(get_irn_n(n, i)));
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
    for (i = -1; i < get_irn_arity(n); i++)
      set_irn_n (nn, i, get_new_node(get_irn_n(n, i)));
  }
  /* Now the new node is complete.  We can add it to the hash table for cse. */
  /* add_identity (current_ir_graph->value_table, nn); */
  add_identities (current_ir_graph->value_table, nn);
}

/* Copies the graph reachable from current_ir_graph->end to the obstack
   in current_ir_graph.
   Then fixes the fields in current_ir_graph containing nodes of the
   graph.  */
void
copy_graph () {
  /* Not all nodes remembered in current_ir_graph might be reachable
     from the end node.  Assure their link is set to NULL, so that
     we can test whether new nodes have been computed. */
  set_irn_link(get_irg_frame  (current_ir_graph), NULL);
  set_irn_link(get_irg_globals(current_ir_graph), NULL);
  set_irn_link(get_irg_args   (current_ir_graph), NULL);

  /* we use the block walk flag for removing Bads from Blocks ins. */
  inc_irg_block_visited(current_ir_graph);

  /* copy the graph */
  irg_walk(get_irg_end(current_ir_graph), copy_node, copy_preds, NULL);

  /* fix the fields in current_ir_graph */
  set_irg_end        (current_ir_graph, get_new_node(get_irg_end(current_ir_graph)));
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
}

/* Copies all reachable nodes to a new obstack.  Removes bad inputs
   from block nodes and the corresponding inputs from Phi nodes.
   Merges single exit blocks with single entry blocks and removes
   1-input Phis.
   Adds all new nodes to a new hash table for cse.  Does not
   perform cse, so the hash table might contain common subexpressions. */
/* Amroq call this emigrate() */
void
dead_node_elimination(ir_graph *irg) {
  ir_graph *rem;
  struct obstack *graveyard_obst = NULL;
  struct obstack *rebirth_obst   = NULL;

  /* Remember external state of current_ir_graph. */
  rem = current_ir_graph;
  current_ir_graph = irg;

  if (get_optimize() && get_opt_dead_node_elimination()) {

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
    copy_graph();

    /* Free memory from old unoptimized obstack */
    obstack_free(graveyard_obst, 0);  /* First empty the obstack ... */
    xfree (graveyard_obst);           /* ... then free it.           */
  }

  current_ir_graph = rem;
}

/**********************************************************************/
/*  Funcionality for inlining                                         */
/**********************************************************************/

void inline_method(ir_node *call, ir_graph *called_graph) {
  ir_node *pre_call;
  ir_node *post_call, *post_bl;
  ir_node *in[5];
  ir_node *end, *end_bl;
  ir_node **res_pred;
  ir_node **cf_pred;
  ir_node *ret, *phi;
  ir_node *cf_op, *bl;
  int arity, n_ret, n_exc, n_res, i, j;

  if (!get_opt_inline()) return;

  /** Check preconditions **/
  assert(get_irn_op(call) == op_Call);
  assert(get_Call_type(call) == get_entity_type(get_irg_ent(called_graph)));
  assert(get_type_tpop(get_Call_type(call)) == type_method);
  if (called_graph == current_ir_graph) return;

  /** Part the Call node into two nodes.  Pre_call collects the parameters of
  the procedure and later replaces the Start node of the called graph.
  Post_call is the old Call node and collects the results of the called
  graph. Both will end up being a tuple.  **/
  post_bl = get_nodes_Block(call);
  set_irg_current_block(current_ir_graph, post_bl);
  /* XxMxPxP von Start + Parameter von Call */
  in[0] = new_Jmp();
  in[1] = get_Call_mem(call);
  in[2] = get_irg_frame(current_ir_graph);
  in[3] = get_irg_globals(current_ir_graph);
  in[4] = new_Tuple (get_Call_n_params(call),
		     get_Call_param_arr(call));
  pre_call = new_Tuple(5, in);
  post_call = call;

  /** Part the block of the Call node into two blocks.
      The new block gets the ins of the old block, pre_call and all its
      predecessors and all Phi nodes. **/
  part_block(pre_call);

  /** Prepare state for dead node elimination **/
  /* Visited flags in calling irg must be >= flag in called irg.
     Else walker and arity computation will not work. */
  if (get_irg_visited(current_ir_graph) <= get_irg_visited(called_graph))
    set_irg_visited(current_ir_graph, get_irg_visited(called_graph)+1);   /***/
  if (get_irg_block_visited(current_ir_graph)< get_irg_block_visited(called_graph))
    set_irg_block_visited(current_ir_graph, get_irg_block_visited(called_graph));
  /* Set pre_call as new Start node in link field of the start node of
      calling graph and pre_calls block as new block for the start block
      of calling graph.
      Further mark these nodes so that they are not visited by the
      copying. */
  set_irn_link(get_irg_start(called_graph), pre_call);
  set_irn_visited(get_irg_start(called_graph),
		  get_irg_visited(current_ir_graph));/***/
  set_irn_link(get_irg_start_block(called_graph),
	       get_nodes_Block(pre_call));
  set_irn_visited(get_irg_start_block(called_graph),
		  get_irg_visited(current_ir_graph));  /***/

  /* Initialize for compaction of in arrays */
  inc_irg_block_visited(current_ir_graph);
  /*
  set_Block_block_visited(get_irg_start_block(called_graph),
			get_irg_block_visited(current_ir_graph) +1 +1); /* count for self edge */

  /*** Replicate local entities of the called_graph ***/
  /* @@@ */

  /* visited is > than that of called graph.  With this trick visited will
     remain unchanged so that an outer walker calling this inline will
     not visit the inlined nodes. */
  set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph)-1);

  /** Performing dead node elimination inlines the graph **/
  /* Copies the nodes to the obstack of current_ir_graph. */
  /* @@@ endless loops are not copied!! */
  irg_walk(get_irg_end(called_graph), copy_node, copy_preds, NULL);

  /* Repair called_graph */
  set_irg_visited(called_graph, get_irg_visited(current_ir_graph));
  set_irg_block_visited(called_graph, get_irg_block_visited(current_ir_graph));
  set_Block_block_visited(get_irg_start_block(called_graph), 0);

  /*** Merge the end of the inlined procedure with the call site ***/

  /** Precompute some values **/
  end_bl = get_new_node(get_irg_end_block(called_graph));
  end = get_new_node(get_irg_end(called_graph));
  arity = get_irn_arity(end_bl);    /* arity = n_exc + n_ret  */
  n_res = get_method_n_res(get_Call_type(call));

  res_pred = (ir_node **) malloc ((n_res) * sizeof (ir_node *));
  cf_pred = (ir_node **) malloc (arity * sizeof (ir_node *));

  set_irg_current_block(current_ir_graph, post_bl); /* just to make sure */


  /** Collect control flow from Return blocks to post_calls block. Replace
      Return nodes by Jump nodes. **/
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

  /** Collect results from Return nodes to post_call. Post_call is
      turned into a tuple. **/
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
  set_Tuple_pred(call, 0, phi);
  set_irn_link(phi, get_irn_link(post_bl));  /* Conserve Phi-list for further inlinings */
  set_irn_link(post_bl, phi);
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
      set_irn_link(phi, get_irn_link(post_bl));  /* Conserve Phi-list for further inlinings */
      set_irn_link(post_bl, phi);
    }
    set_Tuple_pred(call, 2, new_Tuple(n_res, res_pred));
  } else {
    set_Tuple_pred(call, 2, new_Bad());
  }
  /* Finally the exception control flow.  We need to add a Phi node to
     collect the memory containing the exception objects.  Further we need
     to add another block to get a correct representation of this Phi.  To
     this block we add a Jmp that resolves into the X output of the Call
     when the Call is turned into a tuple. */
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
    new_Block(n_exc, cf_pred);      /* whatch it: current_block is changed! */
    set_Tuple_pred(call, 1, new_Jmp());
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
    set_Tuple_pred(call, 3, new_Phi(n_exc, cf_pred, mode_M));
  } else {
    set_Tuple_pred(call, 1, new_Bad());
    set_Tuple_pred(call, 3, new_Bad());
  }
  free(res_pred);
  free(cf_pred);

  /*** Correct the control flow to the end node.
       If the exception control flow from the Call directly branched to the
       end block we now have the following control flow predecessor pattern:
       ProjX -> Tuple -> Jmp.
       We must remove the Jmp along with it's empty block and add Jmp's
       predecessors as predecessors of this end block. ***/
  /* find the problematic predecessor of the end block. */
  end_bl = get_irg_end_block(current_ir_graph);
  for (i = 0; i < get_Block_n_cfgpreds(end_bl); i++) {
    cf_op = get_Block_cfgpred(end_bl, i);
    if (get_irn_op(cf_op) == op_Proj) {
      cf_op = get_Proj_pred(cf_op);
      if (get_irn_op(cf_op) == op_Tuple) {
	cf_op = get_Tuple_pred(cf_op, 1);
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
  }
}
