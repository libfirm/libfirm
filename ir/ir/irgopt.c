/* Coyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer
**
** Optimizations for a whole ir graph, i.e., a procedure.
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <assert.h>

# include "irprog.h"
# include "irgopt.h"
# include "irnode_t.h"
# include "irgraph_t.h"
# include "iropt_t.h"
# include "irgwalk.h"
# include "ircons.h"
# include "misc.h"
# include "irgmod.h"
# include "array.h"
# include "pset.h"
# include "pdeq.h"       /* Fuer code placement */
# include "irouts.h"

/* Defined in iropt.c */
pset *new_identities (void);
void  del_identities (pset *value_table);
void  add_identities   (pset *value_table, ir_node *node);

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

  for (i = 0; i < get_irn_arity(n); i++) {
    optimized = optimize_in_place_2(get_irn_n(n, i));
    set_irn_n(n, i, optimized);
  }

  if (get_irn_op(n) == op_Block) {
    optimized = optimize_in_place_2(n);
    if (optimized != n) exchange (n, optimized);
  }
}

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
  set_new_node(n, nn);

  /*  printf("\n old node: "); DDMSG2(n);
      printf(" new node: "); DDMSG2(nn); */

}

/* Copies new predecessors of old node to new node remembered in link.
   Spare the Bad predecessors of Phi and Block nodes. */
void
copy_preds (ir_node *n, void *env) {
  ir_node *nn, *block;
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
       We don't call optimize_in_place as it requires
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
  /* Now the new node is complete.  We can add it to the hash table for cse.
     @@@ inlinening aborts if we identify End. Why? */
  if(get_irn_op(nn) != op_End)
    add_identities (current_ir_graph->value_table, nn);
}

/* Copies the graph recursively, compacts the keepalive of the end node. */
void
copy_graph () {
  ir_node *oe, *ne; /* old end, new end */
  ir_node *ka;      /* keep alive */
  int i;

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

  /** ... and now the keep alives. **/
  /* First pick the not marked block nodes and walk them.  We must pick these
     first as else we will oversee blocks reachable from Phis. */
  for (i = 0; i < get_irn_arity(oe); i++) {
    ka = get_irn_n(oe, i);
    if ((get_irn_op(ka) == op_Block) &&
	(get_irn_visited(ka) < get_irg_visited(current_ir_graph))) {
      /* We must keep the block alive and copy everything reachable */
      set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph)-1);
      irg_walk(ka, copy_node, copy_preds, NULL);
      add_End_keepalive(ne, get_new_node(ka));
    }
  }

  /* Now pick the Phis.  Here we will keep all! */
  for (i = 0; i < get_irn_arity(oe); i++) {
    ka = get_irn_n(oe, i);
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

/* Copies the graph reachable from current_ir_graph->end to the obstack
   in current_ir_graph and fixes the environment.
   Then fixes the fields in current_ir_graph containing nodes of the
   graph.  */
void
copy_graph_env () {
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
  free_End(get_irg_end(current_ir_graph));
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
  if (get_irn_link(get_irg_unknown(current_ir_graph)) == NULL) {
    copy_node(get_irg_unknown(current_ir_graph), NULL);
    copy_preds(get_irg_unknown(current_ir_graph), NULL);
  }
  set_irg_unknown(current_ir_graph, get_new_node(get_irg_unknown(current_ir_graph)));
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

  /* Handle graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  free_outs(current_ir_graph);

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
    copy_graph_env();

    /* Free memory from old unoptimized obstack */
    obstack_free(graveyard_obst, 0);  /* First empty the obstack ... */
    xfree (graveyard_obst);           /* ... then free it.           */
  }

  current_ir_graph = rem;
}

/**********************************************************************/
/*  Funcionality for inlining                                         */
/**********************************************************************/

/* Copy node for inlineing.  Copies the node by calling copy_node and
   then updates the entity if it's a local one.  env must be a pointer
   to the frame type of the procedure. The new entities must be in
   the link field of the entities. */
void
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
  }
}

void inline_method(ir_node *call, ir_graph *called_graph) {
  ir_node *pre_call;
  ir_node *post_call, *post_bl;
  ir_node *in[5];
  ir_node *end, *end_bl;
  ir_node **res_pred;
  ir_node **cf_pred;
  ir_node *ret, *phi;
  ir_node *cf_op, *bl;
  int arity, n_ret, n_exc, n_res, i, j, rem_opt;
  type *called_frame;

  if (!get_optimize() || !get_opt_inline()) return;
  /** Turn off optimizations, this can cause problems when allocating new nodes. **/
  rem_opt = get_optimize();
  set_optimize(0);

  /* Handle graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  assert(get_irg_pinned(current_ir_graph) == pinned);
  assert(get_irg_pinned(called_graph) == pinned);
  if (get_irg_outs_state(current_ir_graph) == outs_consistent)
    set_irg_outs_inconsistent(current_ir_graph);

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
  /* XxMxPxP of Start + parameter of Call */
  in[0] = new_Jmp();
  in[1] = get_Call_mem(call);
  in[2] = get_irg_frame(current_ir_graph);
  in[3] = get_irg_globals(current_ir_graph);
  in[4] = new_Tuple (get_Call_n_params(call), get_Call_param_arr(call));
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
  /* copy the entities. */
  called_frame = get_irg_frame_type(called_graph);
  for (i = 0; i < get_class_n_member(called_frame); i++) {
    entity *new_ent, *old_ent;
    old_ent = get_class_member(called_frame, i);
    new_ent = copy_entity_own(old_ent, get_cur_frame_type());
    set_entity_link(old_ent, new_ent);
  }

  /* visited is > than that of called graph.  With this trick visited will
     remain unchanged so that an outer walker, e.g., searching the call nodes
     to inline, calling this inline will not visit the inlined nodes. */
  set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph)-1);

  /** Performing dead node elimination inlines the graph **/
  /* Copies the nodes to the obstack of current_ir_graph. Updates links to new
     entities. */
  /* @@@ endless loops are not copied!! */
  irg_walk(get_irg_end(called_graph), copy_node_inline, copy_preds,
	   get_irg_frame_type(called_graph));

  /* Repair called_graph */
  set_irg_visited(called_graph, get_irg_visited(current_ir_graph));
  set_irg_block_visited(called_graph, get_irg_block_visited(current_ir_graph));
  set_Block_block_visited(get_irg_start_block(called_graph), 0);

  /*** Merge the end of the inlined procedure with the call site ***/
  /* We will turn the old Call node into a Tuple with the following
     predecessors:
     -1:  Block of Tuple.
     0: Phi of all Memories of Return statements.
     1: Jmp from new Block that merges the control flow from all exception
	 predecessors of the old end block.
     2: Tuple of all arguments.
     3: Phi of Exception memories.
  */

  /** Precompute some values **/
  end_bl = get_new_node(get_irg_end_block(called_graph));
  end = get_new_node(get_irg_end(called_graph));
  arity = get_irn_arity(end_bl);    /* arity = n_exc + n_ret  */
  n_res = get_method_n_res(get_Call_type(call));

  res_pred = (ir_node **) malloc (n_res * sizeof (ir_node *));
  cf_pred = (ir_node **) malloc (arity * sizeof (ir_node *));

  set_irg_current_block(current_ir_graph, post_bl); /* just to make sure */

  /** archive keepalives **/
  for (i = 0; i < get_irn_arity(end); i++)
    add_End_keepalive(get_irg_end(current_ir_graph), get_irn_n(end, i));
  /* The new end node will die, but the in array is not on the obstack ... */
  free_End(end);

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

  /** Turn cse back on. **/
  set_optimize(rem_opt);
}

/********************************************************************/
/* Apply inlineing to small methods.                                */
/********************************************************************/

static int pos;

/* It makes no sense to inline too many calls in one procedure. Anyways,
   I didn't get a version with NEW_ARR_F to run. */
#define MAX_INLINE 1024

static void collect_calls(ir_node *call, void *env) {
  ir_node **calls = (ir_node **)env;
  ir_node *addr;
  tarval *tv;
  ir_graph *called_irg;

  if (get_irn_op(call) != op_Call) return;

  addr = get_Call_ptr(call);
  if (get_irn_op(addr) == op_Const) {
    /* Check whether the constant is the pointer to a compiled entity. */
    tv = get_Const_tarval(addr);
    if (tv->u.p.ent) {
      called_irg = get_entity_irg(tv->u.p.ent);
      if (called_irg && pos < MAX_INLINE) {
	/* The Call node calls a locally defined method.  Remember to inline. */
	calls[pos] = call;
	pos++;
      }
    }
  }
}


/* Inlines all small methods at call sites where the called address comes
   from a Const node that references the entity representing the called
   method.
   The size argument is a rough measure for the code size of the method:
   Methods where the obstack containing the firm graph is smaller than
   size are inlined. */
void inline_small_irgs(ir_graph *irg, int size) {
  int i;
  ir_node *calls[MAX_INLINE];
  ir_graph *rem = current_ir_graph;

  if (!(get_optimize() && get_opt_inline())) return;

  /*DDME(get_irg_ent(current_ir_graph));*/

  current_ir_graph = irg;
  /* Handle graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);

  /* Find Call nodes to inline.
     (We can not inline during a walk of the graph, as inlineing the same
     method several times changes the visited flag of the walked graph:
     after the first inlineing visited of the callee equals visited of
     the caller.  With the next inlineing both are increased.) */
  pos = 0;
  irg_walk(get_irg_end(irg), NULL, collect_calls, (void *) calls);

  if ((pos > 0) && (pos < MAX_INLINE)) {
    /* There are calls to inline */
    collect_phiprojs(irg);
    for (i = 0; i < pos; i++) {
      tarval *tv;
      ir_graph *callee;
      tv = get_Const_tarval(get_Call_ptr(calls[i]));
      callee = get_entity_irg(tv->u.p.ent);
      if ((_obstack_memory_used(callee->obst) - obstack_room(callee->obst)) < size) {
	/*printf(" inlineing "); DDME(tv->u.p.ent);*/
	inline_method(calls[i], callee);
      }
    }
  }

  current_ir_graph = rem;
}


/********************************************************************/
/*  Code Placement.  Pinns all floating nodes to a block where they */
/*  will be executed only if needed.                                */
/********************************************************************/

static pdeq *worklist;		/* worklist of ir_node*s */

/* Find the earliest correct block for N.  --- Place N into the
   same Block as its dominance-deepest Input.  */
static void
place_floats_early (ir_node *n)
{
  int i, start;

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
	(is_Bad(n))) {
      /* These nodes will not be placed by the loop below. */
      b = get_irg_start_block(current_ir_graph);
      depth = 1;
    }

    /* find the block for this node. */
    for (i = 0; i < get_irn_arity(n); i++) {
      ir_node *dep = get_irn_n(n, i);
      ir_node *dep_block;
      if ((irn_not_visited(dep)) &&
	  (get_op_pinned(get_irn_op(dep)) == floats)) {
	place_floats_early (dep);
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
  for (i = start; i < get_irn_arity(n); i++) {
    ir_node *pred = get_irn_n(n, i);
    if (irn_not_visited(pred)) {
      pdeq_putr (worklist, pred);
    }
  }
}

/* Floating nodes form subgraphs that begin at nodes as Const, Load,
   Start, Call and end at pinned nodes as Store, Call.  Place_early
   places all floating nodes reachable from its argument through floating
   nodes and adds all beginnings at pinned nodes to the worklist. */
inline void place_early () {
  assert(worklist);
  inc_irg_visited(current_ir_graph);

  /* this inits the worklist */
  place_floats_early (get_irg_end(current_ir_graph));

  /* Work the content of the worklist. */
  while (!pdeq_empty (worklist)) {
    ir_node *n = pdeq_getl (worklist);
    if (irn_not_visited(n)) place_floats_early (n);
  }

  set_irg_outs_inconsistent(current_ir_graph);
  current_ir_graph->pinned = pinned;
}


/* deepest common dominance ancestor of DCA and CONSUMER of PRODUCER */
static ir_node *
consumer_dom_dca (ir_node *dca, ir_node *consumer, ir_node *producer)
{
  ir_node *block;

  /* Compute the latest block into which we can place a node so that it is
     before consumer. */
  if (get_irn_op(consumer) == op_Phi) {
    /* our comsumer is a Phi-node, the effective use is in all those
       blocks through which the Phi-node reaches producer */
    int i;
    ir_node *phi_block = get_nodes_Block(consumer);
    for (i = 0;  i < get_irn_arity(consumer); i++) {
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

#if 0
/* @@@ Needs loop informations.  Will implement later interprocedural. */
static void
move_out_of_loops (ir_node *n, ir_node *dca)
{
  assert(dca);

  /* Find the region deepest in the dominator tree dominating
     dca with the least loop nesting depth, but still dominated
     by our early placement. */
  ir_node *best = dca;
  while (dca != get_nodes_Block(n)) {
    dca = get_Block_idom(dca);
    if (!dca) break; /* should we put assert(dca)? */
    if (get_Block_loop_depth(dca) < get_Block_loop_depth(best)) {
      best = dca;
    }
  }
  if (get_Block_dom_depth(best) >= get_Block_dom_depth(get_nodes_Block(n)))
    set_nodes_Block(n, best);
}
#endif

/* Find the latest legal block for N and place N into the
   `optimal' Block between the latest and earliest legal block.
   The `optimal' block is the dominance-deepest block of those
   with the least loop-nesting-depth.  This places N out of as many
   loops as possible and then makes it as controldependant as
   possible. */
static void
place_floats_late (ir_node *n)
{
  int i;

  assert (irn_not_visited(n)); /* no multiple placement */

  /* no need to place block nodes, control nodes are already placed. */
  if ((get_irn_op(n) != op_Block) && (!is_cfop(n)) && (get_irn_mode(n) != mode_X)) {
    /* Assure that our users are all placed, except the Phi-nodes.
       --- Each dataflow cycle contains at least one Phi-node.  We
       have to break the `user has to be placed before the
       producer' dependance cycle and the Phi-nodes are the
       place to do so, because we need to base our placement on the
       final region of our users, which is OK with Phi-nodes, as they
       are pinned, and they never have to be placed after a
       producer of one of their inputs in the same block anyway. */
    for (i = 0; i < get_irn_n_outs(n); i++) {
      ir_node *succ = get_irn_out(n, i);
      if (irn_not_visited(succ) && (get_irn_op(succ) != op_Phi))
	place_floats_late (succ);
    }

    /* We have to determine the final block of this node... except for constants. */
    if ((get_op_pinned(get_irn_op(n)) == floats) &&
	(get_irn_op(n) != op_Const) &&
	(get_irn_op(n) != op_SymConst)) {
      ir_node *dca = NULL;	/* deepest common ancestor in the
				   dominator tree of all nodes'
				   blocks depending on us; our final
				   placement has to dominate DCA. */
      for (i = 0; i < get_irn_n_outs(n); i++) {
	dca = consumer_dom_dca (dca, get_irn_out(n, i), n);
      }
      set_nodes_Block(n, dca);
#if 0
      move_out_of_loops (n, dca);
#endif
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

inline void place_late() {
  assert(worklist);
  inc_irg_visited(current_ir_graph);

  /* This fills the worklist initially. */
  place_floats_late(get_irg_start_block(current_ir_graph));
  /* And now empty the worklist again... */
  while (!pdeq_empty (worklist)) {
    ir_node *n = pdeq_getl (worklist);
    if (irn_not_visited(n)) place_floats_late(n);
  }
}

void place_code(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  if (!(get_optimize() && get_opt_global_cse())) return;

  /* Handle graph state */
  assert(get_irg_phase_state(irg) != phase_building);
  if (get_irg_dom_state(irg) != dom_consistent)
    compute_doms(irg);

  /* Place all floating nodes as early as possible. This guarantees
     a legal code placement. */
  worklist = new_pdeq ();
  place_early();

  /* place_early invalidates the outs, place_late needs them. */
  compute_outs(irg);
  /* Now move the nodes down in the dominator tree. This reduces the
     unnecessary executions of the node. */
  place_late();

  set_irg_outs_inconsistent(current_ir_graph);
  del_pdeq (worklist);
  current_ir_graph = rem;
}



/********************************************************************/
/* Control flow optimization.                                       */
/* Removes Bad control flow predecessors and empty blocks.  A block */
/* is empty if it contains only a Jmp node.                         */
/* */
/********************************************************************/


static void merge_blocks(ir_node *n, void *env) {
  int i;
  set_irn_link(n, NULL);

  if (get_irn_op(n) == op_Block) {
    /* Remove Tuples */
    for (i = 0; i < get_Block_n_cfgpreds(n); i++)
      set_Block_cfgpred(n, i, skip_Tuple(get_Block_cfgpred(n, i)));
  } else if (get_irn_mode(n) == mode_X) {
    /* We will soon visit a block.  Optimize it before visiting! */
    ir_node *b = get_nodes_Block(n);
    ir_node *new = equivalent_node(b);
    while (irn_not_visited(b) && (!is_Bad(new)) && (new != b)) {
      /* We would have to run gigo if new is bad. */
      if (get_optimize() && get_opt_control_flow()) exchange (b, new);
      b = new;
      new = equivalent_node(b);
    }
    if (is_Bad(new)) exchange (n, new_Bad());
  }
}

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

/* Returns true if pred is pred of block */
int is_pred_of(ir_node *pred, ir_node *b) {
  int i;
  for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
    ir_node *b_pred = get_nodes_Block(get_Block_cfgpred(b, i));
    if (b_pred == pred) return 1;
  }
  return 0;
}

int test_whether_dispensable(ir_node *b, int pos) {
  int i, j, n_preds = 1;
  int dispensable = 1;
  ir_node *cfop = get_Block_cfgpred(b, pos);
  ir_node *pred = get_nodes_Block(cfop);

  if (get_Block_block_visited(pred) + 1
      < get_irg_block_visited(current_ir_graph)) {
    if (!get_optimize() || !get_opt_control_flow()) {
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

void optimize_blocks(ir_node *b, void *env) {
  int i, j, k, max_preds, n_preds;
  ir_node *pred, *phi;
  ir_node **in;

  max_preds = 0;
  for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
    pred = get_Block_cfgpred(b, i);
    max_preds += test_whether_dispensable(b, i);
  }
  in = (ir_node **) malloc(max_preds * sizeof(ir_node *));


  /** Debug output **
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
 ** end Debug output **/

  /** Fix the Phi nodes **/
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
#if 0
	/* @@@ hier brauche ich schleifeninformation!!! Wenn keine Rueckwaertskante
	   dann darfs auch keine Verwendung geben. */
	if (get_nodes_Block(phi_pred) == pred) {
	  /* remove the Phi as it might be kept alive. Further there
	     might be other users. */
	  exchange(phi_pred, phi);  /* geht, is aber doch semantisch falsch! */
	}
#endif
      } else {
	in[n_preds] = get_Phi_pred(phi, i);
	n_preds ++;
      }
    }
    /* Fix the node */
    set_irn_in(phi, n_preds, in);
    phi = get_irn_link(phi);
  }

  /** Move Phi nodes from removed blocks to this one.
      This happens only if merge between loop backedge and single loop entry. **/
  for (k = 0; k < get_Block_n_cfgpreds(b); k++) {
    pred = get_nodes_Block(get_Block_cfgpred(b, k));
    if (get_Block_block_visited(pred) +1
	< get_irg_block_visited(current_ir_graph)) {
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
		/* @@@ Hier brauche ich schleifeninformation!!! Kontrllflusskante
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

  /** Fix the block **/
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

  //DDME(get_irg_ent(irg));

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
    if (irn_not_visited(ka))
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
  /* DEL_ARR_F(end->in);   GL @@@ tut nicht ! */
  end->in = in;

  current_ir_graph = rem;
}
