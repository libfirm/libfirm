/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer
**
** Optimizations for a whole ir graph, i.e., a procedure.
*/

# include <assert.h>

# include "irgopt.h"
# include "irnode_t.h"
# include "irgraph_t.h"
# include "iropt.h"
# include "irgwalk.h"
# include "ircons.h"
# include "misc.h"

/********************************************************************/
/* apply optimizations of iropt to all nodes.                       */
/********************************************************************/

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

  /* walk over the graph */
  irg_walk(irg->end, NULL, optimize_in_place_wrapper, NULL);

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
   Further we encode the new arity in this flag.  Remembering the arity is useful,
   as it saves a lot of pointer accesses.  This function is called for all
   Phi and Block nodes in a Block. */
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
   the predecessors on the old obstack.  n->link points to the new node.
   For Phi and Block nodes the function allocate in arrays with an arity
   only for useful predecessors.  The arity is determined by counting
   the non-bad predecessors of the block. */
inline void
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
  copy_attrs(n, nn);
  set_new_node(n, nn);
}

/* Copies new predecessors of old node to new node remembered in link.
   Spare the Bad predecessors of Phi and Block nodes. */
inline void
copy_preds (ir_node *n, void *env) {
  ir_node *nn, *block;
  int i, j;

  nn = get_new_node(n);

  if (get_irn_opcode(n) == iro_Block) {
    /* Don't copy Bad nodes. */
    j = 0;
    for (i = 0; i < get_irn_arity(n); i++)
      if (get_irn_opcode(get_irn_n(n, i)) != iro_Bad) {
	set_irn_n (nn, j, get_new_node(get_irn_n(n, i)));
	j++;
      }
    /* repair the block visited flag from above misuse */
    set_Block_block_visited(nn, 0);
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
  } else {
    for (i = -1; i < get_irn_arity(n); i++)
      set_irn_n (nn, i, get_new_node(get_irn_n(n, i)));
  }
}

/* Copies the graph reachable from current_ir_graph->end to the obstack
   in current_ir_graph.
   Then fixes the fields in current_ir_graph containing nodes of the
   graph.  */
void
copy_graph () {
  /* Not all nodes remembered in current_ir_graph might be reachable
     from the end node.  Assure their link is set to NULL so that
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
  if (get_irn_link(get_irg_frame(current_ir_graph)) == NULL)
    irg_walk(get_irg_frame(current_ir_graph), copy_node, copy_preds, NULL);
  if (get_irn_link(get_irg_globals(current_ir_graph)) == NULL)
    irg_walk(get_irg_globals(current_ir_graph), copy_node, copy_preds, NULL);
  if (get_irn_link(get_irg_args(current_ir_graph)) == NULL)
    irg_walk(get_irg_args(current_ir_graph), copy_node, copy_preds, NULL);
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

    /* Copy the graph from the old to the new obstack */
    copy_graph();

    /* Free memory from old unoptimized obstack */
    obstack_free(graveyard_obst, 0);  /* First empty the obstack ... */
    xfree (graveyard_obst);           /* ... then free it.           */
  }

  current_ir_graph = rem;
}





#if 0  /* An old implementation */

/* To break the recursion of the graph walk if there are loops in
   the graph we have to allocate new nodes for Phis and blocks
   before descending.  Here we use the old predecessors for the
   new nodes.  These are replaced by the proper predecessors in
   copy_node.
   It turned out that it is not sufficient to just break loops
   for Phi and Block nodes, as the walker can hit visited but
   not copied nodes at any point in the graph.
   A simple fix would be allocating Id's for every node and then
   exchanging them, but this will cause new dead nodes on the new
   obstack.
   So now there is a different implementation more based on the
   view on the graph as a graph than as a represented program. */
void
create_dummy (ir_node *n, void *env) {
  assert (n);

  /* Assure link is set to NULL so we can test whether there is a
     new node by checking link.
     set_irn_link(n, NULL); */

  switch (get_irn_opcode(n)) {
  case iro_Block:
      set_new_node(n, new_ir_node(current_ir_graph, NULL, op_Block, mode_R,
				  get_irn_arity(n), get_irn_in(n)));
    break;
  case iro_Phi:
      set_new_node(n, new_ir_node(current_ir_graph, NULL, op_Phi,
				  get_irn_mode(n),
				  get_irn_arity(n), get_irn_in(n)));
    break;
  default: {}
  } /* end switch (get_irn_opcode(n)) */
}

/* Create a copy of this node on a new obstack. */
void
copy_node2 (ir_node *n, void *env) {
  ir_node *res = NULL;
  ir_node *a = NULL;
  ir_node *b = NULL;
  int i = 0;

  assert (n);
  DDMSG2(n);

  if (is_binop(n)) {
    a = get_binop_left(n);
    b = get_binop_right(n);
  } else if (is_unop(n)) {
    a = get_unop_op(n);
  }

  switch (get_irn_opcode(n)) {
  case iro_Block:
    {
      res = get_new_node(n);
      assert(res);
      for (i = 0; i < get_Block_n_cfgpreds(n); i++)
	set_Block_cfgpred(res, i, get_new_node(get_Block_cfgpred(n, i)));
      set_Block_matured(res, 1);
    }
    break;
  case iro_Start:
    res = new_r_Start (current_ir_graph, get_new_node(get_nodes_Block(n)));
    break;
  case iro_End:
    res = new_r_End (current_ir_graph, get_new_node(get_nodes_Block(n)));
    current_ir_graph -> end = res;
    current_ir_graph -> end_block = get_nodes_Block(res);
    break;
  case iro_Jmp:
    res = new_r_Jmp (current_ir_graph, get_new_node(get_nodes_Block(n)));
    break;
  case iro_Cond:
    res = new_r_Cond (current_ir_graph, get_new_node(get_nodes_Block(n)),
		      get_new_node(get_Cond_selector(n)));
    break;
  case iro_Return:
    {
      ir_node **in;
      in = get_Return_res_arr(n);
      for (i = 0; i < get_Return_n_res(n); i++)
	set_Return_res(n, i, get_new_node(get_Return_res(n, i)));
      res = new_r_Return (current_ir_graph,
			  get_new_node(get_nodes_Block(n)),
			  get_new_node(get_Return_mem(n)),
			  get_Return_n_res(n), in);
    }
    break;
  case iro_Raise:
    res = new_r_Raise (current_ir_graph,
		       get_new_node(get_nodes_Block(n)),
		       get_new_node(get_Raise_mem(n)),
		       get_new_node(get_Raise_exo_ptr(n)));
    break;
  case iro_Const:
    res = new_r_Const (current_ir_graph, get_new_node(get_nodes_Block(n)),
		       get_irn_mode(n), get_Const_tarval(n));
    break;
  case iro_SymConst:
    {
      type_or_id_p value = NULL;

      if ((get_SymConst_kind(n)==type_tag) || (get_SymConst_kind(n)==size))
	{

	   value = (type_or_id_p) get_SymConst_type(n);
	}
      else
	{
	  if (get_SymConst_kind(n)==linkage_ptr_info)
	  {
	    value = (type_or_id_p) get_SymConst_ptrinfo(n);
	  }
	}
    res = new_r_SymConst (current_ir_graph, get_new_node(get_nodes_Block(n)),
			  value, get_SymConst_kind (n));
    }
    break;
  case iro_Sel:
    {
      ir_node **in = get_Sel_index_arr(n);
      for (i = 0; i < get_Sel_n_index(n); i++)
	set_Sel_index(n, i, get_new_node(get_Sel_index(n, i)));
      res = new_r_Sel (current_ir_graph, get_new_node(get_nodes_Block(n)),
		       get_new_node(get_Sel_mem(n)),
		       get_new_node(get_Sel_ptr(n)), get_Sel_n_index(n),
		       in, get_Sel_entity(n));
    }
    break;
  case  iro_Call:
    {
      ir_node **in;
      in = get_Call_param_arr(n);

      for (i = 0; i < get_Call_arity(n); i++)
	set_Call_param(n, i, get_new_node(get_Call_param(n, i)));
      res = new_r_Call (current_ir_graph,
			get_new_node(get_nodes_Block(n)),
			get_new_node(get_Call_mem(n)),
			get_new_node(get_Call_ptr(n)),
			get_Call_arity(n), in,
			get_Call_type (n));
    }
    break;
  case iro_Add:
    res = new_r_Add (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(a), get_new_node(b), get_irn_mode(n));
    break;
  case iro_Sub:
    {
      res = new_r_Sub (current_ir_graph, get_new_node(get_nodes_Block(n)),
                       get_new_node(a), get_new_node(b), get_irn_mode(n));
    }
    break;
  case iro_Minus:
    res = new_r_Minus (current_ir_graph, get_new_node(get_nodes_Block(n)),
		       get_new_node(a), get_irn_mode(n));
    break;
  case iro_Mul:
    res = new_r_Mul (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(a), get_new_node(b), get_irn_mode(n));
    break;
  case iro_Quot:
    res = new_r_Quot (current_ir_graph, get_new_node(get_nodes_Block(n)),
		      get_new_node(get_Quot_mem(n)), get_new_node(a),
		      get_new_node(b));
    break;
  case iro_DivMod:
    res = new_r_DivMod (current_ir_graph, get_new_node(get_nodes_Block(n)),
			get_new_node(get_DivMod_mem(n)), get_new_node(a),
			get_new_node(b));
    break;
  case iro_Div:
    res = new_r_Div (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(get_Div_mem(n)), get_new_node(a),
		     get_new_node(b));
    break;
  case iro_Mod:
    res = new_r_Mod (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(get_Mod_mem(n)), get_new_node(a),
		     get_new_node(b));
    break;
  case iro_Abs:
    res = new_r_Abs (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(get_Abs_op(n)), get_irn_mode(n));
    break;
  case iro_And:
    res = new_r_And (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(a), get_new_node(b), get_irn_mode(n));
    break;
  case iro_Or:
    res = new_r_Or (current_ir_graph, get_new_node(get_nodes_Block(n)),
		    get_new_node(a), get_new_node(b), get_irn_mode(n));
    break;
  case iro_Eor:
    res = new_r_Eor (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(a), get_new_node(b), get_irn_mode(n));
    break;
  case iro_Not:
    res = new_r_Not (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(get_Not_op(n)), get_irn_mode(n));
    break;
  case iro_Cmp:
    res = new_r_Cmp (current_ir_graph,
		     get_new_node(get_nodes_Block(n)),
		     get_new_node(get_Cmp_left(n)),
		     get_new_node(get_Cmp_right(n)));
    break;
  case iro_Shl:
    res = new_r_Shl (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(get_Shl_left(n)),
		     get_new_node(get_Shl_right(n)), get_irn_mode(n));
    break;
  case iro_Shr:
    res = new_r_Shr (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(get_Shr_left(n)),
		     get_new_node(get_Shr_right(n)), get_irn_mode(n));
    break;
  case iro_Shrs:
    res = new_r_Shrs (current_ir_graph, get_new_node(get_nodes_Block(n)),
		      get_new_node(get_Shrs_left(n)),
		      get_new_node(get_Shrs_right(n)), get_irn_mode(n));
    break;
  case iro_Rot:
    res = new_r_Rot (current_ir_graph, get_new_node(get_nodes_Block(n)),
		     get_new_node(get_Rot_left(n)),
		     get_new_node(get_Rot_right(n)), get_irn_mode(n));
    break;
  case iro_Conv:
    res = new_r_Conv (current_ir_graph, get_new_node(get_nodes_Block(n)),
		      get_new_node(get_Conv_op(n)),
		      get_irn_mode(n));
    break;
  case iro_Phi:
    {
      res = get_new_node(n);
      for (i = 0; i < get_Phi_n_preds(n); i++)
	set_Phi_pred(res, i, get_new_node(get_Phi_pred(n, i)));
      set_nodes_Block(res, get_new_node(get_nodes_Block(n)));
    }
    break;
  case iro_Load:
    res = new_r_Load (current_ir_graph, get_new_node(get_nodes_Block(n)),
		      get_new_node(get_Load_mem(n)),
		      get_new_node(get_Load_ptr(n)));
    break;
  case iro_Store:
    res = new_r_Store (current_ir_graph, get_new_node(get_nodes_Block(n)),
		       get_new_node(get_Store_mem(n)),
		       get_new_node(get_Store_ptr(n)),
		       get_new_node(get_Store_value(n)));
    break;
  case iro_Alloc:
    res = new_r_Alloc (current_ir_graph, get_new_node(get_nodes_Block(n)),
		       get_new_node(get_Alloc_mem(n)),
		       get_new_node(get_Alloc_size(n)),
		       get_Alloc_type(n), get_Alloc_where(n));

    break;
  case iro_Free:
    res = new_r_Free (current_ir_graph, get_new_node(get_nodes_Block(n)),
		      get_new_node(get_Free_mem(n)),
		      get_new_node(get_Free_ptr(n)),
		      get_new_node(get_Free_size(n)), get_Free_type(n));
    break;
  case iro_Sync:
    {
      ir_node **in = get_Sync_preds_arr(n);
      for (i = 0; i < get_Sync_n_preds(n); i++)
	set_Sync_pred(n, i, get_new_node(get_Sync_pred(n, i)));
      res = new_r_Sync (current_ir_graph, get_new_node(get_nodes_Block(n)),
			get_Sync_n_preds(n), in);
    }
    break;
  case iro_Proj: {
    res = new_r_Proj (current_ir_graph, get_new_node(get_nodes_Block(n)),
		      get_new_node(get_Proj_pred(n)), get_irn_mode(n),
		      get_Proj_proj(n));
  }
    break;
  case iro_Tuple:
    {
      ir_node **in = get_Tuple_preds_arr(n);
      for (i = 0; i < get_Tuple_n_preds(n); i++)
	set_Tuple_pred(n, i, get_new_node(get_Tuple_pred(n, i)));
      res = new_r_Tuple (current_ir_graph, get_new_node(get_nodes_Block(n)),
			 get_Tuple_n_preds(n), in);
    }
    break;
  case iro_Id:
    res = get_new_node(get_Id_pred(n));
    break;
  case iro_Bad:
    res = new_r_Bad ();
    break;
  }
  /* @@@ Here we could call optimize()!! Not necessary, called in constructor anyways. */
  set_new_node(n, res);
  printf(" "); DDMSG2(res);
}

void
copy_graph2 () {
  ir_node *old_node, *new_node, *projX;
  ir_graph *irg = current_ir_graph;

  /*CS*/
  printf("Before starting the DEAD NODE ELIMINATION !\n");

  /* Copy nodes remembered in irg fields first.
     The optimization contains tests against these fields, e.g., not
     to optimize the start block away.  Therefore these fields have to
     be fixed first.
     Further setting these fields in copy_node would impose additional
     tests for all nodes of a kind.
     Predict the visited flag the walker will use! */
  /* Copy the start Block node.  Get the ProjX of the Start node, that is
     predecessor of the start Block.  We have to break the cycle and fix it
     later.  We use the old in array as placeholder. */
  old_node = irg->start_block;
  new_node = new_r_Block (current_ir_graph, get_Block_n_cfgpreds(old_node),
			  get_Block_cfgpred_arr(old_node));
  /* new_r_Block calls no optimization --> save */
  projX = get_Block_cfgpred(old_node, 0);
  irg->start_block = new_node;
  set_new_node (old_node, new_node);
  set_irn_visited (old_node, get_irg_visited(current_ir_graph)+1);
  /* Copy the Start node */
  old_node = irg->start;
  new_node = new_r_Start (current_ir_graph, irg->start_block);
  irg->start = new_node;
  set_new_node (old_node, new_node);
  set_irn_visited (old_node, get_irg_visited(current_ir_graph)+1);
  /* Copy the Bad node */
  old_node = irg->bad;
  new_node = new_ir_node (irg, irg->start_block, op_Bad, mode_T, 0, NULL);
  irg->bad = new_node;
  set_new_node (old_node, new_node);
  set_irn_visited (old_node, get_irg_visited(current_ir_graph)+1);
  /* Copy the Projs for the Start's results. */
  old_node = projX;
  new_node = new_r_Proj (irg, irg->start_block, irg->start, mode_X, pns_initial_exec);
  set_Block_cfgpred(irg->start_block, 0, new_node);
  set_new_node (old_node, new_node);
  set_irn_visited (old_node, get_irg_visited(current_ir_graph)+1);

  old_node = irg->frame;
  new_node = new_r_Proj (irg, irg->start_block, irg->start, mode_p, pns_frame_base);
  irg->frame = new_node;
  set_new_node (old_node, new_node);
  set_irn_visited (old_node, get_irg_visited(current_ir_graph)+1);

  old_node = irg->globals;
  new_node = new_r_Proj (irg, irg->start_block, irg->start, mode_p, pns_globals);
  irg->globals = new_node;
  set_new_node (old_node, new_node);
  set_irn_visited (old_node, get_irg_visited(current_ir_graph)+1);

  old_node = irg->args;
  new_node = new_r_Proj (irg, irg->start_block, irg->start, mode_T, pns_args);
  irg->args = new_node;
  set_new_node (old_node, new_node);
  set_irn_visited (old_node, get_irg_visited(current_ir_graph)+1);

  /* Walks the graph once, and at the recursive way do the copy thing.
     all reachable nodes will be copied to a new obstack. */
  irg_walk(irg->end, create_dummy, copy_node2, NULL);

  /*CS*/
  printf("After DEAD NODE ELIMINATION !\n");
}
#endif
