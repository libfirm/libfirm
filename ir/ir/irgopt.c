/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer
**
**  dead node elimination
**  walks one time through the whole graph and copies it into another graph,
**  so unreachable nodes will be lost.
*/

# include "irgopt.h"
# include "irnode.h"
# include "iropt.h"
# include "irgwalk.h"
# include "irgraph.h"
# include "ircons.h"

/********************************************************************/
/* apply optimizations of iropt to all nodes.                       */
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

/* Remeber the new node in the old node,
   by using a field that all nodes have. */
void *
set_new_node (ir_node *old, ir_node *new)
{
  old->in[0] = new;   /* Hi Chris: Benutze old->link, ich hab mich vergewissert dass
			 das hier ueberschrieben werden kann, das erspaart eine
			 indirektion --> schneller.  */
  return old;
}

/* Get this new node, before the old node is forgotton.*/
ir_node *
get_new_node (ir_node * n)
{
  ir_node *new;
  new = n->in[0];
  assert(new);

  return new;
}

/* Create this node on a new obstack. */
void
copy_node (ir_node *n, void *env) {
  ir_node *res = NULL;
  ir_node *a = NULL;
  ir_node *b = NULL;
  int i;

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
      ir_node **in = get_Block_cfgpred_arr(n);
      for (i = 0; i < get_Block_n_cfgpreds(n); i++)
	set_Block_cfgpred(n, i, get_new_node(get_Block_cfgpred(n, i)));
      res = new_r_Block (current_ir_graph, get_Block_n_cfgpreds(n), in);
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
      for (i = 0; i < get_Return_n_res(n); i++) {
	set_Return_res(n, i, get_new_node(get_Return_res(n, i)));
      }
      res = new_r_Return (current_ir_graph, get_new_node(get_nodes_Block(n)),
			  get_new_node(get_Return_mem(n)),
			  get_Return_n_res(n), in);
    }
    break;
  case iro_Raise:
    res = new_r_Raise (current_ir_graph, get_new_node(get_nodes_Block(n)),
		       get_new_node(get_Raise_mem(n)),
		       get_new_node(get_Raise_exo_ptr(n)));
    break;
  case iro_Const:
    res = new_r_Const (current_ir_graph, get_new_node(get_nodes_Block(n)),
		       get_irn_mode(n), get_Const_tarval(n));
    break;
  case iro_SymConst:
    {
      type_or_id *value = NULL;

      if ((get_SymConst_kind(n)==type_tag) || (get_SymConst_kind(n)==size))
	{

	   value = (type_or_id *) get_SymConst_type(n);
	}
      else
	{
	  if (get_SymConst_kind(n)==linkage_ptr_info)
	  {
	    value = (type_or_id *) get_SymConst_ptrinfo(n);
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
      ir_node **in = get_Call_param_arr(n);
      for (i = 0; i < get_Call_arity(n); i++)
	set_Call_param(n, i, get_new_node(get_Call_param(n, i)));
      res = new_r_Call (current_ir_graph, get_new_node(get_nodes_Block(n)),
			get_new_node(get_Call_mem(n)),
			get_new_node(get_Call_ptr(n)), get_Call_arity(n),
			in, get_Call_type (n));
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
    res = new_r_Cmp (current_ir_graph, get_new_node(get_nodes_Block(n)),
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
      ir_node **in = get_Phi_preds_arr(n);
      for (i = 0; i < get_Phi_n_preds(n); i++)
	set_Phi_pred(n, i, get_new_node(get_Phi_pred(n, i)));
      res = new_r_Phi (current_ir_graph, get_new_node(get_nodes_Block(n)),
		       get_Phi_n_preds(n), in, get_irn_mode(n));
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
  case iro_Proj:
    res = new_r_Proj (current_ir_graph, get_new_node(get_nodes_Block(n)),
		      get_new_node(get_Proj_pred(n)), get_irn_mode(n),
		      get_Proj_proj(n));
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
    res = new_r_Id (current_ir_graph, get_new_node(get_nodes_Block(n)),
		    get_new_node(get_Id_pred(n)), get_irn_mode(n));
    break;
  case iro_Bad:
    res = new_r_Bad ();
    break;
  }
  /* @@@ Here we could call optimize()!! */
  set_new_node(n, res);

  printf(" "); DDMSG2(res);
}


void
dead_node_elimination(ir_graph *irg) {
  struct obstack *graveyard_obst=NULL;
  struct obstack *rebirth_obst;

  ir_node *old_node, *new_node;
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  if (get_opt_dead_node_elimination()) {

    /* A quiet place, where the old obstack can rest in peace,
       until it will be cremated. */
    graveyard_obst = irg->obst;

    /* A new obstack, where the reachable nodes will be copied to. */
    rebirth_obst = (struct obstack *) xmalloc (sizeof (struct obstack));
    current_ir_graph->obst = rebirth_obst;
    obstack_init (current_ir_graph->obst);

    /*CS*/
    printf("Before starting the DEAD NODE ELIMINATION !\n");

    /* Copy nodes remembered in irg fields first.
       The optimization contains tests against these fields, e.g., not
       to optimize the start block away.  Therefore these fields have to
       be fixed first.
       Further setting these fields in copy_node would impose additional
       tests for all nodes of a kind.
       Predict the visited flag the walker will use! */
    /* Copy the start Block node */
    old_node = irg->start_block;
    new_node = new_r_Block (current_ir_graph, 0, NULL);   /* new_r_Block calls
						     no optimization --> save */
    irg->start_block = new_node;
    set_new_node (old_node, new_node);
    set_irn_visited (new_node, get_irg_visited(current_ir_graph)+1);
    /* Copy the Start node */
    old_node = irg->start;
    new_node = new_r_Start (current_ir_graph, irg->start_block);
    irg->start = new_node;
  DDMSG2(new_node);
    set_new_node (old_node, new_node);
    set_irn_visited (new_node, get_irg_visited(current_ir_graph)+1);
    /* Copy the Bad node */
    old_node = irg->bad;
    new_node = new_ir_node (irg, irg->start_block, op_Bad, mode_T, 0, NULL);
    irg->bad = new_node;
    set_new_node (old_node, new_node);
    set_irn_visited (new_node, get_irg_visited(current_ir_graph)+1);
    /* Copy the Projs for the Start's results. */
    old_node = irg->frame;
    new_node = new_r_Proj (irg, irg->start_block, irg->start, mode_p, pns_frame_base);
    irg->frame = new_node;
    set_new_node (old_node, new_node);
    set_irn_visited (new_node, get_irg_visited(current_ir_graph)+1);

    old_node = irg->globals;
    new_node = new_r_Proj (irg, irg->start_block, irg->start, mode_p, pns_globals);
    irg->globals = new_node;
    set_new_node (old_node, new_node);
    set_irn_visited (new_node, get_irg_visited(current_ir_graph)+1);

    old_node = irg->args;
    new_node = new_r_Proj (irg, irg->start_block, irg->start, mode_T, pns_args);
    irg->args = new_node;
    set_new_node (old_node, new_node);
    set_irn_visited (new_node, get_irg_visited(current_ir_graph)+1);

    /* Walks the graph once, and at the recursive way do the copy thing.
       all reachable nodes will be copied to a new obstack. */
    irg_walk(irg->end, NULL, copy_node, NULL);

    /*CS*/
    printf("After the DEAD NODE ELIMINATION !\n");

    /* Free memory from old unoptimized obstack */
    xfree (graveyard_obst);
  }

  current_ir_graph = rem;
}
