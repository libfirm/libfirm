/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer
**
**  dead node elemination
**  walks one time through the whole graph and copies it into another graph,
**  so unreachable nodes will be lost.
*/

# include "irgopt.h"
# include "irnode.h"
# include "iropt.h"
# include "irgwalk.h"
# include "irgraph.h"
# include "ircons.h"

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

/* Remeber the new node in the old node,
   by using a field that all nodes have. */
void *
set_new_node (ir_node *old, ir_node *new)
{
  old->in[0] = new;
  return old;
}

/* Get this new node, before the old node is forgotton.*/
ir_node *
get_new_node (ir_node * n)
{
  return n->in[0];
}



/* Create this node on a new obstack. */
void
copy_node (ir_node *n, void *env) {
  int i;
  ir_node *res, *a, *b;
  res = (ir_node *) malloc (sizeof (ir_node));
  a = (ir_node *) malloc (sizeof (ir_node));
  b = (ir_node *) malloc (sizeof (ir_node));

  if (is_binop(n)) {
    a = get_binop_left(n);
    b = get_binop_right(n);
  } else if (is_unop(n)) {
    a = get_unop_op(n);
  }

  switch (get_irn_opcode(n)) {
  case iro_Block:
    {
      /*CS malloc*/
      ir_node *in [get_Block_n_cfgpreds(n)];

      for (i=0; i <(get_Block_n_cfgpreds(n)); i++) {
	in[i] = get_Block_cfgpred (n, i);
      }
      res = new_r_Block (current_ir_graph, get_Block_n_cfgpreds(n), in);
      set_new_node(n, res);
    }
    break;
  case iro_Start:
    res = new_r_Start (current_ir_graph, get_new_node(n));
    set_new_node(n, res);
    break;
  case iro_End:
    res = new_r_End (current_ir_graph, get_new_node(n));
    set_new_node(n, res);
    break;
  case iro_Jmp:
    res = new_r_Jmp (current_ir_graph, get_new_node(n));
    set_new_node(n, res);
    break;
  case iro_Cond:
    res = new_r_Cond (current_ir_graph, get_new_node(n),
		      get_Cond_selector(n));
    set_new_node(n, res);
    break;
  case iro_Return:
    {
      /*CS malloc*/
      ir_node *in [get_Return_n_res(n)];
      for (i=0; i <(get_Return_n_res(n)); i++) {
	in[i] = get_Return_res (n, i);
      }
      res = new_r_Return (current_ir_graph, get_new_node(n),
			  get_Return_mem (n), get_Return_n_res(n), in);
      set_new_node(n, res);
    }
    break;
  case iro_Raise:
    res = new_r_Raise (current_ir_graph, get_new_node(n),
		       get_Raise_mem(n), get_Raise_exoptr(n));
    set_new_node(n, res);
    break;
  case iro_Const:
    res = new_r_Const (current_ir_graph, get_new_node(n),
		       get_irn_mode(n), get_Const_tarval(n));
    set_new_node(n, res);
    break;
  case iro_SymConst:
    {
      type_or_id *value;
      value = (type_or_id *) malloc (sizeof (type_or_id));
      if ((get_SymConst_kind(n)==type_tag) || (get_SymConst_kind(n)==size))
	{
	  value = get_SymConst_type(n);
	}
      else
	{
	  if (get_SymConst_kind(n)==linkage_ptr_info)
	  {
	    value = get_SymConst_ptrinfo(n);
	  }
	}
    res = new_r_SymConst (current_ir_graph, get_new_node(n), value,
			  get_SymConst_kind (n));
    set_new_node(n, res);
    }
    break;
  case iro_Sel:
    {
      /*CS*/
      ir_node *in [get_Sel_n_index(n)];
      for (i=0; i <(get_Sel_n_index(n)); i++) {
	in[i] = get_Sel_index (n, i);
      }
      res = new_r_Sel (current_ir_graph, get_new_node(n),
		       get_Sel_mem(n), get_Sel_ptr(n), get_Sel_n_index(n),
		       in, get_Sel_entity(n));
      set_new_node(n, res);
    }
    break;
  case  iro_Call:
    {
      /*CS*/
      ir_node *in [get_Call_arity(n)];
      for (i=0; i <(get_Call_arity(n)); i++) {
	in[i] = get_Call_param (n, i);
      }
      res = new_r_Call (current_ir_graph, get_new_node(n), get_Call_mem(n),
			get_Call_ptr(n), get_Call_arity(n),
			in, get_Call_type (n));
      set_new_node(n, res);
    }
    break;
  case iro_Add:
    res = new_r_Add (current_ir_graph, get_new_node(n),
		     get_new_node(a), get_new_node(b), get_irn_mode(n));
    set_new_node(n, res);
    break;
  case iro_Sub:
    {
      ir_node *temp_node;
      temp_node = get_nodes_block(n);
      res = new_r_Sub (current_ir_graph, get_new_node(temp_node),
                       get_new_node(a), get_new_node(b), get_irn_mode(n));
      set_new_node(n, res);
    }
    break;
  case iro_Minus:
    res = new_r_Minus (current_ir_graph, get_new_node(n), get_new_node(a),
		       get_irn_mode(n));
    set_new_node(n, res);
    break;
  case iro_Mul:
    res = new_r_Mul (current_ir_graph, get_new_node(n), get_new_node(a),
		       get_new_node(b), get_irn_mode(n));
    break;
  case iro_Quot:
    res = new_r_Quot (current_ir_graph, get_new_node(n), get_Quot_mem (n),
		      get_new_node(a), get_new_node(b));
    break;
  case iro_DivMod:
    res = new_r_DivMod (current_ir_graph, get_new_node(n), get_DivMod_mem(n),
			get_new_node(a), get_new_node(b));
    break;
  case iro_Div:
    res = new_r_Div (current_ir_graph, get_new_node(n), get_Div_mem(n),
		     get_new_node(a), get_new_node(b));
    break;
  case iro_Mod:
    res = new_r_Mod (current_ir_graph, get_new_node(n), get_Mod_mem(n),
		     get_new_node(a), get_new_node(b));
    break;
  case iro_Abs:
    res = new_r_Mod (current_ir_graph, get_new_node(n), get_Abs_op(n),
		     get_irn_mode(n));
    break;
  case iro_And:
    res = new_r_And (current_ir_graph, get_new_node(n), get_new_node(a),
		     get_new_node(b), get_irn_mode(n));
    break;
  case iro_Or:
    res = new_r_Or (current_ir_graph, get_new_node(n), get_new_node(a),
		    get_new_node(b), get_irn_mode(n));
    break;
  case iro_Eor:
    res = new_r_Eor (current_ir_graph, get_new_node(n), get_new_node(a),
		     get_new_node(b), get_irn_mode(n));
    break;
  case iro_Not:
    res = new_r_Not (current_ir_graph, get_new_node(n), get_Not_op(n),
		     get_irn_mode(n));
    break;
  case iro_Cmp:
    res = new_r_Cmp (current_ir_graph, get_new_node(n), get_Cmp_left(n),
		     get_Cmp_right(n));
    break;
  case iro_Shl:
    res = new_r_Shl (current_ir_graph, get_new_node(n), get_Shl_left(n),
		     get_Shl_right(n), get_irn_mode(n));
    break;
  case iro_Shr:
    res = new_r_Shr (current_ir_graph, get_new_node(n), get_Shr_left(n),
		     get_Shr_right(n), get_irn_mode(n));
    break;
  case iro_Shrs:
    res = new_r_Shrs (current_ir_graph, get_new_node(n), get_Shrs_left(n),
		      get_Shrs_right(n), get_irn_mode(n));
    break;
  case iro_Rot:
    res = new_r_Rot (current_ir_graph, get_new_node(n), get_Rot_left(n),
		     get_Rot_right(n), get_irn_mode(n));
    break;
  case iro_Conv:
    res = new_r_Conv (current_ir_graph, get_new_node(n), get_Conv_op(n),
		     get_irn_mode(n));
    break;
  case iro_Phi:
    /*CS malloc*/
    {
      ir_node *in [get_Phi_n_preds(n)];
      for (i=0; i <(get_Phi_n_preds(n)); i++) {
	in[i] = get_Phi_pred (n, i);
      }
      res = new_r_Phi (current_ir_graph, get_new_node(n),
		       get_Phi_n_preds(n), in, get_irn_mode(n));
      set_new_node(n, res);
    }
    break;
  case iro_Load:
    res = new_r_Load (current_ir_graph, get_new_node(n), get_Load_mem(n),
		      get_Load_ptr(n));
    break;
  case iro_Store:
    res = new_r_Store (current_ir_graph, get_new_node(n), get_Store_mem(n),
		       get_Store_ptr(n), get_Store_value(n));
    break;
  case iro_Alloc:
    res = new_r_Alloc (current_ir_graph, get_new_node(n),
		       get_Alloc_mem(n), get_Alloc_size(n),
		       get_Alloc_type(n), get_Alloc_where(n));

    break;
  case iro_Free:
    res = new_r_Free (current_ir_graph, get_new_node(n),
		      get_Free_mem(n), get_Free_ptr(n),
		      get_Free_size(n), get_Free_type(n));
    break;
  case iro_Sync:
    /*CS malloc*/
    {
      ir_node *in [get_Sync_n_preds(n)];
      for (i=0; i <(get_Sync_n_preds(n)); i++) {
	in[i] = get_Sync_pred (n, i);
      }
      res = new_r_Sync (current_ir_graph, get_new_node(n),
			get_Sync_n_preds(n), in);
      set_new_node(n, res);
    }
    break;
  case iro_Proj:
    res = new_r_Proj (current_ir_graph, get_new_node(n),
		      get_Proj_pred(n), get_irn_mode(n),
		      get_Proj_proj(n));
    break;
  case iro_Tuple:
    /*CS malloc*/
    {
      ir_node *in [get_Tuple_n_preds(n)];
      for (i=0; i <(get_Tuple_n_preds(n)); i++) {
	in[i] = get_Tuple_pred (n, i);
      }
      res = new_r_Tuple (current_ir_graph, get_new_node(n),
			 get_Tuple_n_preds(n), in);
      set_new_node(n, res);
    }
    break;
  case iro_Id:
    res = new_r_Id (current_ir_graph, get_new_node(n),
		      get_Id_pred(n), get_irn_mode(n));
    break;
  case iro_Bad:
    res = new_r_Bad (get_new_node(n));
    break;
  }

}


void
dead_node_elemination(ir_graph *irg) {
  struct obstack *graveyard_obst;
  struct obstack *rebirth_obst;

  /* A quiet place, where the old obstack can rest in peace,
     until it will be cremated. */
  graveyard_obst = irg->obst;

  /* A new obstack, where the reachable nodes will be copied to. */
  rebirth_obst = (struct obstack *) xmalloc (sizeof (struct obstack));
  current_ir_graph->obst = rebirth_obst;
  obstack_init (current_ir_graph->obst);

  /* Walks the graph once, and at the recursive way do the copy thing.
     all reachable nodes will be copied to a new obstack. */
  irg_walk(irg->end, NULL, copy_node, NULL);

  /* Free memory from old unoptimized obstack */
  xfree (graveyard_obst);

}
