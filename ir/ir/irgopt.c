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
  ir_node * res, a, b;

  if (is_binop(n)) {
    a = get_binop_left(n);
    b = get_binop_right(n);
  } else if (is_unop(n)) {
    a = get_unop_op(n);
  }

  switch (get_irn_opcode(n)) {
  case iro_Block:
      int i;
      ir_node **in [get_Block_n_cfgpreds(n)];
      for (i=0; i <(get_Return_n_res(n)); i++) {
	in[i] = get_Block_cfgpred (n, i);
      }
      res = new_r_Block (current_ir_graph, get_Block_n_cfgpreds(n), in);
      set_new_node(n, res);
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
      int i;
      ir_node **in [get_Return_n_res(n)];
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
      if (get_SymConst_kind(n) == type_tag || get_SymConst_kind(n) == size)
	{
	  res = new_r_Raise (current_ir_graph, get_new_node(n),
			     get_SymConst_type(n), get_SymConst_kind (n));
	}
      else
	/* if get_SymConst_kind(n) == linkage_ptr_info */
	{
	  res = new_r_Raise (current_ir_graph, get_new_node(n),
			     get_SymConst_ptrinfo(n), get_SymConst_kind (n));
	}
    set_new_node(n, res);
    }
    break;
  case iro_Sel:
    {
      int i;
      ir_node **in [get_Sel_n_index(n)];
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
      int i;
      ir_node **in [get_Call_arity(n)];
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
		     get_new_node(a),
		     get_new_node(b), get_irn_mode(n));
    set_new_node(n, res);
    break;
  case iro_Sub:
    res = new_r_Sub (current_ir_graph, get_new_node(get_nodes_block(n)),
		     get_new_node(a),
		     get_new_node(b), get_irn_mode(n));
    set_new_node(n, res);
    break;
  case iro_Minus:
    res = new_r_Minus (current_ir_graph, get_new_node(n), get_new_node(a),
		       get_irn_mode(n));
    set_new_node(n, res);
    break;
  case iro_Mul:
    break;
  case iro_Quot:
    break;
  case iro_DivMod:
    break;
  case iro_Div:
    break;
  case iro_Mod:
    break;
  case iro_Abs:
    break;
  case iro_And:
    break;
  case iro_Or:
    break;
  case iro_Eor:
    break;
  case iro_Not:
    break;
  case iro_Cmp:
    break;
  case iro_Shl:
    break;
  case iro_Shr:
    break;
  case iro_Shrs:
    break;
  case iro_Rot:
    break;
  case iro_Conv:
    break;
  case iro_Phi:
    break;
  case iro_Load:
    break;
  case iro_Store:
    break;
  case iro_Alloc:
    break;
  case iro_Free:
    break;
  case iro_Sync:
    break;
  case iro_Proj:
    break;
  case iro_Tuple:
    break;
  case iro_Id:
    break;
  case iro_Bad:
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
