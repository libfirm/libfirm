/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Christian Schaefer
*
*
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irgraph_t.h"
# include "irvrfy.h"
# include "irgwalk.h"

/* @@@ replace use of array "in" by access functions. */
ir_node **get_irn_in (ir_node *node);

INLINE static void
vrfy_Proj_proj(ir_node *p, ir_graph *irg) {
  ir_node *pred;
  ir_mode *mode;
  int proj;

  pred = skip_nop(get_Proj_pred(p));
  assert(get_irn_mode(pred) == mode_T);
  mode = get_irn_mode(p);
  proj = get_Proj_proj(p);

  switch (get_irn_opcode(pred)) {
  case iro_Start:
    assert (((proj == 0 && mode == mode_X) ||
	     (proj == 1 && mode == mode_M) ||
	     (proj == 2 && mode == mode_P) ||
	     (proj == 3 && mode == mode_P) ||
	     (proj == 4 && mode == mode_T))   &&
	    "wrong Proj from Start"  );
    break;
  case iro_Cond:
    assert ((proj >= 0 && mode == mode_X) &&
	    "wrong Proj from Cond");
    break;
  case iro_Raise:
    assert (((proj == 0 && mode == mode_X) ||
	     (proj == 1 && mode == mode_M))   &&
	    "wrong Proj from Raise" );
    break;
  case iro_InstOf:
	assert (((proj >= 0 && mode == mode_X) &&
			 "wrong Proj from InstOf"));
	break;
  case iro_Call:
    assert (((proj == 0 && mode == mode_M) ||
	     (proj == 1 && mode == mode_X) ||
	     (proj == 2 && mode == mode_T) ||
	     (proj == 3 && mode == mode_M))   &&
	    "wrong Proj from Call" );
    break;
  case iro_Quot:
    assert (((proj == 0 && mode == mode_M) ||
	     (proj == 1 && mode == mode_X) ||
	     (proj == 2 && mode_is_float(mode)))  &&
	    "wrong Proj from Quot");
    break;
  case iro_DivMod:
    assert (((proj == 0 && mode == mode_M) ||
	     (proj == 1 && mode == mode_X) ||
	     (proj == 2 && mode == mode_Is) ||
	     (proj == 3 && mode == mode_Is))   &&
	    "wrong Proj from DivMod" );
    break;
  case iro_Div:
  case iro_Mod:
    assert (((proj == 0 && mode == mode_M) ||
	     (proj == 1 && mode == mode_X) ||
	     (proj == 2 && mode == mode_Is))   &&
	     "wrong Proj from Div or Mod" );
    break;
  case iro_Cmp:
    assert ((proj >= 0 && proj <= 15 && mode == mode_b) &&
	    "wrong Proj from Cmp");
    break;
  case iro_Load:
    assert (((proj == 0 && mode == mode_M) ||
	     (proj == 1 && mode == mode_X) ||
	     (proj == 2 && mode_is_data(mode)))  &&
	    "wrong Proj from Load");
    break;
  case iro_Store:
    assert (((proj == 0 && mode == mode_M) ||
	     (proj == 1 && mode == mode_X))    &&
	    "wrong Proj from Store");
    break;
  case iro_Alloc:
    assert (((proj == 0 && mode == mode_M) ||
	     (proj == 1 /* && mode == mode_X*/) ||
	     (proj == 2 && mode == mode_P))      &&
	    "wrong Proj from Alloc");
    break;
  case iro_Proj: {
    type *mt; /* A method type */
    pred = skip_nop(get_Proj_pred(pred));
    assert(get_irn_mode(pred) == mode_T);
    switch (get_irn_opcode(pred)) {
    case iro_Start: {
      assert (proj >= 0 && mode_is_data(mode) &&
	      "wrong Proj from Proj from Start");
      mt = get_entity_type(get_irg_ent(irg));
      assert(proj < get_method_n_params(mt) &&
	     "More Projs for args than args in type");
      if ((mode == mode_P) && is_compound_type(get_method_param_type(mt, proj)))
	/* value argument */ break;
      assert(mode == get_type_mode(get_method_param_type(mt, proj)) &&
      "Mode of Proj from Start doesn't match mode of param type.");
    } break;
    case iro_Call: {
      assert (proj >= 0 && mode_is_data(mode) &&
	      "wrong Proj from Proj from Call");
      mt = get_Call_type(pred);
      assert(proj < get_method_n_ress(mt) &&
	     "More Projs for results than results in type.");
      if ((mode == mode_P) && is_compound_type(get_method_res_type(mt, proj)))
	/* value result */ break;
      assert(mode == get_type_mode(get_method_res_type(mt, proj)) &&
      "Mode of Proj from Call doesn't match mode of result type.");
    } break;
    case iro_Tuple: ;
      /* We don't test */
      break;
    default: assert(0);
    } break;
  }
  case iro_Tuple:
    /* We don't test */
    break;
  case iro_CallBegin:
    break;
  case iro_EndReg:
    break;
  case iro_EndExcept:
    break;
  default: assert(0);
  }
}

void
irn_vrfy_irg (ir_node *n, ir_graph *irg)
{
  int i;
  int opcode, opcode1;
  ir_mode *mymode, *op1mode = NULL, *op2mode, *op3mode;
  int op_is_symmetric = 1;	/* 0: asymmetric
				   1: operands have identical modes
  				   2: modes of operands == mode of this node */
  type *mt; /* A method type */

  ir_node **in;

  if (! interprocedural_view) {
    /*
     * do NOT check placement in interprocedural view, as we don't always know
     * the "right" graph ...
     */
    assert(node_is_in_irgs_storage(irg, n) && "Node is not stored on proper IR graph!");
  }

  opcode = get_irn_opcode (n);

  if (opcode != iro_Phi && opcode != iro_Block)
    for (i = 0; i < get_irn_arity(n); i++) {
      opcode1 = get_irn_opcode(get_irn_n(n, i));
      if (opcode1 == iro_Bad || opcode1 == iro_Unknown)
	return;
    }

  mymode = get_irn_mode (n);
  in = get_irn_in (n);

  switch (opcode) {

  case iro_Start:
    assert (
	    /* Start: BB --> X x M x P x data1 x ... x datan */
	    mymode == mode_T && "Start node"
	   );
    break;
  case iro_Jmp:
    assert (
	    /* Jmp: BB --> X */
	    mymode == mode_X && "Jmp node"
	   );
    break;
  case iro_Break:
    assert (
	    /* Jmp: BB --> X */
	    mymode == mode_X && "Jmp node"
	   );
    break;
  case iro_Cond:
    op1mode = get_irn_mode(in[1]);
    assert (
	    /* Cond: BB x b --> X x X */
	    (op1mode == mode_b
	    /* Cond: BB x Iu --> X^n */
	    || op1mode == mode_Iu) && "Cond node"
           );
    assert (mymode == mode_T);
    break;
  case iro_Return:
    op1mode = get_irn_mode(in[1]);
    /* Return: BB x M x data1 x ... x datan --> X */
    /* printf("mode: %s, code %s\n", ID_TO_STR(n->mode->name), ID_TO_STR(n->op->name));*/
    assert ( op1mode == mode_M  && "Return node" );  /* operand M */
    for (i=2; i < get_irn_arity(n); i++) {
      assert ( mode_is_data(get_irn_mode(in[i]))  && "Return node");  /* operand datai */
    };
    assert ( mymode == mode_X );   /* result X */
    /* Compare returned results with result types of method type */
    mt = get_entity_type(get_irg_ent(irg));
    assert(get_Return_n_ress(n) == get_method_n_ress(mt) &&
	     "Number of results for Return doesn't match number of results in type.");
    for (i = 0; i < get_Return_n_ress(n); i++)
      assert((get_irn_mode(get_Return_res(n, i))
	      == get_type_mode(get_method_res_type(mt, i))) &&
	     "Mode of result for Return doesn't match mode of result type.");

    break;
  case iro_Raise:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* Sel: BB x M x P --> X x M */
	    op1mode == mode_M && op2mode == mode_P
	    && mymode == mode_T && "Raise node"
	   );
    break;
  case iro_Const:
    assert (
	    /* Const: BB --> data */
	    (mode_is_data (mymode) ||
	     mymode == mode_b)      /* we want boolean constants for static evaluation */
             && "Const node"        /* of Cmp. */
	   );
    break;
  case iro_SymConst:
    assert (
	    /* SymConst: BB --> Iu or
                         BB --> P */
	    ((mymode == mode_Iu) || (mymode == mode_P))  && "SymConst node"
	   );
    break;
  case iro_Sel:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* Sel: BB x M x P x Iu^n --> P */
	    op1mode == mode_M && op2mode == mode_P
            && mymode == mode_P && "Sel node"
	   );
    for (i=3; i < get_irn_arity(n); i++) {
	    assert (get_irn_mode(in[i]) == mode_Iu && "Sel node"); }
    break;
  case iro_InstOf:
	assert (mode_T == mymode);
	assert (mode_is_data (op1mode));
	break;
  case iro_Call:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
      /* Call: BB x M x P x data1 x ... x datan
                 --> M x datan+1 x ... x data n+m */
    assert ( op1mode == mode_M && op2mode == mode_P  && "Call node");  /* operand M x P */
    for (i=3; i < get_irn_arity(n); i++) {
      assert ( mode_is_data(get_irn_mode(in[i])) && "Call node");  /* operand datai */
    };
    assert ( mymode == mode_T );   /* result T */
    /* Compare arguments of node with those of type */
    mt = get_Call_type(n);

    if(get_method_variadicity(mt) == variadic)
      {
	assert(get_Call_n_params(n) >= get_method_n_params(mt) &&
	       "Number of args for Call doesn't match number of args in " \
	       " variadic type.");
      }
    else
      {
	assert(get_Call_n_params(n) == get_method_n_params(mt) &&
	       "Number of args for Call doesn't match number of args in " \
	       "non variadic type.");
      }

    for (i = 0; i < get_method_n_params(mt); i++)
      assert((get_irn_mode(get_Call_param(n, i))
	      == get_type_mode(get_method_param_type(mt, i))) &&
	     "Mode of arg for Call doesn't match mode of arg type.");
    break;
  case iro_Add:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* common Add: BB x num x num --> num */
	    ((mymode == op1mode && mymode == op2mode
	      && (mode_is_num(mymode) || mymode == mode_P))
	     ||  /* Pointer Add: BB x P x Is --> P */
	     (op1mode == mode_P && op2mode == mode_Is && mymode == mode_P)
	     ||  /* Pointer Add: BB x Is x P --> P */
	     (op1mode == mode_Is && op2mode == mode_P && mymode == mode_P))
	    && "Add node"
           );
      if (op1mode == mode_P || op2mode == mode_P) {
	/* BB x P x Is --> P or BB x Is x P --> P */
        op_is_symmetric = 0; /* ArmRoq */
      } else {
	/* BB x num x num --> num */
        op_is_symmetric = 2;
      }
    break;
  case iro_Sub:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* common Sub: BB x num x num --> num */
	    ((mymode ==op1mode && mymode == op2mode
	      && mode_is_num(op1mode))
	     ||  /* Pointer Sub: BB x P x Is --> P */
	     (op1mode == mode_P && op2mode == mode_Is && mymode == mode_P)
	     ||  /* Pointer Sub: BB x Is x P --> P */
	     (op1mode == mode_Is && op2mode == mode_P && mymode == mode_P)
	     ||  /* Pointer Sub: BB x P x P --> Is */
	     (op1mode == mode_P && op2mode == mode_P && mymode == mode_Is))
	    && "Sub node"
           );
      if (op1mode == mode_P && op2mode == mode_P) {
        op_is_symmetric = 1; /* ArmRoq */
      } else if (op1mode == mode_P || op2mode == mode_P) {
        op_is_symmetric = 0; /* ArmRoq */
      } else {
        op_is_symmetric = 2;
      }
    break;
  case iro_Minus:
    op1mode = get_irn_mode(in[1]);
    assert (
	    /* Minus: BB x float --> float */
	    op1mode == mymode && mode_is_float (op1mode)  && "Minus node"
	   );
    op_is_symmetric = 2;
    break;
  case iro_Mul:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* Mul: BB x num x num --> num */
	    mymode == op1mode && mymode == op2mode
	    && mode_is_num (op1mode) && "Mul node"
	   );
    op_is_symmetric = 2;
    break;
  case iro_Quot:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    op3mode = get_irn_mode(in[3]);
    assert (
	    /* Quot: BB x M x float x float --> M x X x float */
	    op1mode == mode_M && op2mode == op3mode
	    && mode_is_float(op2mode) && mymode == mode_T && "Quot node"
	   );
    op_is_symmetric = 2;
    break;
  case iro_DivMod:;
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    op3mode = get_irn_mode(in[3]);
    assert (
	    /* DivMod: BB x M x num x num --> M x X x Is x Is */
	    op1mode == mode_M && op2mode == op3mode
	    && mode_is_num (op2mode) && mymode == mode_T && "DivMod node"
           );
    op_is_symmetric = 1;
    break;
  case iro_Div:
  case iro_Mod:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    op3mode = get_irn_mode(in[3]);
    assert (
	    /* Div or Mod: BB x M x num x num --> M x X x Is */
	    op1mode == mode_M && op2mode == op3mode &&
	    mode_is_num (op2mode) && mymode == mode_T && "Div or Mod node"
           );
    op_is_symmetric = 1;
    break;
  case iro_Abs:
    op1mode = get_irn_mode(in[1]);
    assert (
	    /* Abs: BB x num --> num */
	    op1mode == mymode && mode_is_num (op1mode) && "Abs node"
	   );
    op_is_symmetric = 2;
    break;
  case iro_And:
  case iro_Or:
  case iro_Eor:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert(
	   /* And or Or or Eor: BB x int x int --> int */
	   mymode == op1mode && mymode == op2mode
	   && mode_is_int (mymode) && "And, Or or Eor node"
	  );
    op_is_symmetric = 2;
    break;
  case iro_Not:
    op1mode = get_irn_mode(in[1]);
    assert(
	   /* Not: BB x int --> int */
	   mymode == op1mode
	   && mode_is_int (mymode) && "Not node"
	  );
    op_is_symmetric = 2;
    break;

  case iro_Cmp:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert(
	   /* Cmp: BB x datab x datab --> b16 */
           op1mode == op2mode && mode_is_data (op1mode)
           && mymode == mode_T && "Cmp node"
	  );
    break;
  case iro_Shl:
  case iro_Shr:
  case iro_Shrs:
  case iro_Rot:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert(
	   /* Shl, Shr, Shrs or Rot: BB x int x Iu --> int */
	   mode_is_int (op1mode) && op2mode == mode_Iu
           && op1mode == mymode && "Shl, Shr, Shr or Rot node"
	  );
    break;
  case iro_Conv:
    op1mode = get_irn_mode(in[1]);
    assert(
           /* Conv: BB x datab1 --> datab2 */
	   mode_is_datab (op1mode)
           && mode_is_data (mymode) && "Conv node"
	  );
    break;
  case iro_Phi:
    /* Phi: BB x dataM^n --> dataM */
    /* for some reason "<=" aborts. Is there a problem with get_store? */
    for (i=1; i < get_irn_arity(n); i++) {
      if (!is_Bad(in[i]) && (get_irn_op(in[i]) != op_Unknown))
	assert ( get_irn_mode(in[i]) == mymode  && "Phi node");
    };
    assert ( mode_is_dataM(mymode)  && "Phi node");
    break;
  case iro_Load:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert(
           /* Load: BB x M x P --> M x X x data */
           op1mode == mode_M && op2mode == mode_P  && "Load node"
	  );
    assert ( mymode == mode_T  && "Load node");
    break;
  case iro_Store:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    op3mode = get_irn_mode(in[3]);
    assert(
           /* Load: BB x M x P x data --> M x X */
           op1mode == mode_M && op2mode == mode_P
           && mode_is_data (op3mode) && "Store node"
	  );
    assert(mymode == mode_T && "Store node");
    break;
  case iro_Alloc:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert(
           /* Alloc: BB x M x Iu --> M x X x P */
           op1mode == mode_M && op2mode == mode_Iu
           && mymode == mode_T && "Alloc node"
	  );
    break;
  case iro_Free:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    op3mode = get_irn_mode(in[3]);
    assert(
           /* Free: BB x M x P x Iu --> M */
           op1mode == mode_M && op2mode == mode_P && op3mode == mode_Iu
           && mymode == mode_M && "Free node"
	  );
    break;
  case iro_Sync:
           /* Sync: BB x M^n --> M */
    for (i=1; i < get_irn_arity(n); i++) {
      assert ( get_irn_mode(in[i]) == mode_M  && "Sync node");
    };
    assert ( mymode == mode_M  && "Sync node");
    break;
  case iro_Proj:
    vrfy_Proj_proj(n, irg);
    break;
  default: ;
  }
}

void
irn_vrfy (ir_node *n)
{
  irn_vrfy_irg(n, current_ir_graph);
}

/*******************************************************************/
/* Verify the whole graph.                                         */
/*******************************************************************/

static void
vrfy_wrap (ir_node *node, void *env) {
  irn_vrfy(node);
}

void irg_vrfy (ir_graph *irg)
{
  ir_graph *rem;
  rem = current_ir_graph;
  current_ir_graph = irg;

  assert(get_irg_pinned(irg) == pinned);

  irg_walk(irg->end, vrfy_wrap, NULL, NULL);

  current_ir_graph = rem;
}
