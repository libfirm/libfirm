/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
x**
*/

# include "irgraph_t.h"
# include "irvrfy.h"
# include "irgwalk.h"

void
irn_vrfy (ir_node *n)
{
  int i;
  int opcode;
  ir_mode *mymode, *op1mode, *op2mode, *op3mode;
  int op_is_symmetric = 1;	/* 0: asymmetric
				   1: operands have identical modes
  				   2: modes of operands == mode of this node */

  ir_node **in;

  opcode = get_irn_opcode (n);
  mymode = get_irn_mode (n);
  in = get_irn_in (n);

  switch (opcode) {

  case iro_Start:
    assert (
	    /* Start: BB --> X x M x P x data1 x ... x datan */
	    mymode == mode_T
	   );
    break;
  case iro_Jmp:
    assert (
	    /* Jmp: BB --> X */
	    mymode == mode_X
	   );
    break;
  case iro_Cond:
    op1mode = get_irn_mode(in[1]);
    assert (
	    /* Cond: BB x b --> X x X */
	    (op1mode == mode_b
	    /* Cond: BB x Iu --> X^n */
	    || op1mode == mode_I)
           );
            assert (mymode == mode_T);
    break;
  case iro_Return:
    op1mode = get_irn_mode(in[1]);
      /* Return: BB x M x data1 x ... x datan --> X */
    //printf("mode: %s, code %s\n", ID_TO_STR(n->mode->name), ID_TO_STR(n->op->name));
    assert ( op1mode == mode_M );  /* operand M */
    for (i=2; i < get_irn_arity(n); i++) {
      assert ( mode_is_data(get_irn_mode(in[i])) );  /* operand datai */
    };
    assert ( mymode == mode_X );   /* result X */
    break;
  case iro_Raise:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* Sel: BB x M x P --> X x M */
	    op1mode == mode_M && op2mode == mode_p
	    && mymode == mode_T
	   );
    break;
  case iro_Const:
    assert (
	    /* Const: BB --> data */
	    mode_is_data (mymode) ||
            mymode == mode_b      /* we want boolean constants for static evaluation
                                     of Cmp. */
	   );
    break;
  case iro_SymConst:
    assert (
	    /* SymConst: BB --> Iu or
                         BB --> P */
	    (mymode == mode_I) || (mymode == mode_p)
	   );
    break;
  case iro_Sel:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* Sel: BB x M x P x Iu^n --> P */
	    op1mode == mode_M && op2mode == mode_p
            && mymode == mode_p
	   );
    for (i=3; i < get_irn_arity(n); i++) {
	    assert (get_irn_mode(in[i]) == mode_I); }
    break;
  case iro_Call:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
      /* Call: BB x M x P x data1 x ... x datan
                 --> M x datan+1 x ... x data n+m */
    assert ( op1mode == mode_M && op2mode == mode_p );  /* operand M x P */
    for (i=3; i < get_irn_arity(n); i++) {
      assert ( mode_is_data(get_irn_mode(in[i])) );  /* operand datai */
    };
    assert ( mymode == mode_T );   /* result T */
    break;
  case iro_Add:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* common Add: BB x num x num --> num */
	    (mymode == op1mode && mymode == op2mode
             && mode_is_num(mymode))
	    ||  /* Pointer Add: BB x P x Is --> P */
	    (op1mode == mode_p && op2mode == mode_i && mymode == mode_p)
	    ||  /* Pointer Add: BB x Is x P --> P */
	    (op1mode == mode_i && op2mode == mode_p && mymode == mode_p)
           );
      if (op1mode == mode_p || op2mode == mode_p) {
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
	    (mymode ==op1mode && mymode == op2mode
	     && mode_is_num(op1mode))
	    ||  /* Pointer Sub: BB x P x Is --> P */
	    (op1mode == mode_p && op2mode == mode_i && mymode == mode_p)
	    ||  /* Pointer Sub: BB x Is x P --> P */
	    (op1mode == mode_i && op2mode == mode_p && mymode == mode_p)
	    ||  /* Pointer Sub: BB x P x P --> Is */
	    (op1mode == mode_p && op2mode == mode_p && mymode == mode_i)
           );
      if (op1mode == mode_p && op2mode == mode_p) {
        op_is_symmetric = 1; /* ArmRoq */
      } else if (op1mode == mode_p || op2mode == mode_p) {
        op_is_symmetric = 0; /* ArmRoq */
      } else {
        op_is_symmetric = 2;
      }
    break;
  case iro_Minus:
    op1mode = get_irn_mode(in[1]);
    assert (
	    /* Minus: BB x float --> float */
	    op1mode == mymode && mode_is_float (op1mode)
	   );
    op_is_symmetric = 2;
    break;
  case iro_Mul:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert (
	    /* Mul: BB x num x num --> num */
	    mymode == op1mode && mymode == op2mode
	    && mode_is_num (op1mode)
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
	    && mode_is_float(op2mode) && mymode == mode_T
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
	    && mode_is_num (op2mode) && mymode == mode_T
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
	    mode_is_num (op2mode) && mymode == mode_T
           );
    op_is_symmetric = 1;
    break;
  case iro_Abs:
    op1mode = get_irn_mode(in[1]);
    assert (
	    /* Abs: BB x num --> num */
	    op1mode == mymode && mode_is_num (op1mode)
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
	   && mode_is_int (mymode)
	  );
    op_is_symmetric = 2;
    break;
  case iro_Not:
    op1mode = get_irn_mode(in[1]);
    assert(
	   /* Not: BB x int --> int */
	   mymode == op1mode
	   && mode_is_int (mymode)
	  );
    op_is_symmetric = 2;
    break;

  case iro_Cmp:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert(
	   /* Cmp: BB x datab x datab --> b16 */
           op1mode == op2mode && mode_is_data (op1mode)
           && mymode == mode_T
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
	   mode_is_int (op1mode) && op2mode == mode_I
           && op1mode == mymode
	  );
    break;
  case iro_Conv:
    op1mode = get_irn_mode(in[1]);
    assert(
           /* Conv: BB x datab1 --> datab2 */
	   mode_is_datab (op1mode)
           && mode_is_data (mymode)
	  );
    break;
  case iro_Phi:
           /* Phi: BB x dataM^n --> dataM */
    /* for some reason "<=" aborts. Is there a problem with get_store? */
    for (i=1; i < get_irn_arity(n); i++) {
      if (!is_Bad(in[i]))
	assert ( get_irn_mode(in[i]) == mymode );
    };
    assert ( mode_is_dataM(mymode) );
    break;
  case iro_Load:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert(
           /* Load: BB x M x P --> M x X x data */
           op1mode == mode_M && op2mode == mode_p
	  );
    assert ( mymode == mode_T );
    break;
  case iro_Store:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    op3mode = get_irn_mode(in[3]);
    assert(
           /* Load: BB x M x P x data --> M x X */
           op1mode == mode_M && op2mode == mode_p
           && mode_is_data (op3mode)
	  );
    assert(mymode == mode_T);
    break;
  case iro_Alloc:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    assert(
           /* Alloc: BB x M x Iu --> M x X x P */
           op1mode == mode_M && op2mode == mode_I
           && mymode == mode_T
	  );
    break;
  case iro_Free:
    op1mode = get_irn_mode(in[1]);
    op2mode = get_irn_mode(in[2]);
    op3mode = get_irn_mode(in[3]);
    assert(
           /* Free: BB x M x P x Iu --> M */
           op1mode == mode_M && op2mode == mode_p && op3mode == mode_I
           && mymode == mode_M
	  );
    break;
  case iro_Sync:
           /* Sync: BB x M^n --> M */
    for (i=1; i < get_irn_arity(n); i++) {
      assert ( get_irn_mode(in[i]) == mode_M );
    };
    assert ( mymode == mode_M );
    break;

  default: ;
  }
}

/*******************************************************************/
/* Verify the whole graph.                                         */
/*******************************************************************/

void
vrfy_wrap (ir_node *node, void *env) {
  irn_vrfy(node);
}

void
irg_vrfy (ir_graph *irg)
{
  irg_walk(irg->end, vrfy_wrap, NULL, NULL);
}
