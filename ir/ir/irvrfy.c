/*
 * Project:     libFIRM
 * File name:   ir/ir/irvrfy.c
 * Purpose:     Check irnodes for correctness.
 * Author:      Christian Schaefer
 * Modified by: Goetz Lindenmaier. Till Riedel
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irgraph_t.h"
# include "irvrfy.h"
# include "irgwalk.h"

#ifdef NDEBUG
/*
 * in RELEASE mode, returns ret if the expression expr evaluates to zero
 * in ASSERT mode, asserts the expression expr (and the string string).
 */
#define ASSERT_AND_RET(expr, string, ret)  		if (!(expr)) return (ret)

/*
 * in RELEASE mode, returns ret if the expression expr evaluates to zero
 * in ASSERT mode, executes blk if the expression expr evaluates to zero and asserts
 */
#define ASSERT_AND_RET_DBG(expr, string, ret, blk)  	if (!(expr)) return (ret)
#else
#define ASSERT_AND_RET(expr, string, ret)  		do { assert((expr) && string); if (!(expr)) return (ret); } while(0)
#define ASSERT_AND_RET_DBG(expr, string, ret, blk) 	do { if (!(expr)) { { blk } assert(0 && string); return (ret); } } while(0)
#endif

/* @@@ replace use of array "in" by access functions. */
ir_node **get_irn_in(ir_node *node);

bool opt_do_node_verification = 1;
void do_node_verification(bool b) {
  opt_do_node_verification = b;
}

/**
 * Prints a failure message for a binop
 */
static void show_binop_failure(ir_node *n, const char *text)
{
  ir_node *left  = get_binop_left(n);
  ir_node *right = get_binop_right(n);

  fprintf(stderr, "\nFIRM: irn_vrfy_irg() of node %ld %s%s(%s%s, %s%s) did not match (%s)\n",
      get_irn_node_nr(n),
      get_irn_opname(n), get_irn_modename(n),
      get_irn_opname(left), get_irn_modename(left),
      get_irn_opname(right), get_irn_modename(right),
      text);
}

/**
 * Prints a failure message for an unop
 */
static void show_unop_failure(ir_node *n, const char *text)
{
  ir_node *op  = get_unop_op(n);

  fprintf(stderr, "\nFIRM: irn_vrfy_irg() of node %ld %s%s(%s%s) did not match (%s)\n",
      get_irn_node_nr(n),
      get_irn_opname(n), get_irn_modename(n),
      get_irn_opname(op), get_irn_modename(op),
      text);
}

/**
 * Prints a failure message for a proj
 */
static void show_proj_failure(ir_node *n)
{
  ir_node *op  = get_Proj_pred(n);
  int proj     = get_Proj_proj(n);

  fprintf(stderr, "\nFIRM: irn_vrfy_irg() of node %ld %s%s %d(%s%s) failed\n" ,
      get_irn_node_nr(n),
      get_irn_opname(n), get_irn_modename(n), proj,
      get_irn_opname(op), get_irn_modename(op));
}

/**
 * Prints a failure message for a proj
 */
static void show_proj_failure_ent(ir_node *n, entity *ent)
{
  ir_node *op  = get_Proj_pred(n);
  int proj     = get_Proj_proj(n);
  ir_mode *m   = get_type_mode(get_entity_type(ent));

  fprintf(stderr, "\nFIRM: irn_vrfy_irg() of node %ld %s%s %d(%s%s) entity %s(type %s mode %s)failed\n" ,
      get_irn_node_nr(n),
      get_irn_opname(n), get_irn_modename(n), proj,
      get_irn_opname(op), get_irn_modename(op),
      get_entity_name(ent), get_type_name(get_entity_type(ent)),
      m ? get_mode_name(m) : "<no mode>");
}


/**
 * Show a node and a graph
 */
static void show_node_on_graph(ir_graph *irg, ir_node *n)
{
  entity *ent = get_irg_ent(irg);

  if (ent)
    fprintf(stderr, "\nFIRM: irn_vrfy_irg() of entity %s, node %ld %s%s\n",
      get_entity_name(ent),
      get_irn_node_nr(n), get_irn_opname(n), get_irn_modename(n));
  else
    fprintf(stderr, "\nFIRM: irn_vrfy_irg() of graph %p, node %ld %s%s\n",
      (void *)irg,
      get_irn_node_nr(n), get_irn_opname(n), get_irn_modename(n));
}

/**
 * Show call params
 */
static void show_call_param(ir_node *n, type *mt)
{
  int i;

  fprintf(stderr, "\nFIRM: irn_vrfy_irg() Call type-check failed: %s(", get_type_name(mt));
  for (i = 0; i < get_method_n_params(mt); ++i) {
    fprintf(stderr, "%s ", get_mode_name(get_type_mode(get_method_param_type(mt, i))));
  }
  fprintf(stderr, ") != CALL(");

  for (i = 0; i < get_Call_n_params(n); ++i) {
    fprintf(stderr, "%s ", get_mode_name(get_irn_mode(get_Call_param(n, i))));
  }
  fprintf(stderr, ")\n");

}

/**
 * Show return modes
 */
static void show_return_modes(ir_graph *irg, ir_node *n, type *mt, int i)
{
  entity *ent = get_irg_ent(irg);

  fprintf(stderr, "\nFIRM: irn_vrfy_irg() Return node %ld in entity \"%s\" mode %s different from type mode %s\n",
    get_irn_node_nr(n), get_entity_name(ent),
    get_mode_name(get_irn_mode(get_Return_res(n, i))),
    get_mode_name(get_type_mode(get_method_res_type(mt, i)))
  );
}

/**
 * Show return number of results
 */
static void show_return_nres(ir_graph *irg, ir_node *n, type *mt)
{
  entity *ent = get_irg_ent(irg);

  fprintf(stderr, "\nFIRM: irn_vrfy_irg() Return node %ld in entity \"%s\" has %d results different from type %d\n",
    get_irn_node_nr(n), get_entity_name(ent),
    get_Return_n_ress(n), get_method_n_ress(mt));
}

INLINE static int
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
      ASSERT_AND_RET_DBG(
          (
	   (proj == pns_initial_exec   && mode == mode_X) ||
           (proj == pns_global_store   && mode == mode_M) ||
           (proj == pns_frame_base     && mode_is_reference(mode)) ||
           (proj == pns_globals        && mode_is_reference(mode)) ||
           (proj == pns_args           && mode == mode_T) ||
	   (proj == pns_value_arg_base && mode_is_reference(mode))
	  ),
          "wrong Proj from Start", 0,
	  show_proj_failure(p);
      );
      break;

    case iro_Cond:
      ASSERT_AND_RET_DBG(
        (proj >= 0 && mode == mode_X),
	"wrong Proj from Cond", 0,
	show_proj_failure(p);
      );
      break;

    case iro_Raise:
      ASSERT_AND_RET_DBG(
        ((proj == pn_Raise_X && mode == mode_X) || (proj == pn_Raise_M && mode == mode_M)),
        "wrong Proj from Raise", 0,
	show_proj_failure(p);
      );
      break;

    case iro_InstOf:
      ASSERT_AND_RET_DBG(
	(proj >= 0 && mode == mode_X),
	"wrong Proj from InstOf", 0,
	show_proj_failure(p);
      );
      break;

    case iro_Call:
      ASSERT_AND_RET_DBG(
        ((proj == pn_Call_M_regular        && mode == mode_M) ||
         (proj == pn_Call_X_except         && mode == mode_X) ||
         (proj == pn_Call_T_result         && mode == mode_T) ||
         (proj == pn_Call_M_except         && mode == mode_M) ||
	 (proj == pn_Call_P_value_res_base && mode == mode_P)),
        "wrong Proj from Call", 0,
        show_proj_failure(p);
      );
      break;

    case iro_Quot:
      ASSERT_AND_RET_DBG(
        ((proj == pn_Quot_M        && mode == mode_M) ||
         (proj == pn_Quot_X_except && mode == mode_X) ||
         (proj == pn_Quot_res      && mode_is_float(mode))),
        "wrong Proj from Quot", 0,
	show_proj_failure(p);
      );
      break;

    case iro_DivMod:
      ASSERT_AND_RET_DBG(
        ((proj == pn_DivMod_M        && mode == mode_M) ||
         (proj == pn_DivMod_X_except && mode == mode_X) ||
         (proj == pn_DivMod_res_div  && mode_is_int(mode)) ||
         (proj == pn_DivMod_res_mod  && mode_is_int(mode))),
        "wrong Proj from DivMod", 0,
	show_proj_failure(p);
      );
      break;

    case iro_Div:
      ASSERT_AND_RET_DBG(
        ((proj == pn_Div_M        && mode == mode_M) ||
         (proj == pn_Div_X_except && mode == mode_X) ||
         (proj == pn_Div_res      && mode_is_int(mode))),
        "wrong Proj from Div or Mod", 0,
	show_proj_failure(p);
      );
      break;

    case iro_Mod:
      ASSERT_AND_RET_DBG(
        ((proj == pn_Mod_M        && mode == mode_M) ||
         (proj == pn_Mod_X_except && mode == mode_X) ||
         (proj == pn_Mod_res      && mode_is_int(mode))),
        "wrong Proj from Div or Mod", 0,
	show_proj_failure(p);
      );
      break;

    case iro_Cmp:
      ASSERT_AND_RET_DBG(
        (proj >= 0 && proj <= 15 && mode == mode_b),
        "wrong Proj from Cmp", 0,
	show_proj_failure(p);
      );
      break;

    case iro_Load:
      if (proj == pn_Load_res) {
	ir_node *ptr = get_Load_ptr(pred);
	entity *ent = NULL;
	if (get_irn_op(ptr) == op_Sel) {
	  ent = get_Sel_entity(ptr);
	} /*
        We may not test this, after lowering and optimization the Const can
        have an unexpected type.
    else if ((get_irn_op(ptr) == op_Const) &&
		   tarval_is_entity(get_Const_tarval(ptr))) {
	  ent = get_tarval_entity(get_Const_tarval(ptr));
	} */
	if (ent) {
	  ASSERT_AND_RET_DBG(
	    (mode == get_type_mode(get_entity_type(ent))),
	    "wrong data Proj from Load, entity type_mode failed", 0,
	    show_proj_failure_ent(p, ent);
	  );
	}
	else {
	  ASSERT_AND_RET_DBG(
	    mode_is_data(mode),
	   "wrong data Proj from Load", 0,
	   show_proj_failure(p);
	 );
	}
      } else {
	ASSERT_AND_RET_DBG(
	  ((proj == pn_Load_M        && mode == mode_M) ||
	   (proj == pn_Load_X_except && mode == mode_X)),
          "wrong Proj from Load", 0,
	  show_proj_failure(p);
	);
      }
      break;

    case iro_Store:
      ASSERT_AND_RET_DBG(
        ((proj == pn_Store_M        && mode == mode_M) ||
         (proj == pn_Store_X_except && mode == mode_X)),
        "wrong Proj from Store", 0,
	show_proj_failure(p);
      );
      break;

    case iro_Alloc:
      ASSERT_AND_RET_DBG(
        (
         (proj == pn_Alloc_M        && mode == mode_M) ||
         (proj == pn_Alloc_X_except /* && mode == mode_X*/) ||
         (proj == pn_Alloc_res      && mode_is_reference(mode))
        ),
        "wrong Proj from Alloc", 0,
        show_proj_failure(p);
      );
      break;

    case iro_Proj:
      {
        type *mt; /* A method type */
        pred = skip_nop(get_Proj_pred(pred));
        ASSERT_AND_RET((get_irn_mode(pred) == mode_T), "Proj from something not a tuple", 0);
        switch (get_irn_opcode(pred))
        {
          case iro_Start:
            {
              ASSERT_AND_RET(
                  (proj >= 0 && mode_is_data(mode)),
                  "wrong Proj from Proj from Start", 0);
              mt = get_entity_type(get_irg_ent(irg));
              ASSERT_AND_RET(
                (proj < get_method_n_params(mt)),
                "More Projs for args than args in type", 0
	      );
              if ((mode_is_reference(mode)) && is_compound_type(get_method_param_type(mt, proj)))
                /* value argument */ break;

              ASSERT_AND_RET(
                  (mode == get_type_mode(get_method_param_type(mt, proj))),
                  "Mode of Proj from Start doesn't match mode of param type.", 0);
            }
            break;

          case iro_Call:
            {
              ASSERT_AND_RET(
                  (proj >= 0 && mode_is_data(mode)),
                  "wrong Proj from Proj from Call", 0);
              mt = get_Call_type(pred);
              ASSERT_AND_RET(
                  (proj < get_method_n_ress(mt)),
                  "More Projs for results than results in type.", 0);
              if ((mode_is_reference(mode)) && is_compound_type(get_method_res_type(mt, proj)))
                /* value result */ break;

              ASSERT_AND_RET(
                  (mode == get_type_mode(get_method_res_type(mt, proj))),
                  "Mode of Proj from Call doesn't match mode of result type.", 0);
            }
            break;

          case iro_Tuple:
            /* We don't test */
            break;

          default:
            ASSERT_AND_RET(0, "Unknown opcode", 0);
        }
        break;

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

    default:
      ASSERT_AND_RET(0, "Unknown opcode", 0);
  }

  /* all went ok */
  return 1;
}

int irn_vrfy_irg(ir_node *n, ir_graph *irg)
{
  int i;
  int opcode, opcode1;
  ir_mode *mymode, *op1mode = NULL, *op2mode, *op3mode;
  int op_is_symmetric = 1;  /*  0: asymmetric
				1: operands have identical modes
				2: modes of operands == mode of this node */
  type *mt; /* A method type */

  ir_node **in;

  if (!opt_do_node_verification) return 1;

  if (! interprocedural_view) {
    /*
     * do NOT check placement in interprocedural view, as we don't always know
     * the "right" graph ...
     */
    ASSERT_AND_RET_DBG(
      node_is_in_irgs_storage(irg, n),
      "Node is not stored on proper IR graph!", 0,
      show_node_on_graph(irg, n);
    );
  }

  opcode = get_irn_opcode (n);

  /* We don't want to test nodes whose predecessors are Bad or Unknown,
     as we would have to special case that for each operation. */
  if (opcode != iro_Phi && opcode != iro_Block)
    for (i = 0; i < get_irn_arity(n); i++) {
      opcode1 = get_irn_opcode(get_irn_n(n, i));
      if (opcode1 == iro_Bad /*|| opcode1 == iro_Unknown*/)  /* GL: for analyses mode must be correct. */
        return 1;
    }

  mymode = get_irn_mode (n);
  in = get_irn_in (n);

  switch (opcode)
  {

    case iro_Block:
      for (i = 0; i < get_Block_n_cfgpreds(n); ++i) {
	ir_node *pred =  get_Block_cfgpred(n, i);
	ASSERT_AND_RET(
	  (is_Bad(pred)     ||
	   is_Unknown(pred) ||
	   (get_irn_mode(pred) == mode_X)
	  ), "Block node", 0);
      }
      // End block may only have Return, Raise or fragile ops as preds.
      if (n == get_irg_end_block(irg))
	for (i = 0; i < get_Block_n_cfgpreds(n); ++i) {
	  ir_node *pred =  skip_Proj(get_Block_cfgpred(n, i));
	  if (is_Proj(pred) || get_irn_op(pred) == op_Tuple)
	    break;   // We can not test properly.  How many tuples are there?
	  ASSERT_AND_RET(((get_irn_op(pred) == op_Return) ||
			  is_Bad(pred)                    ||
			  (get_irn_op(pred) == op_Raise)  ||
			  is_fragile_op(pred)               ),
			 "End Block node", 0);
	}
      // irg attr must == graph we are in.
      if (! interprocedural_view) {
	ASSERT_AND_RET(((get_irn_irg(n) && get_irn_irg(n) == irg)), "Block node has wrong irg attribute", 0);
      }

      break;

    case iro_Start:
      ASSERT_AND_RET(
          /* Start: BB --> X x M x ref x data1 x ... x datan x ref */
          mymode == mode_T, "Start node", 0
          );
      break;

    case iro_Jmp:
      ASSERT_AND_RET(
          /* Jmp: BB --> X */
          mymode == mode_X, "Jmp node", 0
          );
      break;

    case iro_Break:
      ASSERT_AND_RET(
          /* Jmp: BB --> X */
          mymode == mode_X, "Jmp node", 0
          );
      break;

    case iro_Cond:
      op1mode = get_irn_mode(in[1]);
      ASSERT_AND_RET(
          /* Cond: BB x b --> X x X */
          (op1mode == mode_b ||
           /* Cond: BB x int --> X^n */
           mode_is_int(op1mode) ),  "Cond node", 0
          );
      ASSERT_AND_RET(mymode == mode_T, "Cond mode is not a tuple", 0);
      break;

    case iro_Return:
      op1mode = get_irn_mode(in[1]);
      /* Return: BB x M x data1 x ... x datan --> X */
      /* printf("mode: %s, code %s\n", ID_TO_STR(n->mode->name), ID_TO_STR(n->op->name));*/
      ASSERT_AND_RET( op1mode == mode_M, "Return node", 0 );  /* operand M */
      for (i=2; i < get_irn_arity(n); i++) {
        ASSERT_AND_RET( mode_is_data(get_irn_mode(in[i])), "Return node", 0 );  /* operand datai */
      };
      ASSERT_AND_RET( mymode == mode_X, "Result X", 0 );   /* result X */
      /* Compare returned results with result types of method type */
      mt = get_entity_type(get_irg_ent(irg));
      ASSERT_AND_RET_DBG( get_Return_n_ress(n) == get_method_n_ress(mt),
        "Number of results for Return doesn't match number of results in type.", 0,
	show_return_nres(irg, n, mt););
      for (i = 0; i < get_Return_n_ress(n); i++)
        ASSERT_AND_RET_DBG(
          get_irn_mode(get_Return_res(n, i)) == get_type_mode(get_method_res_type(mt, i)),
          "Mode of result for Return doesn't match mode of result type.", 0,
	   show_return_modes(irg, n, mt, i););
      break;

    case iro_Raise:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET(
          /* Sel: BB x M x ref --> X x M */
          op1mode == mode_M && mode_is_reference(op2mode) &&
          mymode == mode_T, "Raise node", 0
          );
      break;

    case iro_Const:
      ASSERT_AND_RET(
          /* Const: BB --> data */
          (mode_is_data (mymode) ||
           mymode == mode_b)      /* we want boolean constants for static evaluation */
          ,"Const node", 0        /* of Cmp. */
          );
      break;

    case iro_SymConst:
      ASSERT_AND_RET(
          /* SymConst: BB --> int*/
          (mode_is_int(mymode) ||
           /* SymConst: BB --> ref */
           mode_is_reference(mymode))
          ,"SymConst node", 0);
      break;

    case iro_Sel:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET(
          /* Sel: BB x M x ref x int^n --> ref */
          (op1mode == mode_M && op2mode == mymode && mode_is_reference(mymode)),
	  "Sel node", 0
          );
      for (i=3; i < get_irn_arity(n); i++)
      {
        ASSERT_AND_RET(mode_is_int(get_irn_mode(in[i])), "Sel node", 0);
      }
      break;

    case iro_InstOf:
      ASSERT_AND_RET(mode_T == mymode, "mode of Instof is not a tuple", 0);
      ASSERT_AND_RET(mode_is_data(op1mode), "Instof not on data", 0);
      break;

    case iro_Call:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      /* Call: BB x M x ref x data1 x ... x datan
         --> M x datan+1 x ... x data n+m */
      ASSERT_AND_RET( op1mode == mode_M && mode_is_reference(op2mode), "Call node", 0 );  /* operand M x ref */
      for (i=3; i < get_irn_arity(n); i++) {
        ASSERT_AND_RET( mode_is_data(get_irn_mode(in[i])), "Call node", 0 );  /* operand datai */
      };
      ASSERT_AND_RET( mymode == mode_T, "Call result not a tuple", 0 );   /* result T */
      /* Compare arguments of node with those of type */
      mt = get_Call_type(n);

      if (get_method_variadicity(mt) == variadicity_variadic) {
        ASSERT_AND_RET_DBG(
            get_Call_n_params(n) >= get_method_n_params(mt),
            "Number of args for Call doesn't match number of args in variadic type.",
            0,
	    fprintf(stderr, "Call has %d params, method %s type %d\n",
	      get_Call_n_params(n), get_type_name(mt), get_method_n_params(mt));
	    );
      }
      else {
        ASSERT_AND_RET(
            get_Call_n_params(n) == get_method_n_params(mt),
            "Number of args for Call doesn't match number of args in non variadic type.",
            0);
      }

      for (i = 0; i < get_method_n_params(mt); i++) {
        ASSERT_AND_RET_DBG(
            get_irn_mode(get_Call_param(n, i)) == get_type_mode(get_method_param_type(mt, i)),
            "Mode of arg for Call doesn't match mode of arg type.", 0,
	    show_call_param(n, mt);
	    );
      }
      break;

    case iro_Add:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
          (
           /* common Add: BB x numP x numP --> numP */
	   (op1mode == mymode && op2mode == op1mode && mode_is_numP(mymode)) ||
           /* Pointer Add: BB x ref x int --> ref */
           (mode_is_reference(op1mode) && mode_is_int(op2mode) && op1mode == mymode) ||
           /* Pointer Add: BB x int x ref --> ref */
           (mode_is_int(op1mode) && op2mode == mymode && mode_is_reference(mymode))
	  ),
          "Add node", 0,
	  show_binop_failure(n, "/* common Add: BB x numP x numP --> numP */ |\n"
	                        "/* Pointer Add: BB x ref x int --> ref */   |\n"
                                "/* Pointer Add: BB x int x ref --> ref */");
          );
      if (mode_is_reference(op1mode) != mode_is_reference(op2mode)) {
        /* BB x ref x int --> ref or BB x int x ref --> ref */
        op_is_symmetric = 0;
      } else {
        /* BB x num x num --> num or BB x ref x ref */
        op_is_symmetric = 2;
      }
      break;

    case iro_Sub:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
          /* common Sub: BB x numP x numP --> numP */
          ((mymode ==op1mode && mymode == op2mode && mode_is_numP(op1mode)) ||
           /* Pointer Sub: BB x ref x int --> ref */
           (op1mode == mymode && mode_is_int(op2mode) && mode_is_reference(mymode)) ||
           /* Pointer Sub: BB x int x ref --> ref */
           (mode_is_int(op1mode) && op2mode == mymode && mode_is_reference(mymode)) ||
           /* Pointer Sub: BB x ref x ref --> int */
           (op1mode == op2mode && mode_is_reference(op2mode) && mode_is_int(mymode))),
          "Sub node", 0,
	  show_binop_failure(n, "/* common Sub: BB x numP x numP --> numP */ |\n"
	                        "/* Pointer Sub: BB x ref x int --> ref */   |\n"
	                        "/* Pointer Sub: BB x int x ref --> ref */   |\n"
	                        "/* Pointer Sub: BB x ref x ref --> int */" );
          );
      if (mode_is_reference(op1mode) != mode_is_reference(op2mode)) {
        op_is_symmetric = 0;
      } else {
        op_is_symmetric = 2;
      }
      break;

    case iro_Minus:
      op1mode = get_irn_mode(in[1]);
      ASSERT_AND_RET_DBG(
          /* Minus: BB x float --> float */
          op1mode == mymode && get_mode_sort(op1mode) == irms_float_number, "Minus node", 0,
	  show_unop_failure(n , "/* Minus: BB x float --> float */");
          );
      op_is_symmetric = 2;
      break;

    case iro_Mul:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
          /* Mul: BB x int1 x int1 --> int2 */
          ((mode_is_int(op1mode)   && op2mode == op1mode && mode_is_int(mymode)) ||
	   (mode_is_float(op1mode) && op2mode == op1mode && mymode == op1mode)),
          "Mul node",0,
	  show_binop_failure(n, "/* Mul: BB x int1 x int1 --> int2 */");
          );
      op_is_symmetric = 2;
      break;

    case iro_Quot:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      op3mode = get_irn_mode(in[3]);
      ASSERT_AND_RET_DBG(
          /* Quot: BB x M x float x float --> M x X x float */
          op1mode == mode_M && op2mode == op3mode &&
          get_mode_sort(op2mode) == irms_float_number &&
          mymode == mode_T,
          "Quot node",0,
	  show_binop_failure(n, "/* Quot: BB x M x float x float --> M x X x float */");
          );
      op_is_symmetric = 2;
      break;

    case iro_DivMod:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      op3mode = get_irn_mode(in[3]);
      ASSERT_AND_RET(
          /* DivMod: BB x M x int x int --> M x X x int x int */
          op1mode == mode_M &&
          mode_is_int(op2mode) &&
          op3mode == op2mode &&
          mymode == mode_T,
          "DivMod node", 0
          );
      op_is_symmetric = 1;
      break;

    case iro_Div:
    case iro_Mod:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      op3mode = get_irn_mode(in[3]);
      ASSERT_AND_RET(
          /* Div or Mod: BB x M x int x int --> M x X x int */
          op1mode == mode_M &&
          op2mode == op3mode &&
          mode_is_int(op2mode) &&
          mymode == mode_T,
          "Div or Mod node", 0
          );
      op_is_symmetric = 1;
      break;

    case iro_Abs:
      op1mode = get_irn_mode(in[1]);
      ASSERT_AND_RET_DBG(
	/* Abs: BB x num --> num */
	op1mode == mymode &&
	mode_is_num (op1mode),
	"Abs node", 0,
	show_unop_failure(n, "/* Abs: BB x num --> num */");
      );
      op_is_symmetric = 2;
      break;

    case iro_And:
    case iro_Or:
    case iro_Eor:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
	/* And or Or or Eor: BB x int x int --> int */
	mode_is_int(mymode) &&
	op2mode == op1mode &&
	mymode == op2mode,
	"And, Or or Eor node", 0,
	show_binop_failure(n, "/* And or Or or Eor: BB x int x int --> int */");
      );
      op_is_symmetric = 2;
      break;

    case iro_Not:
      op1mode = get_irn_mode(in[1]);
      ASSERT_AND_RET_DBG(
	/* Not: BB x int --> int */
	mode_is_int(mymode) &&
	mymode == op1mode,
	"Not node", 0,
	show_unop_failure(n, "/* Not: BB x int --> int */");
      );
      op_is_symmetric = 2;
      break;


    case iro_Cmp:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
	/* Cmp: BB x datab x datab --> b16 */
	mode_is_data (op1mode) &&
	op2mode == op1mode &&
	mymode == mode_T,
	"Cmp node", 0,
	show_binop_failure(n, "/* Cmp: BB x datab x datab --> b16 */");
      );
      break;

    case iro_Shl:
    case iro_Shr:
    case iro_Shrs:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
	/* Shl, Shr or Shrs: BB x int x int_u --> int */
	mode_is_int(op1mode) &&
	mode_is_int(op2mode) &&
	!mode_is_signed(op2mode) &&
	mymode == op1mode,
	"Shl, Shr, Shr or Rot node", 0,
	show_binop_failure(n, "/* Shl, Shr or Shrs: BB x int x int_u --> int */");
      );
      break;

    case iro_Rot:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
	/* Rot: BB x int x int --> int */
	mode_is_int(op1mode) &&
	mode_is_int(op2mode) &&
	mymode == op1mode,
	"Rot node", 0,
	show_binop_failure(n, "/* Rot: BB x int x int --> int */");
      );
      break;

    case iro_Conv:
      op1mode = get_irn_mode(in[1]);
      ASSERT_AND_RET_DBG(
	/* Conv: BB x datab1 --> datab2 */
	mode_is_datab(op1mode) && mode_is_data(mymode),
	"Conv node", 0,
	show_unop_failure(n, "/* Conv: BB x datab1 --> datab2 */");
      );
      break;

    case iro_Cast:
      op1mode = get_irn_mode(in[1]);
      ASSERT_AND_RET_DBG(
	/* Conv: BB x datab1 --> datab2 */
	mode_is_data(op1mode) && op1mode == mymode,
	"Cast node", 0,
	show_unop_failure(n, "/* Conv: BB x datab1 --> datab2 */");
      );
      break;

    case iro_Phi:
      /* Phi: BB x dataM^n --> dataM */
      /* for some reason "<=" aborts. int there a problem with get_store? */
      for (i=1; i < get_irn_arity(n); i++) {
        if (!is_Bad(in[i]) && (get_irn_op(in[i]) != op_Unknown))
          ASSERT_AND_RET( get_irn_mode(in[i]) == mymode, "Phi node", 0);
      };
      ASSERT_AND_RET( mode_is_dataM(mymode), "Phi node", 0 );
      break;

    case iro_Load:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET(
          /* Load: BB x M x ref --> M x X x data */
          op1mode == mode_M && mode_is_reference(op2mode),
          "Load node", 0
          );
      ASSERT_AND_RET( mymode == mode_T, "Load node", 0 );
      break;

    case iro_Store:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      op3mode = get_irn_mode(in[3]);
      ASSERT_AND_RET(
          /* Load: BB x M x ref data --> M x X */
          op1mode == mode_M && mode_is_reference(op2mode) && mode_is_data(op3mode),
          "Store node", 0
          );
      ASSERT_AND_RET(mymode == mode_T, "Store node", 0);
      break;

    case iro_Alloc:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
	/* Alloc: BB x M x int_u --> M x X x ref */
	op1mode == mode_M &&
	mode_is_int(op2mode) &&
	!mode_is_signed(op2mode) &&
	mymode == mode_T,
	"Alloc node", 0,
	show_binop_failure(n, "/* Alloc: BB x M x int_u --> M x X x ref */");
      );
      break;

    case iro_Free:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
	/* Free: BB x M x ref --> M */
	op1mode == mode_M && mode_is_reference(op2mode) &&
	mymode == mode_M,
	"Free node", 0,
	show_binop_failure(n, "/* Free: BB x M x ref --> M */");
      );
      break;

    case iro_Sync:
      /* Sync: BB x M^n --> M */
      for (i=1; i < get_irn_arity(n); i++) {
        ASSERT_AND_RET( get_irn_mode(in[i]) == mode_M, "Sync node", 0 );
      };
      ASSERT_AND_RET( mymode == mode_M, "Sync node", 0 );
      break;

    case iro_Proj:
      return vrfy_Proj_proj(n, irg);
      break;

    case iro_Confirm:
      op1mode = get_irn_mode(in[1]);
      op2mode = get_irn_mode(in[2]);
      ASSERT_AND_RET_DBG(
	/* Confirm: BB x T x T --> T */
	op1mode == mymode &&
	op2mode == mymode,
	"Confirm node", 0,
	show_binop_failure(n, "/* Confirm: BB x T x T --> T */");
      );
      break;

    default:
      break;
  }

  /* All went ok */
  return 1;
}

int irn_vrfy(ir_node *n)
{
  int res = 1;
#ifdef DEBUG_libfirm
  res = irn_vrfy_irg(n, current_ir_graph);
#endif
  return res;
}

/*******************************************************************/
/* Verify the whole graph.                                         */
/*******************************************************************/

static void vrfy_wrap(ir_node *node, void *env)
{
  int *res = env;

  *res = irn_vrfy(node);
}

int irg_vrfy(ir_graph *irg)
{
  int res = 1;
#ifdef DEBUG_libfirm
  ir_graph *rem;

  rem = current_ir_graph;
  current_ir_graph = irg;

  assert(get_irg_pinned(irg) == pinned);

  irg_walk(irg->end, vrfy_wrap, NULL, &res);

  current_ir_graph = rem;
#endif
  return res;
}
