/*
 * Project:     libFIRM
 * File name:   testprograms/strength_red_example.c
 * Purpose:     Shows how strength red works
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# include <stdio.h>
# include <string.h>

# include "firm.h"

# include "irvrfy.h"
# include "irdump.h"


/**
*  This file constructs the ir for the following pseudo-program:
*
*  int a[10];
*
*  void main(void) {
*    int i = 0;
*
*    while (i < 10)  {
*       a[i] = 19;
*       i++
*    }
*  }
**/

#define CLASSNAME "STRENGTH_RED_EXAMPLE"
#define METHODNAME1 "STRENGTH_RED_EXAMPLE_m1"
#define METHODNAME2 "STRENGTH_RED_EXAMPLE_m2"
#define METHODNAME3 "STRENGTH_RED_EXAMPLE_m3"
#define METHODNAME4 "STRENGTH_RED_EXAMPLE_m4"
#define METHODNAME5 "STRENGTH_RED_EXAMPLE_m5"
#define METHODTPNAME "STRENGTH_RED_EXAMPLE_meth_tp"
#define NRARGS 1
#define NRES 1
#define L_BOUND 0
#define U_BOUND 10
#define N_DIMS 1
  /** The type int.   **/
#define PRIM_NAME "int"



static int i_pos = 0;
static int arr_pos = 1;
static type *typ;

static ir_node *r1, *f, *r;

typedef enum {
  loop_forward,
  loop_backward,
} loop_dir_t;

static void function_begin(type *owner, type *mtp, char *fct_name, loop_dir_t loop_dir) {
  symconst_symbol sym;
  ir_node *x, *t, *cmp;
  /* The entity for the procedure */
  entity *ent = new_entity (owner,  new_id_from_str (fct_name), mtp);
  /* The parameter and result types of the procedure. */
  set_method_param_type(mtp, 0, typ);
  set_method_res_type(mtp, 0, typ);

  /* make type infromation for the array */
  type *array_type = new_type_array(new_id_from_chars("array", 5),N_DIMS, typ);

  /* set the bounds for the array */
  set_array_bounds(array_type, 0,
		   new_Const(mode_Iu, new_tarval_from_long (L_BOUND, mode_Iu)),
		   new_Const(mode_Iu, new_tarval_from_long (U_BOUND, mode_Iu)));
  /* The array is an entity of the global typ */
  entity *array_ent = new_entity( owner, new_id_from_chars("a", 1), array_type);

  /** The code of the procedure **/

  /* Generates start and end blocks and nodes, and a first, initial block */
#define NRLOCS 2
  new_ir_graph (ent, NRLOCS);

  /* The value position used for: */
  i_pos = 0;

  /* Generate the constant and assign it to b. The assignment is resolved to a
     dataflow edge. */
  if (loop_dir == loop_forward) {
    set_value (i_pos, new_Const (mode_Is, new_tarval_from_long (0, mode_Is)));
  } else {
    set_value (i_pos, new_Const (mode_Is, new_tarval_from_long (10, mode_Is)));
  }
  sym.entity_p =  array_ent ;
  set_value (arr_pos, new_SymConst(sym, symconst_addr_ent));

  x = new_Jmp ();

  /* We know all predecessors of the block and all set_values and set_stores are
     preformed.   We can mature the block.  */
  mature_immBlock (get_irg_current_block(current_ir_graph));

  /* Generate a conditional branch */
  r1 = new_immBlock();
  add_immBlock_pred(get_irg_current_block(current_ir_graph), x);

  if (loop_dir == loop_forward) {
    cmp = new_Cmp(new_Const (mode_Is, new_tarval_from_long(10, mode_Is)),
		  get_value(i_pos, mode_Is));
    x = new_Cond (new_Proj(cmp, mode_b, Gt));
  } else {
    cmp = new_Cmp(new_Const (mode_Is, new_tarval_from_long(0, mode_Is)),
		  get_value(i_pos, mode_Is));
    x = new_Cond (new_Proj(cmp, mode_b, Lt));
  }
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate and fill the loop body block */
  r = new_immBlock ();
  add_immBlock_pred (r, t);
}

static void function_end(ir_node *b) {
  ir_node *x = new_Jmp ();
  mature_immBlock (r);
  add_immBlock_pred(r1, x);


  new_immBlock();
  add_immBlock_pred(get_cur_block(), f);
  mature_immBlock (get_cur_block());
  /* The Return statement */
  {
    ir_node *in[1], *store ;
    in[0] = b;
    store = get_store();

     x = new_Return (store, 1, in);
  }
  add_immBlock_pred(get_irg_end_block(current_ir_graph), x);

  mature_immBlock (r1);
  /* finalize the end block generated in new_ir_graph() */
  mature_immBlock (get_irg_end_block(current_ir_graph));
}

int
main(void)
{
  ir_graph *irg;
  type *owner;
  entity *ent, *array_ent;
  type *proc_tp, *array_type; /* type information for the method main */
  ir_node *x,*x1 ,  *r, *t, *f, *f1, *t1, *cmp, *r1, *r2;
  int i_pos;

  printf("\nCreating an IR graph: IF_EXAMPLE...\n");

  init_firm (NULL);

  do_node_verification(NODE_VERIFICATION_REPORT);

  typ = new_type_primitive(new_id_from_chars(PRIM_NAME, strlen(PRIM_NAME)), mode_Is);

  /** The global array variable a **/


  /** Type information for the procedure **/
  owner = get_glob_type();
  /* Type information for the procedure */
  proc_tp = new_type_method(new_id_from_chars(METHODTPNAME, strlen(METHODTPNAME)),
			      NRARGS, NRES);
  set_method_param_type(proc_tp, 0, typ);
  set_method_res_type(proc_tp, 0, typ);


  /* --------------------------------------------------------------------- */

  /* The entity for the procedure */
  ent = new_entity (owner,
                    new_id_from_str (METHODNAME1),
                    proc_tp);
  /* The parameter and result types of the procedure. */

  /* make type infromation for the array */
  array_type = new_type_array(new_id_from_chars("array", 5),N_DIMS, typ);

  /* set the bounds for the array */
  set_array_bounds(array_type, 0,
		   new_Const(mode_Iu, new_tarval_from_long (L_BOUND, mode_Iu)),
		   new_Const(mode_Iu, new_tarval_from_long (U_BOUND, mode_Iu)));
  /* The array is an entity of the global typ */
  array_ent = new_entity( owner, new_id_from_chars("a", 1), array_type);

  /** The code of the procedure **/


  /* Generates start and end blocks and nodes, and a first, initial block */
#undef NRLOCS
#define NRLOCS 1
  irg = new_ir_graph (ent, NRLOCS);

  /* The value position used for: */
  i_pos = 0;

  /* Generate the constant and assign it to b. The assignment is resovled to a
     dataflow edge. */
  set_value (i_pos, new_Const (mode_Is, new_tarval_from_long (0, mode_Is)));
  x = new_Jmp ();

  /* We know all predecessors of the block and all set_values and set_stores are
     preformed.   We can mature the block.  */
   mature_immBlock (get_irg_current_block(irg));

  /* Generate a conditional branch */
  r1 = new_immBlock();
  add_immBlock_pred(get_irg_current_block(irg), x);
  cmp = new_Cmp(new_Const (mode_Is, new_tarval_from_long(10, mode_Is)),
		get_value(i_pos, mode_Is));
  x = new_Cond (new_Proj(cmp, mode_b, Gt));
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate and fill the then block */
  r = new_immBlock ();
  add_immBlock_pred (r, t);

  ir_node *b, *c, *d, *res;
  symconst_symbol sym;
  c = new_Const (mode_Is, new_tarval_from_long (1, mode_Is));
  b = new_Const (mode_Is, new_tarval_from_long (4, mode_Is));
  sym.entity_p =  array_ent ;
  d = new_SymConst(sym, symconst_addr_ent);
  res = new_Add(d, new_Mul(get_value(i_pos, mode_Is), b, mode_Is), mode_P);
  set_store (new_Proj (new_Store (get_store (), res, new_Const (mode_Is,
				  new_tarval_from_long (19,mode_Is))),
		       mode_M, 0));
  set_value (i_pos, new_Add(get_value(i_pos, mode_Is), c , mode_Is));

  x = new_Jmp ();
  mature_immBlock (r);

  add_immBlock_pred(r1, x);
  mature_immBlock (r1);

  r2 = new_immBlock();
  add_immBlock_pred(get_irg_current_block(irg), f);
  cmp = new_Cmp(new_Const (mode_Is, new_tarval_from_long(0, mode_Is)),get_value(i_pos, mode_Is));
  x = new_Cond (new_Proj(cmp, mode_b, Lt));
  f1 = new_Proj (x, mode_X, 0);
  t1 = new_Proj (x, mode_X, 1);

  ir_node *block = new_immBlock();
  add_immBlock_pred(block, t1);

  res = new_Add(d, new_Mul(get_value(i_pos, mode_Is), b, mode_Is), mode_P);
  set_store (new_Proj (new_Store (get_store (), res, new_Const (mode_Is,
                       new_tarval_from_long (19, mode_Is))), mode_M, 0));
  set_value (i_pos, new_Sub(get_value(i_pos, mode_Is), c , mode_Is));

  x1 = new_Jmp ();

  mature_immBlock (block);

  add_immBlock_pred(r2, x1);
  mature_immBlock (r2);

  block = new_immBlock();
  add_immBlock_pred(block, f1);
  mature_immBlock (block);
  /* The Return statement */
  {
    ir_node *in[1], *store ;
    in[0] = get_value (i_pos, mode_Is);
    store = get_store();

     x = new_Return (store, 1, in);
  }
  add_immBlock_pred(get_irg_end_block(irg), x);

  /* finalize the end block generated in new_ir_graph() */
  mature_immBlock (get_irg_end_block(irg));


  /* -------------------------------------------------------------------------------- */

  function_begin(owner, proc_tp, METHODNAME2, loop_forward);

  c = new_Const (mode_Is, new_tarval_from_long (1, mode_Is));
  b = new_Const (mode_Is, new_tarval_from_long (4, mode_Is));

  res = new_Add(get_value(arr_pos, mode_P), new_Mul(get_value(i_pos, mode_Is), b, mode_Is), mode_P);
  set_store (new_Proj (new_Store (get_store (), res, new_Const (mode_Is,
				  new_tarval_from_long (19, mode_Is))),
		       mode_M, 0));

  set_value (i_pos, new_Add(get_value(i_pos, mode_Is), c, mode_Is));

  function_end(b);

  /* -------------------------------------------------------------------------- */

  function_begin(owner, proc_tp, METHODNAME3, loop_backward);

  c = new_Const (mode_Is, new_tarval_from_long (1, mode_Is));
  b = new_Const (mode_Is, new_tarval_from_long (4, mode_Is));


  res = new_Add(get_value(arr_pos, mode_P), new_Mul(get_value(i_pos, mode_Is), b, mode_Is), mode_P);
  set_store (new_Proj (new_Store (get_store (), res, get_value(i_pos, mode_Is)), mode_M, 0));

  set_value (i_pos, new_Sub(get_value(i_pos, mode_Is), c, mode_Is));

  function_end(b);

  /* -------------------------------------------------------------------------- */

  function_begin(owner, proc_tp, METHODNAME4, loop_forward);

  c = new_Const (mode_Is, new_tarval_from_long (1, mode_Is));
  b = new_Const (mode_Is, new_tarval_from_long (4, mode_Is));

  set_value (i_pos, new_Add(get_value(i_pos, mode_Is), c, mode_Is));

  res = new_Add(get_value(arr_pos, mode_P), new_Mul(get_value(i_pos, mode_Is), b, mode_Is), mode_P);
  set_store (new_Proj (new_Store (get_store (), res,get_value(i_pos, mode_Is)),
		       mode_M, 0));

  function_end(b);

  /* -------------------------------------------------------------------------- */

  function_begin(owner, proc_tp, METHODNAME5, loop_backward);

  c = new_Const (mode_Is, new_tarval_from_long (1, mode_Is));
  b = new_Const (mode_Is, new_tarval_from_long (4, mode_Is));

  set_value (i_pos, new_Sub(get_value(i_pos, mode_Is), c, mode_Is));


  res = new_Add(get_value(arr_pos, mode_P), new_Mul(get_value(i_pos, mode_Is), b, mode_Is), mode_P);
  set_store (new_Proj (new_Store (get_store (), res, new_Const (mode_Is,
				  new_tarval_from_long (19, mode_Is))),
		       mode_M, 0));

  function_end(b);

  /* -------------------------------------------------------------------------- */

  int i, n_irgs = get_irp_n_irgs();

  printf("Done building the graph.  Dumping and optimizing it.\n");
  dump_consts_local(1);
  turn_off_edge_labels();
  for (i = 0; i < n_irgs; ++i) {
    current_ir_graph = get_irp_irg(i);
    irg_vrfy(current_ir_graph);
    finalize_cons (current_ir_graph);

    /* output the vcg file */
    //dump_ir_block_graph (current_ir_graph, "-early");
    construct_backedges(current_ir_graph);
    //dump_ir_block_graph (current_ir_graph, 0);
    dump_all_types(0);
    set_opt_strength_red_verbose(2);
    set_firm_verbosity(2);
    reduce_strength(current_ir_graph);

    //dump_loop_tree(current_ir_graph, "");
    dump_ir_block_graph (current_ir_graph, "-strength_reduced");
  }
  //printf("use xvcg to view this graph:\n");
  //printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
