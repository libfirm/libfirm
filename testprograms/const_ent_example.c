/*
 * Project:     libFIRM
 * File name:   testprograms/const_ent_example.c
 * Purpose:     Shows how to construct type information for constant entities.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# include <stdio.h>
# include <string.h>

# include "irvrfy.h"
# include "irdump.h"
# include "firm.h"

/**
 *  This file constructs type information for constant entities.
 *
 *  It constructs the information for a class type with a dispatch
 *  table.  The class has a field a, and two methods f and g.  The
 *  class is represented by a class type with two entities for the
 *  field a and the reference to the dispatch table.  This reference
 *  is a constant entity.  Ther dispatch table is also represented
 *  by a class type that contains the two methods.   There is one entity
 *  of the dispatch table which is constant.
 *
 *  Further the example shows the representation of a constant global
 *  array.
 *
 *  class C {
 *    int a;
 *    void f();
 *    void g(int);
 *  }
 *  int[4] arre = (7, 2, 13, 92);
 **/

int main(int argc, char **argv)
{
  ident *Ci, *ai, *fi, *fti, *gi, *gti, *inti, *dipti, *diptpi, *diptpei, *diptei;
      /* suffix i names identifiers */
  type  *Ct, *intt, *ft, *gt, *diptt, *diptpt;
      /*        t names types       */
  entity *ae, *fe, *ge, *dipte, *diptpe;   /*        e names entities    */
  ir_node *n;

  printf("\nExample program for constant entites.\n");
  printf("Creating type information...\n");

  /** init library */
  init_firm (NULL);

  /** make idents for all used identifiers in the program. */
  Ci  = new_id_from_chars("C",  strlen("C"));
  ai  = new_id_from_chars("a",  strlen("a"));
  fi  = new_id_from_chars("f",  strlen("f"));
  fti  = new_id_from_chars("f_type",  strlen("f_type"));
  gi  = new_id_from_chars("g",  strlen("g"));
  gti  = new_id_from_chars("g_type",  strlen("g_type"));
  inti = new_id_from_chars("int", strlen("int"));
  dipti = new_id_from_chars("C_dispatch_table_type", strlen("C_dispatch_table_type"));
  diptei = new_id_from_chars("C_dispatch_table", strlen("C_dispatch_table"));
  diptpi = new_id_from_chars("C_dispatch_table_p_type", strlen("C_dispatch_table_p_type"));
  diptpei = new_id_from_chars("C_dispatch_table_p", strlen("C_dispatch_table_p"));


  /** make the type information needed */
  /* Language defined types */
  intt = new_type_primitive(inti, mode_Is);
  /* Program defined types */
  Ct = new_type_class(Ci);
  ft = new_type_method(fti, 0, 0);  /* 0 parameters, 0 results */
  gt = new_type_method(gti, 1, 0);  /* 1 parameter, 0 results */
  /* Compiler defined types: dispatch table and pointer to it  */
  diptt = new_type_class(dipti);
  diptpt = new_type_pointer(diptpi, diptt);
  /** add structure to type graph **/
  /* parameters of methods */
  set_method_param_type(gt, 0, intt);

  /** make entities **/
  ae     = new_entity(Ct, ai, intt);
  fe     = new_entity(diptt, fi, ft);
  ge     = new_entity(diptt, gi, gt);
  dipte  = new_entity(get_glob_type(), diptei, diptt);
  diptpe = new_entity(Ct, diptpei, diptpt);

  /** Add constant entity information **/
  current_ir_graph = get_const_code_irg();
  /* The pointer to the dispatch table is constant. */
  /* The constant is the address of the given entity */
  symconst_symbol sym;
  sym.entity_p = dipte;
  n = new_SymConst(sym, symconst_addr_ent);
  set_entity_variability(diptpe, variability_constant);
  set_atomic_ent_value(diptpe, n);

  /* The entity representing the dispatch table is constant, too. */
  set_entity_variability(dipte, variability_constant);
  add_compound_ent_value(dipte, get_atomic_ent_value(fe), fe);
  add_compound_ent_value(dipte, get_atomic_ent_value(ge), ge);

{
  /*** Example with an array ***/
  ident *arrei, *arrti;
  type *arrt;
  entity *arre, *arrelte;

  arrei =  new_id_from_chars("arr", strlen("arr"));
  arrti =  new_id_from_chars("arr_t",  strlen("arr_t"));

  /** The array type **/
  /* Don't reuse int type so that graph layout is better readable */
  intt = new_type_primitive(inti, mode_Is);
  arrt = new_type_array(arrti, 1, intt);
  set_array_bounds_int(arrt, 0, 0, 4);
  arrelte = get_array_element_entity(arrt);

  /** The constant array entity **/
  arre = new_entity(get_glob_type(), arrei, arrt);
  set_entity_variability(arre, variability_constant);
  current_ir_graph = get_const_code_irg();
  n = new_Const(mode_Is, new_tarval_from_long (7, mode_Is));
  add_compound_ent_value(arre, n, arrelte);
  n = new_Const(mode_Is, new_tarval_from_long (2, mode_Is));
  add_compound_ent_value(arre, n, arrelte);
  n = new_Const(mode_Is, new_tarval_from_long (13, mode_Is));
  add_compound_ent_value(arre, n, arrelte);
  n = new_Const(mode_Is, new_tarval_from_long (92, mode_Is));
  add_compound_ent_value(arre, n, arrelte);
}
  printf("Done building the graph.  Dumping it.\n");
  dump_all_types(0);

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
