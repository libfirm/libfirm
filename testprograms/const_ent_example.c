/* Copyright (C) 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Goetz Lindenmaier
**
** testprogram.
*/

#include <stdio.h>

# include "irdump.h"
# include "firm.h"

/**
***  This file constructs type information for constant entities.
***
***  It constructs the information for a class type with a dispatch
***  table.  The class has a field a, and two methods f and g.  The
***  class is represented by a class type with two entities for the
***  field a and the reference to the dispatch table.  This reference
***  is a constant entity.  Ther dispatch table is also represented
***  by a class type that contains the two methods.   There is one entity
***  of the dispatch table which is constant.
***
***  Further the example shows the representation of a constant global
***  array.
***
***  class C {
***    int a;
***    void f();
***    void g(int);
***  }
***  int[4] arre = (7, 2, 13, 92);
**/

int main(int argc, char **argv)
{
  ident *Ci, *ai, *fi, *fti, *gi, *gti, *inti, *dipti, *diptpi, *diptpei, *diptei;
      /* suffix i names identifiers */
  type  *Ct, *intt, *at, *ft, *gt, *diptt, *diptpt;
      /*        t names types       */
  entity *ae, *fe, *ge, *dipte, *diptpe;   /*        e names entities    */
  ir_node *n;

  printf("\nCreating type information...\n");

  /** init library */
  init_firm ();

  /** make idents for all used identifiers in the program. */
  Ci  = id_from_str("C",  strlen("C"));
  ai  = id_from_str("a",  strlen("a"));
  fi  = id_from_str("f",  strlen("f"));
  fti  = id_from_str("f_type",  strlen("f_type"));
  gi  = id_from_str("g",  strlen("g"));
  gti  = id_from_str("g_type",  strlen("g_type"));
  inti = id_from_str("int", strlen("int"));
  dipti = id_from_str("C_dispatch_table_type", strlen("C_dispatch_table_type"));
  diptei = id_from_str("C_dispatch_table", strlen("C_dispatch_table"));
  diptpi = id_from_str("C_dispatch_table_p_type", strlen("C_dispatch_table_p_type"));
  diptpei = id_from_str("C_dispatch_table_p", strlen("C_dispatch_table_p"));


  /** make the type information needed */
  /* Language defined types */
  intt = new_type_primitive(inti, mode_i);
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
  n = new_Const(mode_p, tarval_p_from_entity(dipte));
  set_entity_variability(diptpe, constant);
  set_atomic_ent_value(diptpe, n);

  /* The entity representing the dispatch table is constant, too. */
  set_entity_variability(dipte, constant);
  add_compound_ent_value(dipte, get_atomic_ent_value(fe), fe);
  add_compound_ent_value(dipte, get_atomic_ent_value(ge), ge);

{
  /*** Example with an array ***/
  ident *arrei, *arrti;
  type *arrt;
  entity *arre, *arrelte;

  arrei =  id_from_str("arr", strlen("arr"));
  arrti =  id_from_str("arr_t",  strlen("arr_t"));

  /** The array type **/
  /* Don't reuse int type so that graph layout is better readable */
  intt = new_type_primitive(inti, mode_i);
  arrt = new_type_array(arrti, 1, intt);
  set_array_bounds_int(arrt, 0, 0, 4);
  arrelte = get_array_element_entity(arrt);

  /** The constant array entity **/
  arre = new_entity(get_glob_type(), arrei, arrt);
  set_entity_variability(arre, constant);
  current_ir_graph = get_const_code_irg();
  n = new_Const(mode_i, tarval_from_long (mode_i, 7));
  add_compound_ent_value(arre, n, arrelte);
  n = new_Const(mode_i, tarval_from_long (mode_i, 2));
  add_compound_ent_value(arre, n, arrelte);
  n = new_Const(mode_i, tarval_from_long (mode_i, 13));
  add_compound_ent_value(arre, n, arrelte);
  n = new_Const(mode_i, tarval_from_long (mode_i, 92));
  add_compound_ent_value(arre, n, arrelte);
}
  printf("Done building the graph.  Dumping it.\n");
  dump_all_types();

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
