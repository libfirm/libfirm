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
***  This file constructs type information for the following pseudo-program:
***
***  interface I {
***    void m1 (void);
***  }
***
***  class C implements I {
***    void m1 (void) {return};
***    void m2 (int)  {return 0};
***  }
***
***  class D {
***    int a;
***  }
***
***  class E extends C, D {
***    void m2 (int) {return 1};
***    int a;
***  }
***
**/

int main(int argc, char **argv)
{
  ident *ii, *ci, *di, *ei, *m1i, *m2i, *inti, *ai; /* suffix i names identifiers */
  type_class  *it, *ct, *dt, *et;                   /*        t names types       */
  type_method *m1t, *m2t;
  type_primitive *intt;
  entity *c_m1e, *c_m2e, *e_m2e, *d_ae, *e_ae;        /*        e names entities    */

  ir_node *x;

  printf("\nCreating type information...\n");

  /** init library */
  init_firm ();

  /** make idents for all used identifiers in the program. */
  ii  = id_from_str("i",  strlen("i"));
  ci  = id_from_str("c",  strlen("c"));
  di  = id_from_str("d",  strlen("d"));
  ei  = id_from_str("e",  strlen("e"));
  m1i = id_from_str("m1", strlen("m1"));
  m2i = id_from_str("m2", strlen("m2"));
  inti= id_from_str("int",strlen("int"));
  ai  = id_from_str("a",  strlen("a"));

  /** make the type information needed */
  /* Language defined types */
  intt = new_type_primitive(inti, mode_I);
  /* Program defined types */
  it = new_type_class(ii);
  ct = new_type_class(ci);
  dt = new_type_class(di);
  et = new_type_class(ei);
  m1t = new_type_method(m1i, 0, 0);  /* 0 parameters, 0 results */
  m2t = new_type_method(m2i, 1, 0);  /* 1 parameter, 0 results */

  /** add structure to type graph **/
  /* parameters of methods */
  set_method_param_type(m2t, 0, (type *)intt);
  /* inheritance */
  add_class_subtype(it, ct);
  add_class_subtype(ct, et);
  add_class_subtype(dt, et);
  add_class_supertype(ct, it);
  add_class_supertype(et, ct);
  add_class_supertype(et, dt);

  /** make entities **/
  c_m1e = new_entity((type *)ct, m1i, (type *)m1t);
  c_m2e = new_entity((type *)ct, m2i, (type *)m2t);
  e_m2e = new_entity((type *)et, m2i, (type *)m2t);
  d_ae = new_entity((type *)dt, ai, (type *)intt);
  e_ae = new_entity((type *)et, ai, (type *)intt);

  printf("Done building the graph.  Dumping it.\n");
  dump_all_types();

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
