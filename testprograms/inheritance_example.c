/*
 * Project:     libFIRM
 * File name:   testprograms/inheritance_example.c
 * Purpose:     Shows type graph with inheritance.
 * Author:      Christian Schaefer, Goetz Lindenmaier
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
*  This file constructs type information for the following pseudo-program.
*  The procedure code is not constructed.
*
*  interface I {
*    void m1 (void);
*  }
*
*  class C implements I {
*    void m1 (void) {return};
*    void m2 (int)  {return 0};
*  }
*
*  class D {
*    int b;
*  }
*
*  class E extends C, D {
*    void m2 (int) {return 1};
*    int a;
*  }
*
**/

int main(int argc, char **argv)
{
  ident *ii, *ci, *di, *ei, *m1i, *m2i, *inti, *ai, *bi; /* suffix i names identifiers */
  type  *it, *ct, *dt, *et;                              /*        t names types       */
  type  *m1t, *m2t;
  type  *intt;
  entity *i_m1e, *c_m1e, *c_m2e, *e_m2e, *d_be, *e_ae;   /*        e names entities    */

  printf("\nCreating type information for INHERITANCE_EXAMPLE ...\n");

  /** init library */
  init_firm (NULL);

  /** make idents for all used identifiers in the program. */
  ii  = id_from_str("i",  strlen("i"));
  ci  = id_from_str("c",  strlen("c"));
  di  = id_from_str("d",  strlen("d"));
  ei  = id_from_str("e",  strlen("e"));
  m1i = id_from_str("m1", strlen("m1"));
  m2i = id_from_str("m2", strlen("m2"));
  inti= id_from_str("int",strlen("int"));
  ai  = id_from_str("a",  strlen("a"));
  bi  = id_from_str("b",  strlen("b"));

  /** make the type information needed */
  /* Language defined types */
  intt = new_type_primitive(inti, mode_Iu);
  /* Program defined types */
  it = new_type_class(ii);           /* The fact that this is an interface is
					of no interest.  It's just a class without
					fields and implementations.  But the
					implementation will never be needed. */
  ct = new_type_class(ci);
  dt = new_type_class(di);
  et = new_type_class(ei);
                                     /* Methods with the same type should use the same
					method type information! */
  m1t = new_type_method(m1i, 0, 0);  /* 0 parameters, 0 results */
  m2t = new_type_method(m2i, 1, 0);  /* 1 parameter, 0 results */

  /** add structure to type graph **/
  /* parameters of methods */
  set_method_param_type(m2t, 0, intt);
  /* inheritance. The other direction is added automatically. */
  add_class_subtype(it, ct);
  add_class_subtype(ct, et);
  add_class_subtype(dt, et);

  /** make entities **/
  i_m1e = new_entity(it, m1i, m1t);
  c_m1e = new_entity(ct, m1i, m1t);
  c_m2e = new_entity(ct, m2i, m2t);
  e_m2e = new_entity(et, m2i, m2t);
  d_be  = new_entity(dt, bi, intt);
  e_ae  = new_entity(et, ai, intt);

  /** Add overwirtes relation **/
  /* How these edges are added depends on the source language. */
  add_entity_overwrites (c_m1e, i_m1e);
  add_entity_overwrites (e_m2e, c_m2e);


  printf("Done building the graph.  Dumping it.\n");
  dump_all_types();

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
