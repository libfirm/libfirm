/*
 * Project:     libFIRM
 * File name:   testprograms/inheritance_example.c
 * Purpose:     Shows ir_type graph with inheritance.
 * Author:      Christian Schaefer, Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <stdio.h>
#include <string.h>

#include <libfirm/firm.h>

/**
*  This file constructs ir_type information for the following pseudo-program.
*  The procedure code is not constructed.
*
*  interface I {
*    void m1(void);
*  }
*
*  class C implements I {
*    void m1(void) {return};
*    void m2(int)  {return 0};
*  }
*
*  class D {
*    int b;
*  }
*
*  class E extends C, D {
*    void m2(int) {return 1};
*    int a;
*  }
*
**/

int main(void)
{
  ident *ii, *ci, *di, *ei, *m1i, *m2i, *inti, *ai, *bi; /* suffix i names identifiers */
  ir_type  *it, *ct, *dt, *et;                              /*        t names types       */
  ir_type  *m1t, *m2t;
  ir_type  *intt;
  ir_entity *i_m1e, *c_m1e, *c_m2e, *e_m2e, *d_be, *e_ae;   /*        e names entities    */

  printf("\nCreating ir_type information for INHERITANCE_EXAMPLE ...\n");

  /** init library */
  init_firm(NULL);

  /** make idents for all used identifiers in the program. */
  ii  = new_id_from_chars("i",  strlen("i"));
  ci  = new_id_from_chars("c",  strlen("c"));
  di  = new_id_from_chars("d",  strlen("d"));
  ei  = new_id_from_chars("e",  strlen("e"));
  m1i = new_id_from_chars("m1", strlen("m1"));
  m2i = new_id_from_chars("m2", strlen("m2"));
  inti= new_id_from_chars("int",strlen("int"));
  ai  = new_id_from_chars("a",  strlen("a"));
  bi  = new_id_from_chars("b",  strlen("b"));

  /** make the ir_type information needed */
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
                                     /* Methods with the same ir_type should use the same
					method ir_type information! */
  m1t = new_type_method(m1i, 0, 0);  /* 0 parameters, 0 results */
  m2t = new_type_method(m2i, 1, 0);  /* 1 parameter, 0 results */

  /** add structure to ir_type graph **/
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
  add_entity_overwrites(c_m1e, i_m1e);
  add_entity_overwrites(e_m2e, c_m2e);


  printf("Done building the graph.  Dumping it.\n");
  dump_all_types(0);

  printf("Use ycomp to view this graph:\n");
  printf("ycomp GRAPHNAME\n\n");

  return 0;
}
