/*
 * Project:     libFIRM
 * File name:   testprograms/indentify_types.c
 * Purpose:     Shows use of type identification
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
# include "type_identify.h"



int main(int argc, char **argv)
{
  ident *i1, *i2;
  type  *t1, *t2, *t3;

  printf("\nCreating type information for IDENTIFY_TYPES ...\n");




  /** init library */
  firm_parameter_t params;
  memset (&params, 0, sizeof(params));
  params.size = sizeof(params);
  params.compare_types_func = compare_names;
  init_firm(&params);


  i1 = new_id_from_str("type1");
  i2 = new_id_from_str("type2");

  t1 = new_type_class(i1);
  t1 = mature_type(t1);

  t1 = mature_type(t1);

  t2 = new_type_class(i1);
  t2 = mature_type(t2);

  t3 = new_type_class(i2);
  t3 = mature_type(t3);

  /*
  printf(" t1: "); DDMT(t1);
  printf(" t2: "); DDMT(t2);
  printf(" t3: "); DDMT(t3);
  */

  printf("Done building the graph.  Dumping it.\n");
  dump_all_types();

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
