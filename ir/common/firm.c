/*
 * Project:     libFIRM
 * File name:   ir/common/firm.c
 * Purpose:     Central firm functionality.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <stdio.h>
# include "ident_t.h"
# include "firm.h"
# include "mangle.h"
# include "tv_t.h"
/* init functions are not public */
# include "tpop_t.h"
# include "irnode_t.h"
# include "irmode_t.h"
# include "irgraph_t.h"
# include "type_t.h"
# include "firmstat.h"

void
init_firm (default_initialize_local_variable_func_t *func)
{
  /* enhanced statistics */
  stat_init();
  /* initialize all ident stuff */
  id_init (1024);
  /* create the type kinds. */
  init_tpop ();
  /* create an obstack and put all tarvals in a pdeq */
  init_tarval_1 ();
  /* initialize all modes an ir node can consist of */
  init_mode ();
  /* initialize tarvals, and floating point arithmetic */
  init_tarval_2 ();
  /* init graph construction */
  init_irgraph();
  /* kind of obstack initialization */
  init_mangle ();
  /* initalize all op codes an irnode can consist of */
  init_op ();
  /* called once for each run of this library */
  init_cons (func);
  /* Builds a construct allowing to access all information to be constructed
     later. */
  init_irprog ();
  /* Constructs some idents needed. */
  init_type();
}


void free_firm (void) {
  int i;

  for (i = 0; i < get_irp_n_irgs(); i++)
    free_ir_graph(get_irp_irg(i));

  for (i = 0; i < get_irp_n_types(); i++) {
    free_type_entities(get_irp_type(i));
    free_type(get_irp_type(i));
  }

  free_type_entities(get_glob_type());
  free_ir_prog();

  finish_tarval();
  finish_op();
  finish_mode();
  finish_tpop();
  id_finish();
}
