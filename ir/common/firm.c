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

void
init_firm (default_initialize_local_variable_func_t *func)
{
  /* initialize all ident stuff */
  id_init ();
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
