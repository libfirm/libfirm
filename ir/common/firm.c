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
/* init functions are not public */
# include "tv_t.h"
# include "tpop_t.h"
# include "irnode_t.h"
# include "irmode_t.h"
# include "irgraph_t.h"
# include "type_t.h"
# include "type_identify.h"
# include "firmstat.h"

void
init_firm(const firm_parameter_t *param)
{
  firm_parameter_t def_params;
  unsigned int     size;

  memset(&def_params, 0, sizeof(def_params));

  if (param) {
    /* check for reasonale size */
    assert(param->size <= sizeof(def_params) && (param->size & 3) == 0 &&
	   "parameter struct not initialized ???");
    size = sizeof(def_params);
    if (param->size < size)
      size = param->size;

    memcpy(&def_params, param, size);
  }

  /* initialize all ident stuff */
  id_init(1024);
  /* enhanced statistics, need idents */
  stat_init(def_params.enable_statistics);
  /* create the type kinds. */
  init_tpop();
  /* create an obstack and put all tarvals in a pdeq */
  init_tarval_1();
  /* initialize all modes an ir node can consist of */
  init_mode();
  /* initialize tarvals, and floating point arithmetic */
  init_tarval_2();
  /* init graph construction */
  init_irgraph();
  /* kind of obstack initialization */
  init_mangle();
  /* initalize all op codes an irnode can consist of */
  init_op();
  /* called once for each run of this library */
  init_cons(def_params.initialize_local_func);
  /* Builds a construct allowing to access all information to be constructed
     later. */
  init_irprog();
  /* Constructs some idents needed. */
  init_type();
  /* allocate a hash table. */
  init_type_identify(def_params.compare_types_func, def_params.hash_types_func);
}


void free_firm(void) {
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
