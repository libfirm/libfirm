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
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif

# include <stdio.h>

# include "ident_t.h"
# include "firm.h"
# include "mangle.h"
/* init functions are not public */
# include "tv_t.h"
# include "tpop_t.h"
# include "irprog_t.h"
# include "irnode_t.h"
# include "irmode_t.h"
# include "ircons_t.h"
# include "irgraph_t.h"
# include "type_t.h"
# include "type_identify.h"
# include "firmstat.h"
# include "irreflect_t.h"
# include "irarch.h"
# include "reassoc_t.h"
# include "irhooks.h"
# include "iredges_t.h"

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
  init_ident(def_params.id_if, 1024);
  /* initialize Firm hooks */
  init_hooks();
  /* enhanced statistics, need idents and hooks */
  init_stat(def_params.enable_statistics);
  /* Edges need hooks. */
  init_edges();
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
  firm_init_mangle();
  /* initalize all op codes an irnode can consist of */
  init_op();
  /* called once for each run of this library */
  init_cons(def_params.initialize_local_func);
  /* initialize reassociation */
  firm_init_reassociation();
  /* Builds a construct allowing to access all information to be constructed
     later. */
  init_irprog();
  /* Constructs some idents needed. */
  init_type();
  init_entity();
  /* allocate a hash table. */
  init_type_identify(def_params.ti_if);
  /* Init reflection facility. */
  firm_init_rflct();

  /* Init architecture dependent optimizations. */
  arch_dep_init(arch_dep_default_factory);
  arch_dep_set_opts(arch_dep_mul_to_shift | arch_dep_div_by_const | arch_dep_mod_by_const);

  firm_archops_init(def_params.arch_op_settings);
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
  finish_ident();
}
