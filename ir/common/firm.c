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
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif

#ifdef WITH_LIBCORE
# include <libcore/lc_opts.h>
#endif

#include "ident_t.h"
#include "firm.h"
#include "irflag_t.h"
#include "mangle.h"
/* init functions are not public */
#include "tv_t.h"
#include "tpop_t.h"
#include "irprog_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "type_t.h"
#include "entity_t.h"
#include "type_identify.h"
#include "firmstat.h"
#include "irreflect_t.h"
#include "irarch.h"
#include "reassoc_t.h"
#include "irhooks.h"
#include "iredges_t.h"
#include "debugger.h"

#ifdef WITH_LIBCORE
/* returns the firm root */
lc_opt_entry_t *firm_opt_get_root(void) {
	static lc_opt_entry_t *grp = NULL;
	if(!grp)
		grp = lc_opt_get_grp(lc_opt_root_grp(), "firm");
	return grp;
}

void firm_init_options(const char *arg_prefix, int argc, const char **argv) {
	/* parse any init files for firm */
	lc_opts_init("firm", firm_opt_get_root(), arg_prefix, argc, argv);
}
#endif /* WITH_LIBCORE */

void init_firm(const firm_parameter_t *param)
{
	firm_parameter_t def_params;
	unsigned int     size;

	memset(&def_params, 0, sizeof(def_params));

	if (param) {
		/* check for reasonable size */
		assert(param->size <= sizeof(def_params) && (param->size & 3) == 0 &&
				"parameter struct not initialized ???");
		size = sizeof(def_params);
		if (param->size < size)
			size = param->size;

		memcpy(&def_params, param, size);
	}

	/* initialize firm flags */
	firm_init_flags();
	/* initialize all ident stuff */
	init_ident(def_params.id_if, 1024);
	/* initialize Firm hooks */
	firm_init_hooks();
	/* enhanced statistics, need idents and hooks */
	firm_init_stat(def_params.enable_statistics);
	/* Edges need hooks. */
	init_edges();
	/* create the type kinds. */
	init_tpop();
	/* create an obstack and put all tarvals in a pdeq */
	init_tarval_1(0l);
	/* Builds a basic program representation, so modes can be added. */
	init_irprog_1();
	/* initialize all modes an ir node can consist of */
	init_mode();
	/* initialize tarvals, and floating point arithmetic */
	init_tarval_2();
	/* init graph construction */
	firm_init_irgraph();
	/* kind of obstack initialization */
	firm_init_mangle();
	/* initialize all op codes an irnode can consist of */
	init_op();
	/* called once for each run of this library */
	init_cons(def_params.initialize_local_func);
	/* initialize reassociation */
	firm_init_reassociation();
	/* Builds a construct allowing to access all information to be constructed
	   later. */
	init_irprog_2();
	/* Initialize the type module ancd cr	d construct some idents needed. */
	firm_init_type(def_params.builtin_dbg, def_params.cc_mask);
	/* initialize the entity module */
	firm_init_entity();
	/* allocate a hash table. */
	init_type_identify(def_params.ti_if);
	/* Init reflection facility. */
	firm_init_rflct();

	/* Init architecture dependent optimizations. */
	arch_dep_init(arch_dep_default_factory);
	arch_dep_set_opts(arch_dep_mul_to_shift | arch_dep_div_by_const | arch_dep_mod_by_const);

	firm_archops_init(def_params.arch_op_settings);

#ifdef DEBUG_libfirm
	/* integrated debugger extension */
	firm_init_debugger();
#endif
}

void free_firm(void) {
	int i;

	for (i = get_irp_n_irgs() - 1; i >= 0; --i)
		free_ir_graph(get_irp_irg(i));

	free_type_entities(get_glob_type());
	for (i = get_irp_n_types() - 1; i >= 0; --i)
		free_type_entities(get_irp_type(i));

	for (i = get_irp_n_types() - 1; i >= 0; --i)
		free_type(get_irp_type(i));

	finish_op();
	free_ir_prog();

	finish_tarval();
	finish_mode();
	finish_tpop();
	finish_ident();
}

/* Returns the libFirm version number. */
void firm_get_version(firm_version_t *version) {
	version->major = libFirm_VERSION_MAJOR;
	version->minor = libFirm_VERSION_MINOR;
}
