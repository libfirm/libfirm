/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * Author:      Daniel Grund
 * Date:		28.02.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Common stuff used by all ILP fomulations.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "irtools.h"
#include "irprintf.h"

#include "bestatevent.h"
#include "beirg_t.h"
#include "bemodule.h"
#include "error.h"

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

#ifdef WITH_ILP

#define DUMP_ILP 1
#define DUMP_SOL 2

static int time_limit = 60;
static int solve_net  = 1;
static int solve_log  = 0;
static unsigned dump_flags = 0;

static const lc_opt_enum_mask_items_t dump_items[] = {
	{ "ilp",   DUMP_ILP },
	{ "sol",   DUMP_SOL },
	{ NULL,    0 }
};

static lc_opt_enum_mask_var_t dump_var = {
	&dump_flags, dump_items
};

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_INT      ("limit", "time limit for solving in seconds (0 for unlimited)", &time_limit),
	LC_OPT_ENT_BOOL     ("net",   "solve over the net", &solve_net),
	LC_OPT_ENT_BOOL     ("log",   "show ilp solving log",              &solve_log),
	LC_OPT_ENT_ENUM_MASK("dump",  "dump flags",             &dump_var),
	{ NULL }
};

void be_init_copyilp(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp = lc_opt_get_grp(ra_grp, "chordal");
	lc_opt_entry_t *co_grp = lc_opt_get_grp(chordal_grp, "co");
	lc_opt_entry_t *ilp_grp = lc_opt_get_grp(co_grp, "ilp");

	lc_opt_add_table(ilp_grp, options);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copyilp);

#include "becopyilp_t.h"
#include "beifg_t.h"

/******************************************************************************
    _____ _                        _            _   _
   / ____(_)                      | |          | | (_)
  | (___  _ _______   _ __ ___  __| |_   _  ___| |_ _  ___  _ __
   \___ \| |_  / _ \ | '__/ _ \/ _` | | | |/ __| __| |/ _ \| '_ \
   ____) | |/ /  __/ | | |  __/ (_| | |_| | (__| |_| | (_) | | | |
  |_____/|_/___\___| |_|  \___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

 *****************************************************************************/


size_red_t *new_size_red(copy_opt_t *co) {
	size_red_t *res = xmalloc(sizeof(*res));

	res->co = co;
	res->all_removed = pset_new_ptr_default();
	res->col_suff = NULL;
	obstack_init(&res->ob);

	return res;
}

/**
 * Checks if a node is simplicial in the graph heeding the already removed nodes.
 */
static INLINE int sr_is_simplicial(size_red_t *sr, const ir_node *ifn) {
	int i, o, size = 0;
	ir_node **all, *curr;
	be_ifg_t *ifg = sr->co->cenv->ifg;
	void *iter = be_ifg_neighbours_iter_alloca(ifg);

	all = alloca(be_ifg_degree(ifg, ifn) * sizeof(*all));

	/* get all non-removed neighbors */
	be_ifg_foreach_neighbour(ifg, iter, ifn, curr)
		if (!sr_is_removed(sr, curr))
			all[size++] = curr;
	be_ifg_neighbours_break(ifg, iter);

	/* check if these form a clique */
	for (i=0; i<size; ++i)
		for (o=i+1; o<size; ++o)
			if (!be_ifg_connected(ifg, all[i], all[o]))
				return 0;

	/* all edges exist so this is a clique */
	return 1;
}

void sr_remove(size_red_t *sr) {
	ir_node *irn;
	int redo = 1;
	const be_ifg_t *ifg = sr->co->cenv->ifg;
	void *iter = be_ifg_nodes_iter_alloca(ifg);

	while (redo) {
		redo = 0;
		be_ifg_foreach_node(ifg, iter, irn) {
			const arch_register_req_t *req;

			req = arch_get_register_req(sr->co->aenv, irn, -1);

			if (!arch_register_req_is(req, limited) && !sr_is_removed(sr, irn) && !co_gs_is_optimizable(sr->co, irn)) {
          	 	if (sr_is_simplicial(sr, irn)) {
					coloring_suffix_t *cs = obstack_alloc(&sr->ob, sizeof(*cs));

					cs->irn = irn;
					cs->next = sr->col_suff;
					sr->col_suff = cs;

					pset_insert_ptr(sr->all_removed, irn);

					redo = 1;
          	 	}
			}
		}
		be_ifg_nodes_break(ifg, iter);
	}
}

void sr_reinsert(size_red_t *sr) {
	coloring_suffix_t *cs;
	be_ifg_t *ifg        = sr->co->cenv->ifg;
	bitset_t *used_cols  = bitset_alloca(arch_register_class_n_regs(sr->co->cls));
	void *iter           = be_ifg_neighbours_iter_alloca(ifg);

	/* color the removed nodes in right order */
	for (cs = sr->col_suff; cs; cs = cs->next) {
		int free_col;
		ir_node *other, *irn;

		/* get free color by inspecting all neighbors */
		irn = cs->irn;
		bitset_clear_all(used_cols);

		be_ifg_foreach_neighbour(ifg, iter, irn, other) {
			if (!sr_is_removed(sr, other)) /* only inspect nodes which are in graph right now */
				bitset_set(used_cols, get_irn_col(sr->co, other));
		}
		be_ifg_neighbours_break(ifg, iter);

		/* now all bits not set are possible colors */
		free_col = bitset_next_clear(used_cols, 0);
		assert(free_col != -1 && "No free color found. This can not be.");
		set_irn_col(sr->co, irn, free_col);
		pset_remove_ptr(sr->all_removed, irn); /* irn is back in graph again */
	}
}

void free_size_red(size_red_t *sr) {
	del_pset(sr->all_removed);
	obstack_free(&sr->ob, NULL);
	free(sr);
}

/******************************************************************************
    _____                      _        _____ _      _____
   / ____|                    (_)      |_   _| |    |  __ \
  | |  __  ___ _ __   ___ _ __ _  ___    | | | |    | |__) |
  | | |_ |/ _ \ '_ \ / _ \ '__| |/ __|   | | | |    |  ___/
  | |__| |  __/ | | |  __/ |  | | (__   _| |_| |____| |
   \_____|\___|_| |_|\___|_|  |_|\___| |_____|______|_|

 *****************************************************************************/

#include <stdio.h>

ilp_env_t *new_ilp_env(copy_opt_t *co, ilp_callback build, ilp_callback apply, void *env) {
	ilp_env_t *res = xmalloc(sizeof(*res));
	assert(res);

	res->co         = co;
	res->build      = build;
	res->apply      = apply;
	res->env        = env;
	res->sr         = new_size_red(co);

	return res;
}

lpp_sol_state_t ilp_go(ilp_env_t *ienv) {
	be_main_env_t *main_env = ienv->co->cenv->birg->main_env;

	sr_remove(ienv->sr);

	ienv->build(ienv);
	lpp_set_time_limit(ienv->lp, time_limit);

	if(solve_log)
		lpp_set_log(ienv->lp, stdout);

	if(solve_net)
		lpp_solve_net(ienv->lp, main_env->options->ilp_server, main_env->options->ilp_solver);
	else {
#ifdef LPP_SOLVE_NET
		fprintf(stderr, "can only solve ilp over the net\n");
#else
		lpp_solve_cplex(ienv->lp);
#endif
	}

	be_stat_ev_dbl("co_ilp_objval",     ienv->lp->objval);
	be_stat_ev_dbl("co_ilp_best_bound", ienv->lp->best_bound);
	be_stat_ev    ("co_ilp_iter",       ienv->lp->iterations);
	be_stat_ev_dbl("co_ilp_sol_time",   ienv->lp->sol_time);

	if(dump_flags & DUMP_ILP) {
		char buf[128];
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "%F_%s-co.ilp", ienv->co->cenv->irg,
		            ienv->co->cenv->cls->name);
		f = fopen(buf, "wt");
		if(f == NULL) {
			panic("Couldn't open '%s' for writing", buf);
		}
		lpp_dump_plain(ienv->lp, f);
		fclose(f);
	}

	ienv->apply(ienv);

	sr_reinsert(ienv->sr);

	return lpp_get_sol_state(ienv->lp);
}

void free_ilp_env(ilp_env_t *ienv) {
	free_size_red(ienv->sr);
	free_lpp(ienv->lp);
	free(ienv);
}

#else /* WITH_ILP */

static INLINE void only_that_you_can_compile_without_WITH_ILP_defined(void) {
}

#endif /* WITH_ILP */
