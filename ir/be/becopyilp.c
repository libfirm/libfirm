/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Common stuff used by all ILP formulations.
 * @author      Daniel Grund
 * @date        28.02.2006
 */
#include <stdbool.h>

#include "be_t.h"
#include "belive.h"
#include "beirg.h"
#include "irtools.h"
#include "irprintf.h"

#include "statev_t.h"
#include "bemodule.h"
#include "panic.h"

#include "lpp.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

#define DUMP_ILP 1

static int  time_limit = 60;
static bool solve_log  = false;
static unsigned dump_flags = 0;

static const lc_opt_enum_mask_items_t dump_items[] = {
	{ "ilp", DUMP_ILP },
	{ NULL, 0 }
};

static lc_opt_enum_mask_var_t dump_var = {
	&dump_flags, dump_items
};

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_INT      ("limit", "time limit for solving in seconds (0 for unlimited)", &time_limit),
	LC_OPT_ENT_BOOL     ("log",   "show ilp solving log", &solve_log),
	LC_OPT_ENT_ENUM_MASK("dump",  "dump flags", &dump_var),
	LC_OPT_LAST
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copyilp)
void be_init_copyilp(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp = lc_opt_get_grp(ra_grp, "chordal");
	lc_opt_entry_t *co_grp = lc_opt_get_grp(chordal_grp, "co");
	lc_opt_entry_t *ilp_grp = lc_opt_get_grp(co_grp, "ilp");

	lc_opt_add_table(ilp_grp, options);
}

#include "becopyilp_t.h"
#include "beifg.h"

/******************************************************************************
    _____ _                        _            _   _
   / ____(_)                      | |          | | (_)
  | (___  _ _______   _ __ ___  __| |_   _  ___| |_ _  ___  _ __
   \___ \| |_  / _ \ | '__/ _ \/ _` | | | |/ __| __| |/ _ \| '_ \
   ____) | |/ /  __/ | | |  __/ (_| | |_| | (__| |_| | (_) | | | |
  |_____/|_/___\___| |_|  \___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

 *****************************************************************************/


/**
 * Checks if a node is simplicial in the graph heeding the already removed nodes.
 */
static inline bool sr_is_simplicial(ilp_env_t *const ienv, ir_node const *const ifn)
{
	bool              res = true;
	ir_node         **all = NEW_ARR_F(ir_node*, 0);
	neighbours_iter_t iter;
	be_ifg_foreach_neighbour(ienv->co->cenv->ifg, &iter, ifn, curr) {
		/* Only consider non-removed neighbours. */
		if (sr_is_removed(ienv, curr))
			continue;

		/* Check whether the current node forms a clique with all previous nodes. */
		for (size_t i = ARR_LEN(all); i-- != 0;) {
			if (!be_values_interfere(curr, all[i])) {
				res = false;
				goto end;
			}
		}

		ARR_APP1(ir_node*, all, curr);
	}

end:
	DEL_ARR_F(all);
	return res;
}

/**
 * Virtually remove all nodes not related to the problem
 * (simplicial AND not adjacent to a equal-color-edge)
 */
static void sr_remove(ilp_env_t *const ienv)
{
	bool redo = true;
	const be_ifg_t *ifg = ienv->co->cenv->ifg;

	while (redo) {
		redo = false;
		be_ifg_foreach_node(ifg, irn) {
			const arch_register_req_t *req = arch_get_irn_register_req(irn);
			if (arch_register_req_is(req, limited) || sr_is_removed(ienv, irn))
				continue;
			if (co_gs_is_optimizable(ienv->co, irn))
				continue;
			if (!sr_is_simplicial(ienv, irn))
				continue;

			ARR_APP1(ir_node*, ienv->col_suff, irn);
			ir_nodeset_insert(&ienv->all_removed, irn);

			redo = true;
		}
	}
}

/**
 * Virtually reinsert the nodes removed before and color them
 */
static void sr_reinsert(ilp_env_t *const ienv)
{
	/* color the removed nodes in right order */
	unsigned        const n_regs           = arch_register_class_n_regs(ienv->co->cls);
	unsigned       *const possible_cols    = rbitset_alloca(n_regs);
	unsigned const *const allocatable_cols = ienv->co->cenv->allocatable_regs->data;
	be_ifg_t const *const ifg              = ienv->co->cenv->ifg;
	for (size_t i = ARR_LEN(ienv->col_suff); i-- != 0;) {
		ir_node *const irn = ienv->col_suff[i];

		rbitset_copy(possible_cols, allocatable_cols, n_regs);

		/* get free color by inspecting all neighbors */
		neighbours_iter_t iter;
		be_ifg_foreach_neighbour(ifg, &iter, irn, other) {
			const arch_register_req_t *cur_req;
			unsigned cur_col;

			/* only inspect nodes which are in graph right now */
			if (sr_is_removed(ienv, other))
				continue;

			cur_req = arch_get_irn_register_req(other);
			cur_col = get_irn_col(other);

			/* Invalidate all single size register when it is a large one */
			do  {
				rbitset_clear(possible_cols, cur_col);
				++cur_col;
			} while ((cur_col % cur_req->width) != 0);
		}

		/* now all bits not set are possible colors */
		/* take one that matches the alignment constraint */
		unsigned free_col = 0;
		assert(!rbitset_is_empty(possible_cols, n_regs) && "No free color found. This can not be.");
		for (;;) {
			free_col = (unsigned)rbitset_next(possible_cols, free_col, true);
			if (free_col % arch_get_irn_register_req(irn)->width == 0)
				break;
			++free_col;
			assert(free_col < n_regs);
		}
		set_irn_col(ienv->co->cls, irn, free_col);
		ir_nodeset_remove(&ienv->all_removed, irn); /* irn is back in graph again */
	}
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

ilp_env_t *new_ilp_env(copy_opt_t *co, ilp_callback build, ilp_callback apply, void *env)
{
	ilp_env_t *res = XMALLOC(ilp_env_t);

	res->co         = co;
	res->build      = build;
	res->apply      = apply;
	res->env        = env;
	res->col_suff   = NEW_ARR_F(ir_node*, 0);
	ir_nodeset_init(&res->all_removed);

	return res;
}

lpp_sol_state_t ilp_go(ilp_env_t *ienv)
{
	ir_graph *irg = ienv->co->irg;

	sr_remove(ienv);

	ienv->build(ienv);

	if (dump_flags & DUMP_ILP) {
		char buf[128];
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "%F_%s-co.ilp", irg,
		            ienv->co->cenv->cls->name);
		f = fopen(buf, "wt");
		if (f == NULL) {
			panic("Couldn't open '%s' for writing", buf);
		}
		lpp_dump_plain(ienv->lp, f);
		fclose(f);
	}

	lpp_set_time_limit(ienv->lp, time_limit);
	if (solve_log)
		lpp_set_log(ienv->lp, stdout);

	lpp_solve(ienv->lp, be_options.ilp_server, be_options.ilp_solver);

	//stat_ev_dbl("co_ilp_objval",     ienv->lp->objval);
	//stat_ev_dbl("co_ilp_best_bound", ienv->lp->best_bound);
	stat_ev_int("co_ilp_iter",       lpp_get_iter_cnt(ienv->lp));
	stat_ev_dbl("co_ilp_sol_time",   lpp_get_sol_time(ienv->lp));

	ienv->apply(ienv);

	sr_reinsert(ienv);

	return lpp_get_sol_state(ienv->lp);
}

void free_ilp_env(ilp_env_t *ienv)
{
	ir_nodeset_destroy(&ienv->all_removed);
	DEL_ARR_F(ienv->col_suff);
	lpp_free(ienv->lp);
	free(ienv);
}
