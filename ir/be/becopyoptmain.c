/**
 * Author:      Daniel Grund
 * Date:		11.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.

 * Main file for the optimization reducing the copies needed for:
 * - phi coalescing
 * - register-constrained nodes
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "debug.h"
#include "irouts.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "becopyoptmain.h"

#define DO_HEUR
#define DO_ILP

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

void be_copy_opt_init(void) {
	dbg = firm_dbg_register("ir.be.copyoptmain");
	firm_dbg_set_mask(dbg, LEVEL_1);
}

void be_copy_opt(be_chordal_env_t *chordal_env) {
	copy_opt_t *co;
	int lb, copy_costs;

	/* BETTER: You can remove this if you replace all `grep get_irn_out *.c`*/
	compute_outs(chordal_env->irg);

	co = new_copy_opt(chordal_env, get_costs_loop_depth);
	DBG((dbg, LEVEL_1, "===>  %s  <===\n", co->name));

#ifdef DO_STAT
	lb = co_get_lower_bound(co);
	copy_costs = co_get_copy_costs(co);
	curr_vals[I_COPIES_INIT] += copy_costs;
	DBG((dbg, LEVEL_1, "Init costs: %3d / %3d\n", lb, copy_costs));
#endif

#ifdef DO_HEUR
	co_heur_opt(co);
#ifdef DO_STAT
	copy_costs = co_get_copy_costs(co);
	curr_vals[I_COPIES_HEUR] += copy_costs;
	DBG((dbg, LEVEL_1, "Heur costs: %3d / %3d\n", lb, copy_costs));
#endif
#endif

#ifdef DO_ILP
	copy_costs = co_get_copy_costs(co);
	assert(copy_costs>=lb && "At least one computation of these two is boooogy");
	if (copy_costs > lb) {
		co_ilp_opt(co);
		be_ra_chordal_check(chordal_env);
	}

#ifdef DO_STAT
	copy_costs = co_get_copy_costs(co);
	assert(copy_costs>=lb && "At least one computation of these two is boooogy");
	curr_vals[I_COPIES_OPT] += copy_costs;
	DBG((dbg, LEVEL_1, "Opt  costs: %3d / %3d\n", copy_costs));
#endif
#endif

	free_copy_opt(co);
}
