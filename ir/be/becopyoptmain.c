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


#include <libcore/lc_timing.h>
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
	firm_dbg_set_mask(dbg, DEBUG_LVL);
}

void be_copy_opt(be_chordal_env_t *chordal_env) {
	copy_opt_t *co;
	int costs, costs_init=-1, costs_heur=-1, costs_ilp=-1;
	int lower_bound;

	/* BETTER: You can remove this if you replace all
	 * `grep get_irn_out *.c` by the irouts.h module.*/
	compute_outs(chordal_env->session_env->irg);

	co = new_copy_opt(chordal_env, get_costs_all_one);
	DBG((dbg, LEVEL_1, "----> CO: %s\n", co->name));


#ifdef DO_STAT
	lower_bound = co_get_lower_bound(co);
	DBG((dbg, LEVEL_1, "Lower Bound: %3d\n", lower_bound));
	costs = co_get_copy_costs(co);
	costs_init = costs;
	copystat_add_max_costs(co_get_max_copy_costs(co));
	copystat_add_inevit_costs(co_get_inevit_copy_costs(co));
	copystat_add_init_costs(costs_init);
	DBG((dbg, LEVEL_1, "Init costs: %3d\n", costs_init));
#endif

#ifdef DO_HEUR
	lc_timer_t *timer = lc_timer_register("heur", NULL);
	lc_timer_start(timer);
	co_heur_opt(co);
	lc_timer_stop(timer);
	copystat_add_heur_time(lc_timer_elapsed_msec(timer));
#ifdef DO_STAT
	costs = co_get_copy_costs(co);
	costs_heur = costs;
	copystat_add_heur_costs(costs_heur);
	DBG((dbg, LEVEL_1, "Heur costs: %3d\n", costs_heur));
#endif
#endif

#ifdef DO_ILP
	co_ilp_opt(co);
#ifdef DO_STAT
	costs = co_get_copy_costs(co);
	costs_ilp = costs;
	copystat_add_opt_costs(costs_ilp);
	DBG((dbg, LEVEL_1, "Opt  costs: %3d\n", costs_ilp));
#endif
#endif
	assert(lower_bound <= costs_ilp);
	assert(costs_ilp <= costs_heur);

	free_copy_opt(co);
}
