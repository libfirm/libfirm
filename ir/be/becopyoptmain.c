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
#include "becopyopt.h"
#include "becopystat.h"
#include "becopyoptmain.h"

#define DO_HEUR
#undef DO_ILP

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

void be_copy_opt_init(void) {
	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, LEVEL_1);
}

void be_copy_opt(be_chordal_env_t *chordal_env) {
	copy_opt_t *co;
	int lb, copies;

	co = new_copy_opt(chordal_env);
	DBG((dbg, LEVEL_1, "===>  %s  <===\n", co->name));
	co_check_allocation(co);

#ifdef DO_STAT
	copies = co_get_copy_count(co);
	curr_vals[I_COPIES_INIT] += copies;
	DBG((dbg, LEVEL_1, "Init copies: %3d\n", copies));
#endif

#ifdef DO_HEUR
	co_heur_opt(co);
	co_check_allocation(co);
#ifdef DO_STAT
	copies = co_get_copy_count(co);
	curr_vals[I_COPIES_HEUR] += copies;
	DBG((dbg, LEVEL_1, "Heur copies: %3d\n", copies));
#endif
#endif

#ifdef DO_ILP
	lb = co_get_lower_bound(co);
	copies = co_get_copy_count(co);
	assert(copies>=lb && "At least one computation of these two is boooogy");

	if (copies > lb) {
		co_ilp_opt(co);
		co_check_allocation(co);
	}

#ifdef DO_STAT
	copies = co_get_copy_count(co);
	curr_vals[I_COPIES_OPT] += copies;
	DBG((dbg, LEVEL_1, "Opt  copies: %3d\n", copies));
	assert(copies>=lb && "At least one computation of these two is boooogy");
#endif
#endif

	free_copy_opt(co);
}
