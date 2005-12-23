/**
 * Author:      Daniel Grund
 * Date:		11.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.

 * Main file for the optimization reducing the copies needed for:
 * - Phi coalescing
 * - Register-constrained nodes
 * - Two-address code instructions
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <libcore/lc_timing.h>

#include "pmap.h"
#include "debug.h"
#include "irouts.h"
#include "bearch.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "becopyoptmain.h"
#include "phiclass.h"

#define DO_HEUR
#undef DO_CLASSES
#undef DO_ILP

static firm_dbg_module_t *dbg = NULL;


/**
 * Helpers for saving and restoring colors of nodes.
 * Used to get dependable and comparable benchmark results.
 */
#if (defined(DO_HEUR) && defined(DO_BETTER)) || (defined(DO_HEUR) && defined(DO_ILP)) || (defined(DO_BETTER) && defined(DO_ILP))

typedef struct color_saver {
	arch_env_t *arch_env;
	be_chordal_env_t *chordal_env;
	pmap *saved_colors;
	int flag; /* 0 save, 1 load */
} color_save_t;

static void save_load(ir_node *irn, void *env) {
	color_save_t *saver = env;
	if (saver->chordal_env->cls == arch_get_irn_reg_class(saver->arch_env, irn, -1)) {
		if (saver->flag == 0) { /* save */
			const arch_register_t *reg = arch_get_irn_register(saver->arch_env, irn);
			pmap_insert(saver->saved_colors, irn, (void *) reg);
		} else { /*load */
			arch_register_t *reg = pmap_get(saver->saved_colors, irn);
			arch_set_irn_register(saver->arch_env, irn, reg);
		}
	}
}

static void save_colors(color_save_t *color_saver) {
	color_saver->flag = 0;
	irg_walk_graph(color_saver->chordal_env->irg, save_load, NULL, color_saver);
}

static void load_colors(color_save_t *color_saver) {
	color_saver->flag = 1;
	irg_walk_graph(color_saver->chordal_env->irg, save_load, NULL, color_saver);
}

#endif /* Need save/load stuff */



void be_copy_opt_init(void) {
	dbg = firm_dbg_register("ir.be.copyoptmain");
}



void be_copy_opt(be_chordal_env_t *chordal_env) {
	copy_opt_t *co;

#ifdef DO_STAT
	lc_timer_t *timer;
	color_save_t saver;
	int costs, costs_inevit, costs_init, costs_heur, costs_classes, costs_ilp, lower_bound;
#endif


	co = new_copy_opt(chordal_env, get_costs_loop_depth);
	DBG((dbg, LEVEL_1, "----> CO: %s\n", co->name));
	phi_class_compute(chordal_env->irg);


#ifdef DO_STAT
#if (defined(DO_HEUR) && defined(DO_BETTER)) || (defined(DO_HEUR) && defined(DO_ILP)) || (defined(DO_BETTER) && defined(DO_ILP))
		saver.arch_env = chordal_env->main_env->arch_env;
		saver.chordal_env = chordal_env;
		saver.saved_colors = pmap_create();
		save_colors(&saver);
#endif

		costs_inevit = co_get_inevit_copy_costs(co);
		lower_bound  = co_get_lower_bound(co);
		costs_init   = co_get_copy_costs(co);

		DBG((dbg, LEVEL_1, "Inevit Costs: %3d\n", costs_inevit));
		DBG((dbg, LEVEL_1, "Lower Bound: %3d\n", lower_bound));
		DBG((dbg, LEVEL_1, "Init costs: %3d\n", costs_init));

		copystat_add_inevit_costs(costs_inevit);
		copystat_add_init_costs(costs_init);
		copystat_add_max_costs(co_get_max_copy_costs(co));
#endif


#ifdef DO_HEUR
#ifdef DO_STAT
	timer = lc_timer_register("heur", NULL);
	lc_timer_reset_and_start(timer);
#endif

	co_heur_opt(co);

#ifdef DO_STAT
	lc_timer_stop(timer);
	costs_heur = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "Heur costs: %3d\n", costs_heur));
	copystat_add_heur_time(lc_timer_elapsed_msec(timer));
	copystat_add_heur_costs(costs_heur);
	assert(lower_bound <= costs_heur);
#endif
#endif /* DO_HEUR */



#ifdef DO_CLASSES
#ifdef DO_STAT
#ifdef DO_HEUR
	load_colors(&saver);
#endif
	timer = lc_timer_register("classes", NULL);
	lc_timer_reset_and_start(timer);
#endif

	co_classes_opt(co);

#ifdef DO_STAT
	lc_timer_stop(timer);
	costs_classes = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "Classes costs: %3d\n", costs_classes));
	copystat_add_classes_time(lc_timer_elapsed_msec(timer));
	copystat_add_classes_costs(costs_heur);
	assert(lower_bound <= costs_classes);
#endif
#endif /* DO_CLASSES */



#ifdef DO_ILP
#ifdef DO_STAT
#if defined(DO_HEUR) || defined(DO_CLASSES)
	load_colors(&saver);
#endif
#endif

	co_ilp_opt(co, 60.0);

#ifdef DO_STAT
	costs_ilp = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "Opt  costs: %3d\n", costs_ilp));
	copystat_add_opt_costs(costs_ilp);
	assert(lower_bound <= costs_ilp);
#endif
#endif /* DO_ILP */


#ifdef DO_STAT
#if (defined(DO_HEUR) && defined(DO_BETTER)) || (defined(DO_HEUR) && defined(DO_ILP)) || (defined(DO_BETTER) && defined(DO_ILP))
	pmap_destroy(saver.saved_colors);
#endif
#endif
	free_copy_opt(co);
}
