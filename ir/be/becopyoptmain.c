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
#include "pmap.h"
#include "debug.h"
#include "irouts.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "becopyoptmain.h"
#include "phiclass.h"

#define DO_HEUR
#undef DO_ILP_5_SEC
#undef DO_ILP_30_SEC
#undef DO_ILP

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

void be_copy_opt_init(void) {
	dbg = firm_dbg_register("ir.be.copyoptmain");
	firm_dbg_set_mask(dbg, DEBUG_LVL);
}

typedef struct color_saver {
	arch_env_t *arch_env;
	be_chordal_env_t *chordal_env;
	pmap *saved_colors;
	int flag;  /* 0 save; 1 load */
} color_save_t;

static void save_load(ir_node *irn, void *env) {
	color_save_t *saver = env;
	if (saver->chordal_env->cls == arch_get_irn_reg_class(saver->arch_env, irn, arch_pos_make_out(0))) {
		if (saver->flag == 0) { /* save */
			const arch_register_t *reg = arch_get_irn_register(saver->arch_env, irn, 0);
			pmap_insert(saver->saved_colors, irn, (void *) reg);
		} else { /*load */
			arch_register_t *reg = pmap_get(saver->saved_colors, irn);
			arch_set_irn_register(saver->arch_env, irn, 0, reg);
		}
	}
}

static void save_colors(color_save_t *color_saver) {
	color_saver->flag = 0;
	irg_walk_graph(color_saver->chordal_env->session_env->irg, save_load, NULL, color_saver);
}

static void load_colors(color_save_t *color_saver) {
	color_saver->flag = 1;
	irg_walk_graph(color_saver->chordal_env->session_env->irg, save_load, NULL, color_saver);
}

void be_copy_opt(be_chordal_env_t *chordal_env) {
	copy_opt_t *co;
	int costs, costs_init=-1, costs_heur=-1, costs_ilp_5_sec=-1, costs_ilp_30_sec=-1, costs_ilp=-1;
	int lower_bound;
	color_save_t saver;
	saver.arch_env = chordal_env->session_env->main_env->arch_env;
	saver.chordal_env = chordal_env;
	saver.saved_colors = pmap_create();

	/* BETTER: You can remove this if you replace all
	 * `grep get_irn_out *.c` by the irouts.h module.*/
	compute_outs(chordal_env->session_env->irg);

	co = new_copy_opt(chordal_env, get_costs_loop_depth);
	DBG((dbg, LEVEL_1, "----> CO: %s\n", co->name));
	phi_class_compute(chordal_env->session_env->irg);

#ifdef DO_STAT
	lower_bound = co_get_lower_bound(co);
	DBG((dbg, LEVEL_1, "Lower Bound: %3d\n", lower_bound));
	DBG((dbg, LEVEL_1, "Inevit Costs: %3d\n", co_get_inevit_copy_costs(co)));

	costs = co_get_copy_costs(co);
	costs_init = costs;
	copystat_add_max_costs(co_get_max_copy_costs(co));
	copystat_add_inevit_costs(co_get_inevit_copy_costs(co));
	copystat_add_init_costs(costs_init);
	DBG((dbg, LEVEL_1, "Init costs: %3d\n", costs_init));
#endif

	save_colors(&saver);

#ifdef DO_HEUR
	lc_timer_t *timer = lc_timer_register("heur", NULL);
	lc_timer_reset_and_start(timer);
	co_heur_opt(co);
	lc_timer_stop(timer);
	copystat_add_heur_time(lc_timer_elapsed_msec(timer));
#ifdef DO_STAT
	costs = co_get_copy_costs(co);
	costs_heur = costs;
	copystat_add_heur_costs(costs_heur);
	DBG((dbg, LEVEL_1, "Heur costs: %3d\n", costs_heur));
#endif
	assert(lower_bound == -1 || costs_heur == -1 || lower_bound <= costs_heur);
#endif

	int was_optimal = 0;

#ifdef DO_ILP_5_SEC
	load_colors(&saver);
	was_optimal = co_ilp_opt(co, 5.0);
#ifdef DO_STAT
	costs = co_get_copy_costs(co);
	costs_ilp_5_sec = costs;
	copystat_add_ilp_5_sec_costs(costs_ilp_5_sec);
	DBG((dbg, LEVEL_1, "5_Sec costs: %3d\n", costs_ilp_5_sec));
#endif
#endif


#ifdef DO_ILP_30_SEC
	if (!was_optimal) {
		load_colors(&saver);
		co_ilp_opt(co, 30.0);
	}
#ifdef DO_STAT
	costs = co_get_copy_costs(co);
	costs_ilp_30_sec = costs;
	copystat_add_ilp_30_sec_costs(costs_ilp_30_sec);
	DBG((dbg, LEVEL_1, "30_Sec costs: %3d\n", costs_ilp_30_sec));
#endif
#endif


#ifdef DO_ILP
	load_colors(&saver);
	co_ilp_opt(co, 1000.0);
#ifdef DO_STAT
	costs = co_get_copy_costs(co);
	costs_ilp = costs;
	copystat_add_opt_costs(costs_ilp);
	DBG((dbg, LEVEL_1, "Opt  costs: %3d\n", costs_ilp));
#endif
	assert(lower_bound == -1 || costs_ilp == -1 || lower_bound <= costs_ilp);
	assert(costs_ilp == -1 || costs_heur == -1 || costs_ilp <= costs_heur);
#endif

	pmap_destroy(saver.saved_colors);
	free_copy_opt(co);
}
